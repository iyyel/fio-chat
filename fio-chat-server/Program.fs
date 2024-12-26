open System
open System.Globalization
open System.Collections.Generic

open FIO.Core
open FIO.Library.Network.WebSockets

type ChatMessage =
    // Connection
    | ConnectionRequest of RequestedUsername: string
    | ConnectionAcceptedResponse of ServerName: string * AcceptedUsername: string * Message: string
    | ConnectionFailedResponse of ServerName: string * FailedUsername: string * Message: string
    | NewConnectionResponse of ServerName: string * NewUsername: string * Message: string

    // Broadcast
    | BroadcastMessageRequest of FromUsername: string * Message: string
    | BroadcastMessageResponse of ServerName: string * FromUsername: string * Message: string

    // Private
    | PrivateMessageRequest of FromUsername: string * ToUsername: string * Message: string
    | PrivateMessageResponse of ServerName: string * FromUsername: string * ToUsername: string * Message: string
    | PrivateMessageFailedResponse of ServerName: string * FromUsername: string * ToUsername: string * Message: string

    // Online clients
    | OnlineClientsRequest of FromUsername: string
    | OnlineClientsResponse of ServerName: string * ToUsername: string * Clients: string list

    // Help
    | HelpRequest of FromUsername: string
    | HelpResponse of ServerName: string * ToUsername: string * Message: string

    // Kick
    | KickRequest of ServerName: string * ToUsername: string

and Server =
    { Name: string
      EndPoint: string
      Socket: ServerWebSocket<ChatMessage> }

and FIOChatServerApp(serverUrl, serverName) =
    inherit FIOApp<unit, exn>()

    let clients = Dictionary<string, WebSocket<ChatMessage>>()

    let formattedDate () =
        DateTime.Now.ToString("dd.MM.yy HH:mm:ss", CultureInfo.InvariantCulture)

    let printMessage username endpoint message =
        !+ printfn($"[%s{formattedDate()}] [%s{username} (%s{endpoint})]: %s{message}")

    let server serverUrl =
        let broadcastMessage message filteredUsername =
            match filteredUsername with
            | Some filteredUsername -> 
                clients
                |> Seq.filter (fun pair -> pair.Key <> filteredUsername)
                |> Seq.map _.Value.Send(message)
                |> Seq.fold (fun acc effect -> acc <!> effect) (!+  ())
            | None ->
                clients
                |> Seq.map _.Value.Send(message)
                |> Seq.fold (fun acc effect -> acc <!> effect) (!+ ())

        let handleClient (clientSocket: WebSocket<ChatMessage>) server =
            let printServerMessage = printMessage server.Name server.EndPoint

            let handleConnectionRequest username = fio {
                let! clientEndpoint = !+ clientSocket.RequestUri.ToString()
                do! printServerMessage($"Received connection request for %s{username} from %s{clientEndpoint}.")
                match clients.TryGetValue username with
                | true, _ ->
                    let! message = !+ $"%s{username} is already connected. Connection failed!"
                    do! printServerMessage(message)
                    do! clientSocket.Send(ConnectionFailedResponse (server.Name, username, message))
                | false, _ ->
                    let! serverMessage = !+ $"%s{username} has connected from %s{clientEndpoint}."
                    let! clientMessage = !+ $"%s{username} has joined the chat. Welcome to %s{server.Name}! 🪻💜"
                    do! !+ clients.Add(username, clientSocket)
                    do! clientSocket.Send(ConnectionAcceptedResponse (server.Name, username, clientMessage))
                    do! printServerMessage(serverMessage)
                    do! broadcastMessage (NewConnectionResponse (server.Name, username, clientMessage)) (Some username)
            }

            let handlePrivateMessageRequest fromUsername toUsername message fromUrl = fio {
                match clients.TryGetValue toUsername with
                | true, toSocket ->
                    let! toUrl = !+ toSocket.RequestUri.ToString()
                    do! printMessage $"%s{fromUsername} -> %s{toUsername}" $"%s{fromUrl} -> %s{toUrl.ToString()}" message
                    do! toSocket.Send(PrivateMessageResponse (server.Name, fromUsername, toUsername, message))
                | false, _ ->
                    match clients.TryGetValue fromUsername with
                    | true, fromSocket ->
                        let! serverMessage = !+ $"%s{fromUsername} failed to private message %s{toUsername} because %s{toUsername} is not connected."
                        let! clientMessage = !+ $"%s{toUsername} is not connected."
                        do! printServerMessage(serverMessage)
                        do! fromSocket.Send(PrivateMessageFailedResponse (server.Name, fromUsername, toUsername, clientMessage))
                    | false, _ ->
                        do! printServerMessage($"Failed handling private message request from %s{fromUsername} to %s{toUsername}. Neither the sender or receiver is connected.")
            }

            let handleMessage message clientUrl = fio {
                match message with
                | ConnectionRequest requestedUsername ->
                    do! handleConnectionRequest requestedUsername
                | ConnectionAcceptedResponse(serverName, acceptedUsername, message) ->
                    do! printServerMessage($"Received a ConnectionAcceptedResponse with ServerName: %s{serverName}, AcceptedUsername: %s{acceptedUsername} and Message: %s{message}. Discarding.")
                | ConnectionFailedResponse(serverName, failedUsername, message) ->
                    do! printServerMessage($"Received a ConnectionFailedResponse with ServerName: %s{serverName}, FailedUsername: %s{failedUsername} and Message: %s{message}. Discarding.")
                | NewConnectionResponse(serverName, newUsername, message) ->
                    do! printServerMessage($"Received a NewConnectionResponse with ServerName: %s{serverName}, NewUsername: %s{newUsername} and Message: %s{message}. Discarding.")
                | BroadcastMessageRequest(fromUsername, message) ->
                    do! (printMessage fromUsername clientUrl message)
                    do! broadcastMessage ((BroadcastMessageResponse (serverName, fromUsername, message))) (Some fromUsername)
                | BroadcastMessageResponse(serverName, fromUsername, message) ->
                    do! printServerMessage($"Received a BroadcastMessageResponse with ServerName: %s{serverName}, FromUsername: %s{fromUsername} and Message: %s{message}. Discarding.")
                | PrivateMessageRequest(fromUsername, toUsername, message) ->
                    do! handlePrivateMessageRequest fromUsername toUsername message clientUrl
                | PrivateMessageResponse (serverName, fromUsername, toUsername, message) ->
                    do! printServerMessage($"Received a PrivateMessageResponse with ServerName: %s{serverName}, FromUsername: %s{fromUsername}, ToUsername: %s{toUsername} and Message: %s{message}. Discarding.")
                | PrivateMessageFailedResponse (serverName, fromUsername, toUsername, message) ->
                    do! printServerMessage($"Received a PrivateMessageFailedResponse with ServerName: %s{serverName}, FromUsername: %s{fromUsername}, ToUsername: %s{toUsername} and Message: %s{message}. Discarding.")
                | OnlineClientsRequest fromUsername ->
                    let clientList = clients.Keys |> List.ofSeq
                    do! printServerMessage($"""Sent online clients to %s{fromUsername}. Online clients: %s{String.Join(", ", clientList)}.""")
                    do! clientSocket.Send(OnlineClientsResponse (server.Name, fromUsername, clientList))
                | OnlineClientsResponse (serverName, toUsername, clientList) ->
                    do! printServerMessage($"""Received a OnlineClientsResponse with ServerName: %s{serverName}, ToUsername: %s{toUsername}, and ClientList: %s{String.Join(", ", clientList)}. Discarding.""")
            }

            fio {
                let! clientEndPoint = !+ clientSocket.RequestUri.ToString()
                try
                    while true do
                        let! message = clientSocket.Receive()
                        do! handleMessage message clientEndPoint
                with exn ->
                    let clientUsername = 
                        match clients |> Seq.tryFind (fun pair -> pair.Value = clientSocket) with
                        | Some pair -> pair.Key
                        | _ -> "Unknown"
                    let! serverMessage = !+ $"%s{clientUsername} from %s{clientEndPoint} has disconnected. Error: %s{exn.Message}"
                    let! clientMessage = !+ $"%s{clientUsername} has disconnected."
                    do! !+ (clients.Remove clientUsername |> ignore)
                    do! printServerMessage(serverMessage)
                    do! broadcastMessage ((BroadcastMessageResponse (server.Name, serverName, clientMessage))) None
                    do! clientSocket.Close()
            }

        fio {
            do! !+ Console.Clear();
            let! server = !+ { Name = serverName; EndPoint = serverUrl; Socket = new ServerWebSocket<ChatMessage>() }
            do! server.Socket.Start <| serverUrl
            do! printMessage server.Name server.EndPoint $"Server started on %s{server.EndPoint}. Listening for clients..."
            
            while true do
                let! clientSocket = server.Socket.Accept()
                do! !! (handleClient clientSocket server)
        }

    override this.effect = fio {
        do! server serverUrl
    }

[<EntryPoint>]
let main args =
    if args.Length = 0 then
        eprintfn "No arguments were provided. Please provide server url and server name!"
        exit 1
    FIOChatServerApp(args.[0], args.[1]).Run()
    exit 0
