module ChatClient.App

open Message
open Printing

open System

open FIO.Core
open FIO.Library.Network.WebSockets
open System.Collections.Generic

type Server =
    { Name: string
      Url: string
      Socket: ServerWebSocket<Message> }

and ChatServerApp(serverUrl, serverName) =
    inherit FIOApp<unit, string>()

    let clients = Dictionary<string, WebSocket<Message>>()

    let runServer serverUrl =
        let broadcastMessage filteredUsername message =
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

        let handleClient (clientSocket: WebSocket<Message>) server =
            let printServerMessage = printServerMessage server.Name server.Url

            let handleConnectionRequest username = fio {
                let! clientUrl = !+ clientSocket.RequestUri.ToString()
                do! printServerMessage $"Received connection request from %s{username} (%s{clientUrl})."
                match clients.TryGetValue username with
                | true, _ ->
                    let! serverMessage = !+ $"%s{username} (%s{clientUrl}) is already connected!"
                    let! clientMessage = !+ $"%s{username} is already connected!"
                    do! printServerMessage(serverMessage)
                    do! clientSocket.Send <| ConnectionFailedResponse (server.Name, username, clientMessage)
                | false, _ ->
                    let! serverMessage = !+ $"%s{username} (%s{clientUrl}) has connected."
                    let! clientMessage = !+ $"%s{username} has joined the chat. Welcome to %s{server.Name}! 🪻💜"
                    do! !+ clients.Add(username, clientSocket)
                    do! clientSocket.Send <| ConnectionAcceptedResponse (server.Name, username, clientMessage)
                    do! printServerMessage(serverMessage)
                    do! broadcastMessage (Some username) <| ConnectionNotify (server.Name, username, clientMessage)
            }

            let handleDisconnect clientUrl = fio {
                let clientUsername = 
                    match clients |> Seq.tryFind (fun pair -> pair.Value = clientSocket) with
                    | Some pair -> Some pair.Key
                    | _ -> None
                match clientUsername with
                | Some clientUsername ->
                    let! serverMessage = !+ $"%s{clientUsername} from %s{clientUrl} has disconnected."
                    let! clientMessage = !+ $"%s{clientUsername} has disconnected."
                    do! !+ (clients.Remove clientUsername |> ignore)
                    do! printServerMessage serverMessage
                    do! broadcastMessage None <| (DisconnectionNotify (server.Name, serverName, clientMessage))
                    do! clientSocket.Close()
                | None -> 
                    let! serverMessage = !+ $"Duplicated connection attempt (%s{clientUrl}) has disconnected."
                    do! printServerMessage serverMessage
                    do! clientSocket.Close()
            }

            let handlePrivateMessageRequest fromUsername toUsername message fromUrl = fio {
                match clients.TryGetValue toUsername with
                | true, toSocket ->
                    let! toUrl = !+ toSocket.RequestUri.ToString()
                    do! printPrivateMessage $"%s{fromUsername} -> %s{toUsername}" $"%s{fromUrl} -> %s{toUrl.ToString()}" message
                    do! toSocket.Send <| PrivateMessageResponse (server.Name, fromUsername, toUsername, message)
                | false, _ ->
                    match clients.TryGetValue fromUsername with
                    | true, fromSocket ->
                        let! serverMessage = !+ $"%s{fromUsername} failed to private message %s{toUsername} because %s{toUsername} is not connected."
                        let! clientMessage = !+ $"%s{toUsername} is not connected."
                        do! printServerMessage serverMessage
                        do! fromSocket.Send <| PrivateMessageFailedResponse (server.Name, fromUsername, toUsername, clientMessage)
                    | false, _ ->
                        do! printServerMessage $"Failed handling private message request from %s{fromUsername} to %s{toUsername}. Neither the sender or receiver is connected."
            }

            let handleMessage message clientUrl = fio {
                match message with
                | ConnectionRequest requestedUsername ->
                    do! handleConnectionRequest requestedUsername
                | ConnectionAcceptedResponse(serverName, acceptedUsername, message) ->
                    do! printServerMessage $"Received a ConnectionAcceptedResponse with ServerName: %s{serverName}, AcceptedUsername: %s{acceptedUsername} and Message: %s{message}. Discarding."
                | ConnectionFailedResponse(serverName, failedUsername, message) ->
                    do! printServerMessage $"Received a ConnectionFailedResponse with ServerName: %s{serverName}, FailedUsername: %s{failedUsername} and Message: %s{message}. Discarding."
                | ConnectionNotify(serverName, newUsername, message) ->
                    do! printServerMessage $"Received a NewConnectionResponse with ServerName: %s{serverName}, NewUsername: %s{newUsername} and Message: %s{message}. Discarding."
                | DisconnectionNotify(serverName, disconnectedUsername, message) ->
                    do! printServerMessage $"Received a DisconnectionNotify with ServerName: %s{serverName}, DisconnectedUsername: %s{disconnectedUsername} and Message: %s{message}. Discarding."
                | BroadcastMessageRequest(fromUsername, message) ->
                    do! printClientMessage fromUsername clientUrl message
                    do! broadcastMessage (Some fromUsername) <| (BroadcastMessageResponse (serverName, fromUsername, message))
                | BroadcastMessageResponse(serverName, fromUsername, message) ->
                    do! printServerMessage $"Received a BroadcastMessageResponse with ServerName: %s{serverName}, FromUsername: %s{fromUsername} and Message: %s{message}. Discarding."
                | PrivateMessageRequest(fromUsername, toUsername, message) ->
                    do! handlePrivateMessageRequest fromUsername toUsername message clientUrl
                | PrivateMessageResponse(serverName, fromUsername, toUsername, message) ->
                    do! printServerMessage $"Received a PrivateMessageResponse with ServerName: %s{serverName}, FromUsername: %s{fromUsername}, ToUsername: %s{toUsername} and Message: %s{message}. Discarding."
                | PrivateMessageFailedResponse(serverName, fromUsername, toUsername, message) ->
                    do! printServerMessage $"Received a PrivateMessageFailedResponse with ServerName: %s{serverName}, FromUsername: %s{fromUsername}, ToUsername: %s{toUsername} and Message: %s{message}. Discarding."
                | OnlineClientsRequest fromUsername ->
                    let clientList = clients.Keys |> List.ofSeq
                    do! printServerMessage $"""Sent online clients to %s{fromUsername}. Online: %s{String.Join(", ", clientList)}."""
                    do! clientSocket.Send <| OnlineClientsResponse (server.Name, fromUsername, clientList)
                | OnlineClientsResponse(serverName, toUsername, clientList) ->
                    do! printServerMessage $"""Received a OnlineClientsResponse with ServerName: %s{serverName}, ToUsername: %s{toUsername}, and ClientList: %s{String.Join(", ", clientList)}. Discarding."""
                | HelpRequest fromUsername ->
                    let! helpMessage = !+ ($"\nHelp: \n" +
                        $"- To send a private message, type: \pm@<username>:<message>\n" +
                        $"- To see online clients, type: \online\n" +
                        $"- To see this help message, type: \help")
                    do! printServerMessage($"Sent help message to %s{fromUsername}.")
                    do! clientSocket.Send <| HelpResponse (server.Name, fromUsername, helpMessage)
                | HelpResponse(serverName, toUsername, message) ->
                    do! printServerMessage $"Received a HelpResponse with ServerName: %s{serverName}, ToUsername: %s{toUsername} and Message: %s{message}. Discarding."
            }

            fio {
                let! clientUrl = !+ clientSocket.RequestUri.ToString()
                try
                    while true do
                        let! message = clientSocket.Receive()
                        do! handleMessage message clientUrl
                with _ ->
                    do! handleDisconnect clientUrl
            }

        fio {
            do! clearConsole()
            let! server = !+ { Name = serverName; Url = serverUrl; Socket = new ServerWebSocket<Message>() }
            do! server.Socket.Start <| serverUrl
            do! printServerMessage server.Name server.Url $"Server started on %s{server.Url}. Listening for clients..."
            
            while true do
                let! clientSocket = server.Socket.Accept()
                do! !! (handleClient clientSocket server)
        }

    override this.effect = fio {
        do! runServer serverUrl
        >>? fun exn -> !- exn.Message
    }
