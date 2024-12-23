open System
open System.Net
open System.Net.Sockets
open System.Globalization
open System.Collections.Generic

open FIO.Core
open FIO.Lib.Network.Socket

type Message =
    | ConnectionRequest of Username: string
    | ConnectionAcceptedResponse of ServerName: string * AcceptedUsername: string * Content: string
    | ConnectionFailedResponse of ServerName: string * FailedUsername: string * Content: string
    | BroadcastMessageRequest of SenderUsername: string * Content: string
    | BroadcastMessageResponse of SenderUsername: string * Content: string
    | PrivateMessageRequest of SenderUsername: string * ReceiverUsername: string * Content: string 
    | PrivateMessageResponse of SenderUsername: string * Content: string
    | PrivateMessageFailedResponse of ServerName: string * SenderUsername: string * ReceiverUsername: string * Content: string
    | OnlineClientListRequest of SenderUsername: string
    | OnlineClientListResponse of SenderUsername: string * ReceiverUsername: string * ClientList: string list

and Server =
    { Name: string
      EndPoint: string
      Listener: TcpListener }

and FIOChatServerApp(serverIp, port) =
    inherit FIOApp<unit, exn>()

    let clients = Dictionary<string, SocketChannel<Message>>()

    let getFormattedDate() = 
        DateTime.Now.ToString("dd.MM.yy HH:mm:ss", CultureInfo.InvariantCulture)

    let printMessage username endpoint message =
        printfn $"[%s{getFormattedDate()}] [%s{username} / %s{endpoint}]: %s{message}"

    let runServer ip port =
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

        let handleClient (clientChannel: SocketChannel<Message>) server =
            let printServerMessage = printMessage server.Name server.EndPoint

            let handleConnectionRequest username =
                fio {
                    let! clientEndpoint = clientChannel.RemoteEndPoint()
                    do! !+ printServerMessage($"Received connection request for %s{username} from %s{clientEndpoint.ToString()}.")
                    match clients.TryGetValue username with
                    | true, _ ->
                        let! content = !+ $"%s{username} is already connected. Connection failed!"
                        do! !+ printServerMessage(content)
                        return! clientChannel.Send(ConnectionFailedResponse (server.Name, username, content))
                    | false, _ ->
                        let! serverContent = !+ $"%s{username} has connected from %s{clientEndpoint.ToString()}."
                        let! clientContent = !+ $"%s{username} has joined the chat. Welcome to %s{server.Name}!"
                        do! !+ clients.Add(username, clientChannel)
                        do! clientChannel.Send(ConnectionAcceptedResponse (server.Name, username, clientContent))
                        do! !+ printServerMessage(serverContent)
                        return! broadcastMessage (BroadcastMessageResponse (server.Name, clientContent)) (Some username)
                }

            let handlePrivateMessageRequest senderUsername receiverUsername content clientEndPoint = 
                fio {
                    match clients.TryGetValue receiverUsername with
                    | true, receiverChannel ->
                        let! receiverEndpoint = receiverChannel.RemoteEndPoint()
                        do! !+ (printMessage $"%s{senderUsername} -> %s{receiverUsername}" $"%s{clientEndPoint} -> %s{receiverEndpoint.ToString()}" content)
                        do! receiverChannel.Send(PrivateMessageResponse (senderUsername, content))
                    | false, _ ->
                        match clients.TryGetValue senderUsername with
                        | true, channel ->
                            let! serverContent = !+ $"%s{senderUsername} failed to private message %s{receiverUsername} because %s{receiverUsername} is not connected."
                            let! clientContent = !+ $"%s{receiverUsername} is not connected."
                            do! !+ printServerMessage(serverContent)
                            do! channel.Send(PrivateMessageFailedResponse (server.Name, senderUsername, receiverUsername, clientContent))
                        | false, _ ->
                            do! !+ printServerMessage($"Failed handling private message request from %s{senderUsername} to %s{receiverUsername}. Neither the sender or receiver is connected.")
                }

            let handleMessage message clientEndPoint =
                fio {
                    match message with
                    | ConnectionRequest username ->
                        do! handleConnectionRequest username
                    | ConnectionAcceptedResponse(serverName, username, content) ->
                        do! !+ printServerMessage($"Received a ConnectionAcceptedResponse from %s{serverName} for %s{username} with content: %s{content}")
                    | ConnectionFailedResponse(serverName, username, content) ->
                         do! !+ printServerMessage($"Received a ConnectionFailedResponse from %s{serverName} for %s{username} with content: %s{content}")
                    | BroadcastMessageRequest(senderUsername, content) ->
                        do! !+ (printMessage senderUsername clientEndPoint content)
                        do! broadcastMessage ((BroadcastMessageResponse (senderUsername, content))) (Some senderUsername)
                    | BroadcastMessageResponse(senderName, content) ->
                        do! !+ printServerMessage($"Received a BroadcastMessageResponse from %s{senderName} with content: %s{content}")
                    | PrivateMessageRequest(senderUsername, receiverUsername, content) ->
                        do! handlePrivateMessageRequest senderUsername receiverUsername content clientEndPoint
                    | PrivateMessageResponse (senderUsername, content) ->
                        do! !+ printServerMessage($"Received a PrivateMessageResponse from %s{senderUsername} with content: %s{content}")
                    | PrivateMessageFailedResponse (serverName, senderUsername, receiverUsername, content) ->
                        do! !+ printServerMessage($"Received a PrivateMessageFailedResponse from %s{serverName} for %s{senderUsername} to %s{receiverUsername} with content: %s{content}")
                    | OnlineClientListRequest senderUsername ->
                        let clientList = clients.Keys |> List.ofSeq
                        do! !+ printServerMessage($"Sent client list to %s{senderUsername}.")
                        do! !+ printServerMessage($"""Currently connected clients: %s{String.Join(", ", clientList)}.""")
                        do! clientChannel.Send(OnlineClientListResponse (server.Name, senderUsername, clientList))
                    | OnlineClientListResponse (serverName, senderUsername, clientList) ->
                        do! !+ printServerMessage($"""Received a OnlineClientListResponse from %s{serverName} for %s{senderUsername} with content: %s{String.Join(", ", clientList)}""")
                }

            fio {
                let! clientEndPoint = clientChannel.RemoteEndPoint()
                try
                    while true do
                        let! message = clientChannel.Receive()
                        do! handleMessage message (clientEndPoint.ToString())
                with exn ->
                    let clientUsername = 
                        match clients |> Seq.tryFind (fun pair -> pair.Value = clientChannel) with
                        | Some pair -> pair.Key
                        | _ -> "Unknown"
                    let! serverContent = !+ $"%s{clientUsername} from %s{clientEndPoint.ToString()} has disconnected. Error: %s{exn.Message}"
                    let! clientContent = !+ $"%s{clientUsername} has disconnected."
                    do! !+ (clients.Remove clientUsername |> ignore)
                    do! !+ printServerMessage(serverContent)
                    do! broadcastMessage ((BroadcastMessageResponse (server.Name, clientContent))) None
                    do! clientChannel.Close()
            }

        let handleClients server =
            fio {
                while true do
                    let! clientSocket = !+ server.Listener.AcceptSocket()
                    let! clientChannel = !+ SocketChannel<Message>(clientSocket)
                    do! !! (handleClient clientChannel server)
            }

        fio {
            let! listener = !+ (new TcpListener(ip, port))
            let! server = !+ { Name = "FIOChat"; EndPoint = listener.LocalEndpoint.ToString(); Listener = listener }
            do! !+ server.Listener.Start()
            do! !+ (printMessage server.Name server.EndPoint $"Server started on %s{server.EndPoint}. Listening for clients...")
            return! handleClients server
        }

    override this.effect =
        runServer serverIp port

[<EntryPoint>]
let main args =
    if args.Length = 0 then
        eprintfn "No arguments were provided. Please provide port number."
        exit 1
    match Int32.TryParse(args.[0]) with
    | true, port when port > 0 && port < 65536 -> 
        FIOApp.Run(FIOChatServerApp(IPAddress.Loopback, port))
        exit 0
    | _ ->
        eprintfn $"Invalid port number: %s{args.[0]}"
        exit 2
