module FIOChat.ServerApp

open FIOChat.Shared.Message
open FIOChat.Shared.Printing.Server

open System

open FIO.Core
open FIO.Library.Network.WebSockets
open System.Collections.Generic

open System.Collections.Immutable

type CircularBuffer<'T>(capacity: int) =
    let mutable queue = ImmutableQueue.Empty
    let mutable count = 0

    member this.Add(item: 'T) =
        if count >= capacity then
            queue <- queue.Dequeue()
        else
            count <- count + 1
        queue <- queue.Enqueue(item)

    member this.ToList() =
        queue |> Seq.toList

and Server =
    { Name: string
      EndPoint: string
      Socket: ServerWebSocket<Message> }

and ServerApp(serverUrl, serverName) =
    inherit FIOApp<unit, string>()

    let clients = Dictionary<string, WebSocket<Message>>()
    let bannedUsers = Dictionary<string, string>()
    let broadcastMessageCache = CircularBuffer<Message>(100)

    let runServer serverUrl =
        let broadcastMessage filteredUser message =
            broadcastMessageCache.Add message
            match filteredUser with
            | Some filteredUser -> 
                clients
                |> Seq.filter (fun pair -> pair.Key <> filteredUser)
                |> Seq.map _.Value.Send(message)
                |> Seq.fold (fun acc effect -> acc <!> effect) (!+  ())
            | None ->
                clients
                |> Seq.map _.Value.Send(message)
                |> Seq.fold (fun acc effect -> acc <!> effect) (!+ ())

        let onlineClients () =
            serverName :: (clients.Keys |> List.ofSeq)

        let handleKickCommand kickedUser message server = fio {
            match clients.TryGetValue kickedUser with
            | true, clientSocket ->
                let! timestamp = !+ DateTime.Now
                do! clientSocket.Send <| KickedResponse (server.Name, kickedUser, message, timestamp)
                do! broadcastMessage (Some kickedUser) <| DisconnectionNotify (server.Name, kickedUser, $"%s{kickedUser} has been kicked from the server with message: %s{message}", timestamp)
                do! !+ (clients.Remove kickedUser |> ignore)
                do! clientSocket.Close()
            | false, _ -> 
                do! printServerMessage server.Name server.EndPoint DateTime.Now $"Failed to kick %s{kickedUser}. User is not connected."
        }

        let handleBanCommand bannedUser message server = fio {
            bannedUsers.Add (bannedUser, message)
            let! timestamp = !+ DateTime.Now
            let! bannedUserMessage = !+ $"You have been banned from the server with message: %s{message}"
            let! clientMessage = !+ $"%s{bannedUser} has been banned from the server with message: %s{message}"
            match clients.TryGetValue bannedUser with
            | true, clientSocket ->
                do! clientSocket.Send <| KickedResponse (server.Name, bannedUser, bannedUserMessage, timestamp)
                do! broadcastMessage (Some bannedUser) <| DisconnectionNotify (server.Name, bannedUser, clientMessage, timestamp)
                do! !+ (clients.Remove bannedUser |> ignore)
                do! clientSocket.Close()
            | false, _ -> 
                do! printServerMessage server.Name server.EndPoint timestamp clientMessage
                do! broadcastMessage (Some bannedUser) <| ServerBroadcastMessageResponse (server.Name, clientMessage, timestamp)
        }

        let handleUnbanCommand bannedUser server = fio {
            bannedUsers.Remove bannedUser
            let! timestamp = !+ DateTime.Now
            let! message = !+ $"%s{bannedUser} has been unbanned."
            do! printServerMessage server.Name server.EndPoint timestamp message
            do! broadcastMessage (Some bannedUser) <| ServerBroadcastMessageResponse (server.Name, message, timestamp)
        }

        let handlePrivateMessageCommand toUser message server = fio {
            match clients.TryGetValue toUser with
            | true, clientSocket ->
                do! clientSocket.Send <| PrivateMessageResponse (server.Name, server.Name, toUser, message, DateTime.Now)
            | false, _ ->
                do! printServerMessage server.Name server.EndPoint DateTime.Now $"Failed to send private message to %s{toUser}. User is not connected."
        }

        let handleMessageCommand message server = fio {
            let! timestamp = !+ DateTime.Now
            do! printServerMessage server.Name server.EndPoint timestamp message
            do! broadcastMessage None <| ServerBroadcastMessageResponse (server.Name, message, timestamp)
        }

        let handleCommands server = fio {
            let printServerMessage = printServerMessage server.Name server.EndPoint
            while true do
                try
                    do! printInputPrompt(server.Name)
                    match Console.ReadLine() with
                    | input when input.Trim().Length = 0 ->
                        return ()
                    | input when input.Trim().StartsWith("\kick@") ->
                        let! parts = !+ (input.Trim().Split("@")[1])
                        let! info = !+ (parts.Split(":"))
                        let! kickedUser = !+ (info.[0].Trim())
                        let! message = !+ (info.[1].Trim())
                        do! handleKickCommand kickedUser message server
                    | input when input.Trim().StartsWith("\\ban@") ->
                        let! parts = !+ (input.Trim().Split("@")[1])
                        let! info = !+ (parts.Split(":"))
                        let! bannedUser = !+ (info.[0].Trim())
                        let! message = !+ (info.[1].Trim())
                        do! handleBanCommand bannedUser message server
                    | input when input.Trim().StartsWith("\unban@") ->
                        let! unbannedUser = !+ (input.Trim().Split("@")[1])
                        do! handleUnbanCommand unbannedUser server
                    | input when input.Trim().StartsWith("\\banned") ->
                        let banned = bannedUsers |> Seq.map (fun pair -> $"%s{pair.Key} - %s{pair.Value}")
                        do! printServerMessage DateTime.Now $"""Banned users: %s{String.Join(", ", banned)}."""
                    | input when input.Trim().StartsWith("\pm@") ->
                        let! parts = !+ (input.Trim().Split("@")[1])
                        let! info = !+ (parts.Split(":"))
                        let! toUser = !+ (info.[0].Trim())
                        let! message = !+ (info.[1].Trim())
                        do! handlePrivateMessageCommand toUser message server
                    | input when input.Trim().StartsWith("\online") ->
                        do! printServerMessage DateTime.Now $"""Online: %s{String.Join(", ", onlineClients())}."""
                    | input when input.Trim().StartsWith("\msg:") ->
                        let! message = !+ (input.Trim().Split(":")[1]).Trim()
                        do! handleMessageCommand message server
                    | input when input.Trim().StartsWith("\commands") ->
                        do! printServerMessage DateTime.Now ($"\nCommands: \n" +
                            $"- To kick a user, type: \kick@<user>:<message>\n" +
                            $"- To ban a user, type: \ban@<user>:<message>\n" +
                            $"- To unban a user, type: \unban@<user>\n" +
                            $"- To see banned users, type: \\banned\n" +
                            $"- To send a private message, type: \pm@<user>:<message>\n" +
                            $"- To see online clients, type: \online\n" +
                            $"- To broadcast a message, type: \msg:<message>\n" +
                            $"- To see this help message, type: \commands")
                    | _ ->
                        do! printServerMessage DateTime.Now $"Invalid command. Type \commands for a list of commands."
                with exn ->
                    do! printServerMessage DateTime.Now $"Failed to execute command. Error: %s{exn.Message}"
                    return ()
        }

        let handleClient (clientSocket: WebSocket<Message>) server =
            let printServerMessage = printServerMessage server.Name server.EndPoint

            let handleConnectionRequest user = fio {
                let! clientEndPoint = clientSocket.RemoteEndPoint()
                let! clientEndPointString = !+ clientEndPoint.ToString()
                do! printServerMessage DateTime.Now $"Received connection request from %s{user} (%s{clientEndPointString})."

                if user = server.Name then
                    printServerMessage DateTime.Now $"%s{user} (%s{clientEndPointString}) is trying to connect to itself. Connection denied."
                    do! clientSocket.Close()
                elif bannedUsers.ContainsKey user then
                    printServerMessage DateTime.Now $"%s{user} (%s{clientEndPointString}) is banned. Connection denied."
                    do! clientSocket.Close()
                else
                    match clients.TryGetValue user with
                    | true, _ ->
                        let! serverMessage = !+ $"%s{user} (%s{clientEndPointString}) is already connected!"
                        let! clientMessage = !+ $"%s{user} is already online!"
                        let! timestamp = !+ DateTime.Now
                        do! printServerMessage timestamp serverMessage
                        do! clientSocket.Send <| ConnectionFailedResponse (server.Name, user, clientMessage, timestamp)
                    | false, _ ->
                        let! serverMessage = !+ $"%s{user} (%s{clientEndPointString}) has connected."
                        let! clientMessage = !+ $"%s{user} has joined the chat. Welcome to %s{server.Name}! 🪻💜"
                        broadcastMessageCache.ToList() |> List.map (fun message -> clientSocket.Send(message))
                        do! !+ clients.Add(user, clientSocket)
                        let! timestamp = !+ DateTime.Now
                        do! clientSocket.Send <| ConnectionAcceptedResponse (server.Name, user, clientMessage, timestamp)
                        do! printServerMessage timestamp serverMessage
                        do! broadcastMessage (Some user) <| ConnectionNotify (server.Name, user, clientMessage, timestamp)
            }

            let handleDisconnect clientEndPoint = fio {
                let clientUser = 
                    match clients |> Seq.tryFind (fun pair -> pair.Value = clientSocket) with
                    | Some pair -> Some pair.Key
                    | _ -> None
                match clientUser with
                | Some clientUser ->
                    let! serverMessage = !+ $"%s{clientUser} from %s{clientEndPoint} has disconnected."
                    let! clientMessage = !+ $"%s{clientUser} has left the chat. See you soon! 👋"
                    do! !+ (clients.Remove clientUser |> ignore)
                    let! timestamp = !+ DateTime.Now
                    do! printServerMessage timestamp serverMessage
                    do! broadcastMessage None <| (DisconnectionNotify (server.Name, serverName, clientMessage, timestamp))
                    do! clientSocket.Close()
                | None ->
                    do! printServerMessage DateTime.Now $"Bad connection attempt from %s{clientEndPoint}. Client has been disconnected."
                    do! clientSocket.Close()
            }

            let handlePrivateMessageRequest fromUser toUser message fromUrl = fio {
                match clients.TryGetValue toUser with
                | true, toSocket ->
                    let! toEndPoint = !+ toSocket.RemoteEndPoint()
                    let! timestamp = !+ DateTime.Now
                    do! printPrivateMessage $"%s{fromUser} -> %s{toUser}" $"%s{fromUrl} -> %s{toEndPoint.ToString()}" timestamp message
                    do! toSocket.Send <| PrivateMessageResponse (server.Name, fromUser, toUser, message, timestamp)
                | false, _ ->
                    match clients.TryGetValue fromUser with
                    | true, fromSocket ->
                        let! serverMessage = !+ $"%s{fromUser} failed to private message %s{toUser} because %s{toUser} is not connected."
                        let! clientMessage = !+ $"%s{toUser} is not online."
                        let! timestamp = !+ DateTime.Now
                        do! printServerMessage timestamp serverMessage
                        do! fromSocket.Send <| PrivateMessageFailedResponse (server.Name, fromUser, toUser, clientMessage, timestamp)
                    | false, _ ->
                        do! printServerMessage DateTime.Now $"Failed handling private message request from %s{fromUser} to %s{toUser}. Neither the sender or receiver is connected."
            }

            let handleMessage message clientEndPoint = fio {
                let printServerMessage = printServerMessage DateTime.Now
                match message with
                | ConnectionRequest(user, _) ->
                    do! handleConnectionRequest user
                | ConnectionAcceptedResponse(server, user, message, timestamp) ->
                    do! printServerMessage $"Received a ConnectionAcceptedResponse with Server: %s{server}, User: %s{user}, Message: %s{message} and Timestamp: %s{timestamp.ToShortDateString()}. Discarding."
                | ConnectionFailedResponse(server, user, message, timestamp) ->
                    do! printServerMessage $"Received a ConnectionFailedResponse with Server: %s{server}, User: %s{user}, Message: %s{message} and Timestamp: %s{timestamp.ToShortDateString()}. Discarding."
                | ConnectionNotify(server, user, message, timestamp) ->
                    do! printServerMessage $"Received a ConnectionNotify with Server: %s{server}, User: %s{user}, Message: %s{message} and Timestamp: %s{timestamp.ToShortDateString()}. Discarding."
                | DisconnectionNotify(server, user, message, timestamp) ->
                    do! printServerMessage $"Received a DisconnectionNotify with Server: %s{server}, User: %s{user}, Message: %s{message} and Timestamp: %s{timestamp.ToShortDateString()}. Discarding."
                | BroadcastMessageRequest(fromUser, message, _) ->
                    let! timestamp = !+ DateTime.Now
                    do! printClientMessage fromUser clientEndPoint timestamp message
                    do! broadcastMessage (Some fromUser) <| (BroadcastMessageResponse (serverName, fromUser, message, timestamp))
                | BroadcastMessageResponse(server, fromUser, message, timestamp) ->
                    do! printServerMessage $"Received a BroadcastMessageResponse with Server: %s{server}, FromUser: %s{fromUser}, Message: %s{message} and Timestamp: %s{timestamp.ToShortDateString()}. Discarding."
                | ServerBroadcastMessageResponse(server, message, timestamp) ->
                    do! printServerMessage $"Received a ServerBroadcastMessageResponse with Server: %s{server}, Message: %s{message} and Timestamp: %s{timestamp.ToShortDateString()}. Discarding."
                | PrivateMessageRequest(fromUser, toUser, message, _) ->
                    do! handlePrivateMessageRequest fromUser toUser message clientEndPoint
                | PrivateMessageResponse(server, fromUser, toUser, message, timestamp) ->
                    do! printServerMessage $"Received a PrivateMessageResponse with Server: %s{server}, FromUser: %s{fromUser}, ToUser: %s{toUser}, Message: %s{message} and Timestamp: %s{timestamp.ToShortDateString()}. Discarding."
                | PrivateMessageFailedResponse(server, fromUser, toUser, message, timestamp) ->
                    do! printServerMessage $"Received a PrivateMessageFailedResponse with Server: %s{server}, FromUser: %s{fromUser}, ToUser: %s{toUser}, Message: %s{message} and Timestamp: %s{timestamp.ToShortDateString()}. Discarding."
                | OnlineClientsRequest (fromUser, _) ->
                    let clientList = server.Name :: (clients.Keys |> List.ofSeq)
                    do! printServerMessage $"""Sent online clients to %s{fromUser}. Online: %s{String.Join(", ", clientList)}."""
                    do! clientSocket.Send <| OnlineClientsResponse (server.Name, fromUser, clientList, DateTime.Now)
                | OnlineClientsResponse(server, toUser, clientList, timestamp) ->
                    do! printServerMessage $"""Received a OnlineClientsResponse with Server: %s{server}, ToUser: %s{toUser}, ClientList: %s{String.Join(", ", clientList)} and Timestamp: %s{timestamp.ToShortDateString()}. Discarding."""
                | HelpRequest (fromUser, _) ->
                    let! helpMessage = !+ ($"\nHelp: \n" +
                        $"- To send a private message, type: \pm@<user>:<message>\n" +
                        $"- To see online clients, type: \online\n" +
                        $"- To see this help message, type: \help")
                    do! printServerMessage($"Sent help message to %s{fromUser}.")
                    do! clientSocket.Send <| HelpResponse (server.Name, fromUser, helpMessage, DateTime.Now)
                | HelpResponse(server, toUser, message, timestamp) ->
                    do! printServerMessage $"Received a HelpResponse with Server: %s{server}, ToUser: %s{toUser}, Message: %s{message} and Timestamp: %s{timestamp.ToShortDateString()}. Discarding."
                | KickedResponse(server, toUser, message, timestamp) ->
                    do! printServerMessage $"Received a KickedResponse with Server: %s{server}, ToUser: %s{toUser}, Message: %s{message} and Timestamp: %s{timestamp.ToShortDateString()}. Discarding."
            }

            fio {
                let! clientEndPoint = clientSocket.RemoteEndPoint()
                try
                    while true do
                        let! message = clientSocket.Receive()
                        do! clearInputPrompt()
                        do! handleMessage message (clientEndPoint.ToString())
                        do! printInputPrompt server.Name
                with _ ->
                    do! handleDisconnect (clientEndPoint.ToString())
            }

        let handleClients server = fio {
            while true do
                let! clientSocket = server.Socket.Accept()
                do! !! (handleClient clientSocket server)
        }

        fio {
            do! clearConsole()
            let! server = !+ { Name = serverName; EndPoint = serverUrl; Socket = new ServerWebSocket<Message>() }
            do! server.Socket.Start <| server.EndPoint
            do! printServerMessage server.Name server.EndPoint DateTime.Now $"Server started on %s{server.EndPoint}. Listening for clients..."
            do! handleClients server <!> handleCommands server
        }

    override this.effect = fio {
        do! runServer serverUrl
        >>? fun exn -> !- exn.Message
    }
