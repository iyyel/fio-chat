namespace FIOChat.Server

open System
open System.Collections.Generic

open FIOChat.Shared
open FIOChat.Server.Printing
open FIOChat.Server.Collections

open FIO.Core
open FIO.Library.Network.WebSockets

type Server =
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
            $"%s{serverName} (Server)" :: (clients.Keys |> List.ofSeq)

        let handleCommands server =
            let printServerMessage = printServerMessage server.Name server.EndPoint

            let handleKickCommand kickedUser message server = fio {
                match clients.TryGetValue kickedUser with
                | true, clientSocket ->
                    let! timestamp = !+ DateTime.Now
                    do! clientSocket.Send <| KickedResponse (server.Name, kickedUser, message, timestamp)
                    do! broadcastMessage (Some kickedUser) <| DisconnectionNotify (server.Name, kickedUser, $"%s{kickedUser} has been kicked with message: %s{message}", timestamp)
                    do! !+ (clients.Remove kickedUser |> ignore)
                    do! clientSocket.Close()
                | false, _ -> 
                    do! printServerMessage DateTime.Now $"Failed to kick %s{kickedUser}. User is not connected."
            }

            let handleBanCommand bannedUser message server = fio {
                bannedUsers.Add (bannedUser, message)
                let! timestamp = !+ DateTime.Now
                let! bannedUserMessage = !+ $"You have been banned with message: %s{message}"
                let! clientMessage = !+ $"%s{bannedUser} has been banned with message: %s{message}"
                match clients.TryGetValue bannedUser with
                | true, clientSocket ->
                    do! clientSocket.Send <| KickedResponse (server.Name, bannedUser, bannedUserMessage, timestamp)
                    do! broadcastMessage (Some bannedUser) <| DisconnectionNotify (server.Name, bannedUser, clientMessage, timestamp)
                    do! !+ (clients.Remove bannedUser |> ignore)
                    do! clientSocket.Close()
                | false, _ -> 
                    do! printServerMessage timestamp clientMessage
                    do! broadcastMessage (Some bannedUser) <| ServerBroadcastMessageResponse (server.Name, clientMessage, timestamp)
            }

            let handleUnbanCommand bannedUser message server = fio {
                let! timestamp = !+ DateTime.Now
                match bannedUsers.TryGetValue bannedUser with
                | true, _ ->
                    bannedUsers.Remove bannedUser
                    let! message = !+ $"%s{bannedUser} has been unbanned with message: %s{message}."
                    do! printServerMessage timestamp message
                    do! broadcastMessage (Some bannedUser) <| ServerBroadcastMessageResponse (server.Name, message, timestamp)
                | false, _ ->
                    do! printServerMessage timestamp $"%s{bannedUser} is not banned."
            }

            let handlePrivateMessageCommand toUser message server = fio {
                match clients.TryGetValue toUser with
                | true, clientSocket ->
                    do! clientSocket.Send <| PrivateMessageResponse (server.Name, server.Name, toUser, message, DateTime.Now)
                | false, _ ->
                    do! printServerMessage DateTime.Now $"Failed to send private message to %s{toUser}. User is not connected."
            }

            let handleMessageCommand message server = fio {
                let! timestamp = !+ DateTime.Now
                do! broadcastMessage None <| ServerBroadcastMessageResponse (server.Name, message, timestamp)
            }

            let parseAtInput (input: string) = fio {
                let! parts = !+ input.Trim().Split("@")
                let! info = !+ parts.[1].Trim().Split(":")
                return (info.[0].Trim(), info.[1].Trim())
            }

            fio {
                while true do
                    try
                        do! printInputPrompt server.Name
                        match Console.ReadLine() with
                        | input when input.Trim().Length = 0 ->
                            return ()
                        | input when input.Trim().StartsWith("\kick@") && input.Trim().Contains(':') ->
                            let! (kickedUser, message) = parseAtInput input
                            do! handleKickCommand kickedUser message server
                        | input when input.Trim().StartsWith("\\ban@") && input.Trim().Contains(':')->
                            let! (bannedUser, message) = parseAtInput input
                            do! handleBanCommand bannedUser message server
                        | input when input.Trim().StartsWith("\unban@") && input.Trim().Contains(':') ->
                            let! (unbannedUser, message) = parseAtInput input
                            do! handleUnbanCommand unbannedUser message server
                        | input when input.Trim().StartsWith("\\banned") ->
                            let banned = bannedUsers |> Seq.map (fun pair -> $"%s{pair.Key} - %s{pair.Value}") |> Seq.toList
                            let! message = if banned.Length = 0 then !+ "No banned users." else !+ $"""Banned users: %s{String.Join(", ", banned)}."""
                            do! printServerMessage DateTime.Now message
                        | input when input.Trim().StartsWith("\pm@") && input.Trim().Contains(':') ->
                            let! (toUser, message) = parseAtInput input
                            do! handlePrivateMessageCommand toUser message server
                        | input when input.Trim().StartsWith("\online") ->
                            do! printServerMessage DateTime.Now $"""Online: %s{String.Join(", ", onlineClients())}."""
                        | input when input.Trim().StartsWith("\msg:") ->
                            let! message = !+ (input.Trim().Split(":")[1]).Trim()
                            do! handleMessageCommand message server
                        | input when input.Trim().StartsWith("\commands") ->
                            do! printServerMessage DateTime.Now ($"\nCommands: \n" +
                                $"- To kick a user, type: \kick@<user>:<message>\n" +
                                $"- To ban a user, type: \\ban@<user>:<message>\n" +
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

            let handleConnectionRequest user clientEndPoint = fio {
                do! printServerMessage DateTime.Now $"Received connection request from %s{user} (%s{clientEndPoint})."
                let! timestamp = !+ DateTime.Now

                if user = server.Name then
                    let! serverMessage = !+ $"%s{user} is the name of the server. Connection denied!"
                    let! clientMessage = !+ $"%s{user} is a bad name. Change and try again."
                    printServerMessage timestamp serverMessage
                    do! clientSocket.Send <| ConnectionFailedResponse (server.Name, user, clientMessage, timestamp)
                    do! clientSocket.Close()
                elif bannedUsers.ContainsKey user then
                    let! message = !+ $"%s{user} is banned. Connection denied!"
                    printServerMessage timestamp $"%s{user} is banned. Connection denied!"
                    do! clientSocket.Send <| ConnectionFailedResponse (server.Name, user, message, timestamp)
                    do! clientSocket.Close()
                else
                    match clients.TryGetValue user with
                    | true, _ ->
                        let! serverMessage = !+ $"%s{user} is already connected!"
                        let! clientMessage = !+ $"%s{user} is already online!"
                        do! printServerMessage timestamp serverMessage
                        do! clientSocket.Send <| ConnectionFailedResponse (server.Name, user, clientMessage, timestamp)
                    | false, _ ->
                        let! serverMessage = !+ $"%s{user} (%s{clientEndPoint}) has connected."
                        let! clientMessage = !+ $"%s{user} has joined the chat. Welcome to %s{server.Name}! 🪻💜"
                        do! !+ clients.Add(user, clientSocket)
                        do! clientSocket.Send <| ConnectionAcceptedResponse (server.Name, user, clientMessage, timestamp)
                        broadcastMessageCache.ToList() |> List.map (fun message -> clientSocket.Send(message))
                        do! printServerMessage timestamp serverMessage
                        do! broadcastMessage None <| ConnectionNotify (server.Name, user, clientMessage, timestamp)
            }

            let handlePrivateMessageRequest fromUser toUser message fromUrl = fio {
                let! timestamp = !+ DateTime.Now
                match clients.TryGetValue toUser with
                | true, toSocket ->
                    let! toEndPoint = toSocket.RemoteEndPoint()
                                      >>= fun endPoint -> !+ endPoint.ToString()
                    do! printPrivateMessage $"%s{fromUser} -> %s{toUser}" $"%s{fromUrl} -> %s{toEndPoint}" timestamp message
                    do! toSocket.Send <| PrivateMessageResponse (server.Name, fromUser, toUser, message, timestamp)
                | false, _ ->
                    match clients.TryGetValue fromUser with
                    | true, fromSocket ->
                        let! serverMessage = !+ $"%s{fromUser} failed to private message %s{toUser} because %s{toUser} is not connected."
                        let! clientMessage = !+ $"%s{toUser} is not online."
                        do! printServerMessage timestamp serverMessage
                        do! fromSocket.Send <| PrivateMessageFailedResponse (server.Name, fromUser, toUser, clientMessage, timestamp)
                    | false, _ ->
                        do! printServerMessage DateTime.Now $"Failed handling private message request from %s{fromUser} to %s{toUser}. Neither the sender or receiver is connected."
            }

            let handleMessage message clientEndPoint = fio {
                let printServerMessage = printServerMessage DateTime.Now
                match message with
                | ConnectionRequest(user, _) ->
                    do! handleConnectionRequest user clientEndPoint
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
                    do! broadcastMessage (Some fromUser)
                        <| BroadcastMessageResponse (serverName, fromUser, message, timestamp)
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
                    let clientList = onlineClients ()
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
                | BannedResponse(server, toUser, message, timestamp) ->
                    do! printServerMessage $"Received a BannedResponse with Server: %s{server}, ToUser: %s{toUser}, Message: %s{message} and Timestamp: %s{timestamp.ToShortDateString()}. Discarding."
            }

            let handleDisconnect clientEndPoint = fio {
                let clientUser =
                    match clients |> Seq.tryFind (fun pair -> pair.Value = clientSocket) with
                    | Some pair -> Some pair.Key
                    | _ -> None
                match clientUser with
                | Some clientUser ->
                    let! serverMessage = !+ $"%s{clientUser} (%s{clientEndPoint}) has disconnected."
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

            fio {
                let! clientEndPoint = 
                    clientSocket.RemoteEndPoint()
                    >>= fun endPoint -> !+ endPoint.ToString()
                try
                    while true do
                        let! message = clientSocket.Receive()
                        do! clearInputPrompt()
                        do! handleMessage message clientEndPoint
                        do! printInputPrompt server.Name
                with _ ->
                    do! handleDisconnect clientEndPoint
                do! printInputPrompt server.Name
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
            let! timestamp = !+ DateTime.Now
            do! printServerMessage server.Name server.EndPoint timestamp $"Server started on %s{server.EndPoint}. Listening for clients..."
            do! printServerMessage server.Name server.EndPoint timestamp $"Type \commands for a list of available commands."
            do! handleClients server <!> handleCommands server
        }

    override this.effect = fio {
        do! runServer serverUrl
            >>? fun exn -> !- exn.Message
    }
