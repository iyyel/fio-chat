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
    let broadcastMsgCache = CircularBuffer<Message> 100

    let run serverUrl =
        let broadcastMsg filteredUser msg =
            broadcastMsgCache.Add msg
            let sendMsgs =
                match filteredUser with
                | Some filteredUser -> 
                    clients
                    |> Seq.filter (fun pair -> pair.Key <> filteredUser)
                    |> Seq.map _.Value.Send(msg)
                    |> Seq.fold (fun acc eff -> acc <!> eff) (!+  ())
                | None ->
                    clients
                    |> Seq.map _.Value.Send(msg)
                    |> Seq.fold (fun acc eff -> acc <!> eff) (!+ ())
            sendMsgs >? !- $"Failed to broadcast message: %A{msg}"

        let onlineClients () =
            $"%s{serverName} (Server)" :: (clients.Keys |> List.ofSeq)

        let handleCommands server =
            let printServerMsg = printServerMsg server.Name server.EndPoint

            let handleKickCommand kickedUser msg server = fio {
                match clients.TryGetValue kickedUser with
                | true, clientSocket ->
                    let! date = !+ DateTime.Now
                    do! clientSocket.Send 
                        <| KickedResponse (server.Name, kickedUser, msg, date)
                        >? !- "Failed to send KickedResponse!"
                    do! broadcastMsg (Some kickedUser) 
                        <| DisconnectionNotify (server.Name, kickedUser, $"%s{kickedUser} has been kicked with message: %s{msg}", date)
                    do! !+ (clients.Remove kickedUser |> ignore)
                    do! clientSocket.Close()
                        >? !- "Failed to close client socket!"
                | false, _ -> 
                    do! printServerMsg DateTime.Now $"Failed to kick %s{kickedUser}. User is not connected."
            }

            let handleBanCommand bannedUser msg server = fio {
                bannedUsers.Add (bannedUser, msg)
                let! date = !+ DateTime.Now
                let! bannedUserMsg = !+ $"You have been banned with message: %s{msg}"
                let! clientMsg = !+ $"%s{bannedUser} has been banned with message: %s{msg}"
                match clients.TryGetValue bannedUser with
                | true, clientSocket ->
                    do! clientSocket.Send 
                        <| KickedResponse (server.Name, bannedUser, bannedUserMsg, date)
                        >? !- "Failed to send KickedResponse!"
                    do! broadcastMsg (Some bannedUser)
                        <| DisconnectionNotify (server.Name, bannedUser, clientMsg, date)
                    do! !+ (clients.Remove bannedUser |> ignore)
                    do! clientSocket.Close()
                        >? !- "Failed to close client socket!"
                | false, _ -> 
                    do! printServerMsg date clientMsg
                    do! broadcastMsg (Some bannedUser)
                        <| ServerBroadcastMessageResponse (server.Name, clientMsg, date)
            }

            let handleUnbanCommand bannedUser msg server = fio {
                let! date = !+ DateTime.Now
                match bannedUsers.TryGetValue bannedUser with
                | true, _ ->
                    bannedUsers.Remove bannedUser
                    let! msg = !+ $"%s{bannedUser} has been unbanned with message: %s{msg}."
                    do! printServerMsg date msg
                    do! broadcastMsg (Some bannedUser)
                        <| ServerBroadcastMessageResponse (server.Name, msg, date)
                | false, _ ->
                    do! printServerMsg date $"%s{bannedUser} is not banned."
            }

            let handlePrivateMessageCommand toUser msg server = fio {
                match clients.TryGetValue toUser with
                | true, clientSocket ->
                    do! clientSocket.Send 
                        <| PrivateMessageResponse (server.Name, server.Name, toUser, msg, DateTime.Now)
                        >? !- "Failed to send PrivateMessageResponse!"
                | false, _ ->
                    do! printServerMsg DateTime.Now $"Failed to send private message to %s{toUser}. User is not connected."
            }

            let handleMessageCommand msg server = fio {
                do! broadcastMsg None
                    <| ServerBroadcastMessageResponse (server.Name, msg, DateTime.Now)
            }

            let parseAtInput (input: string) = fio {
                let! parts = !+ input.Trim().Split("@")
                let! info = !+ parts.[1].Trim().Split(":")
                return (info.[0].Trim(), info.[1].Trim())
            }

            fio {
                while true do
                    do! printInputPrompt server.Name
                    match Console.ReadLine() with
                    | input when input.Trim().Length = 0 ->
                        return ()
                    | input when input.Trim().StartsWith("\kick@") && input.Trim().Contains(':') ->
                        let! (kickedUser, msg) = parseAtInput input
                        do! handleKickCommand kickedUser msg server
                    | input when input.Trim().StartsWith("\\ban@") && input.Trim().Contains(':')->
                        let! (bannedUser, msg) = parseAtInput input
                        do! handleBanCommand bannedUser msg server
                    | input when input.Trim().StartsWith("\unban@") && input.Trim().Contains(':') ->
                        let! (unbannedUser, msg) = parseAtInput input
                        do! handleUnbanCommand unbannedUser msg server
                    | input when input.Trim().StartsWith("\\banned") ->
                        let banned = bannedUsers |> Seq.map (fun pair -> $"%s{pair.Key} - %s{pair.Value}") |> Seq.toList
                        let! msg = 
                            if banned.Length = 0 then 
                                !+ "No banned users."
                            else 
                                !+ $"""Banned users: %s{String.Join(", ", banned)}."""
                        do! printServerMsg DateTime.Now msg
                    | input when input.Trim().StartsWith("\pm@") && input.Trim().Contains(':') ->
                        let! (toUser, msg) = parseAtInput input
                        do! handlePrivateMessageCommand toUser msg server
                    | input when input.Trim().StartsWith("\online") ->
                        do! printServerMsg DateTime.Now $"""Online: %s{String.Join(", ", onlineClients())}."""
                    | input when input.Trim().StartsWith("\msg:") ->
                        let! msg = !+ (input.Trim().Split(":")[1]).Trim()
                        do! handleMessageCommand msg server
                    | input when input.Trim().StartsWith("\commands") ->
                        do! printServerMsg DateTime.Now ($"\nCommands: \n" +
                            $"- To kick a user, type: \kick@<user>:<message>\n" +
                            $"- To ban a user, type: \\ban@<user>:<message>\n" +
                            $"- To unban a user, type: \unban@<user>\n" +
                            $"- To see banned users, type: \\banned\n" +
                            $"- To send a private message, type: \pm@<user>:<message>\n" +
                            $"- To see online clients, type: \online\n" +
                            $"- To broadcast a message, type: \msg:<message>\n" +
                            $"- To see this help message, type: \commands")
                    | _ ->
                        do! printServerMsg DateTime.Now $"Invalid command. Type \commands for a list of commands."
            }

        let handleClient (clientSocket: WebSocket<Message>) server =
            let printServerMsg = printServerMsg server.Name server.EndPoint

            let handleConnectionRequest user clientEndPoint = fio {
                do! printServerMsg DateTime.Now $"Received connection request from %s{user} (%s{clientEndPoint})."
                let! date = !+ DateTime.Now

                if user = server.Name then
                    let! serverMsg = !+ $"%s{user} is the name of the server. Connection denied!"
                    let! clientMsg = !+ $"%s{user} is a bad name. Change and try again."
                    printServerMsg date serverMsg
                    do! clientSocket.Send 
                        <| ConnectionFailedResponse (server.Name, user, clientMsg, date)
                        >? !- "Failed to send ConnectionFailedResponse!"
                    do! clientSocket.Close()
                        >? !- "Failed to close client socket!"
                elif bannedUsers.ContainsKey user then
                    let! msg = !+ $"%s{user} is banned. Connection denied!"
                    printServerMsg date $"%s{user} is banned. Connection denied!"
                    do! clientSocket.Send 
                        <| ConnectionFailedResponse (server.Name, user, msg, date)
                        >? !- "Failed to send ConnectionFailedResponse!"
                    do! clientSocket.Close()
                        >? !- "Failed to close client socket!"
                else
                    match clients.TryGetValue user with
                    | true, _ ->
                        let! serverMsg = !+ $"%s{user} is already connected!"
                        let! clientMsg = !+ $"%s{user} is already online!"
                        do! printServerMsg date serverMsg
                        do! clientSocket.Send 
                            <| ConnectionFailedResponse (server.Name, user, clientMsg, date)
                            >? !- "Failed to send ConnectionFailedResponse!"
                    | false, _ ->
                        let! serverMsg = !+ $"%s{user} (%s{clientEndPoint}) has connected."
                        let! clientMsg = !+ $"%s{user} has joined the chat. Welcome to %s{server.Name}! 🪻💜"
                        do! !+ clients.Add(user, clientSocket)
                        do! clientSocket.Send 
                            <| ConnectionAcceptedResponse (server.Name, user, clientMsg, date)
                            >? !- "Failed to send ConnectionAcceptedResponse!"
                        broadcastMsgCache.ToList() |> List.map (fun msg -> clientSocket.Send msg)
                        do! printServerMsg date serverMsg
                        do! broadcastMsg None 
                            <| ConnectionNotify (server.Name, user, clientMsg, date)
            }

            let handlePrivateMessageRequest fromUser toUser msg fromUrl = fio {
                let! date = !+ DateTime.Now
                match clients.TryGetValue toUser with
                | true, toSocket ->
                    let! toEndPoint = 
                        toSocket.RemoteEndPoint()
                        >>= fun endPoint -> !+ endPoint.ToString()
                        >? !- "Failed to get remote endpoint of socket!"
                    do! printPrivateMsg $"%s{fromUser} -> %s{toUser}" $"%s{fromUrl} -> %s{toEndPoint}" date msg
                    do! toSocket.Send
                        <| PrivateMessageResponse (server.Name, fromUser, toUser, msg, date)
                        >? !- "Failed to send PrivateMessageResponse!"
                | false, _ ->
                    match clients.TryGetValue fromUser with
                    | true, fromSocket ->
                        let! serverMsg = !+ $"%s{fromUser} failed to private message %s{toUser} because %s{toUser} is not connected."
                        let! clientMsg = !+ $"%s{toUser} is not online."
                        do! printServerMsg date serverMsg
                        do! fromSocket.Send
                            <| PrivateMessageFailedResponse (server.Name, fromUser, toUser, clientMsg, date)
                            >? !- "Failed to send PrivateMessageFailedResponse!"
                    | false, _ ->
                        do! printServerMsg DateTime.Now $"Failed handling private message request from %s{fromUser} to %s{toUser}. Neither the sender or receiver is connected."
            }

            let handleMsg msg clientEndPoint = fio {
                let printServerMsg = printServerMsg DateTime.Now
                match msg with
                | ConnectionRequest (user, _) ->
                    do! handleConnectionRequest user clientEndPoint
                | ConnectionAcceptedResponse (server, user, msg, date) ->
                    do! printServerMsg $"Received a ConnectionAcceptedResponse with Server: %s{server}, User: %s{user}, Message: %s{msg} and Timestamp: %s{date.ToShortDateString()}. Discarding."
                | ConnectionFailedResponse (server, user, msg, date) ->
                    do! printServerMsg $"Received a ConnectionFailedResponse with Server: %s{server}, User: %s{user}, Message: %s{msg} and Timestamp: %s{date.ToShortDateString()}. Discarding."
                | ConnectionNotify (server, user, msg, date) ->
                    do! printServerMsg $"Received a ConnectionNotify with Server: %s{server}, User: %s{user}, Message: %s{msg} and Timestamp: %s{date.ToShortDateString()}. Discarding."
                | DisconnectionNotify (server, user, msg, date) ->
                    do! printServerMsg $"Received a DisconnectionNotify with Server: %s{server}, User: %s{user}, Message: %s{msg} and Timestamp: %s{date.ToShortDateString()}. Discarding."
                | BroadcastMessageRequest (fromUser, msg, _) ->
                    let! date = !+ DateTime.Now
                    do! printClientMsg fromUser clientEndPoint date msg
                    do! broadcastMsg (Some fromUser)
                        <| BroadcastMessageResponse (serverName, fromUser, msg, date)
                | BroadcastMessageResponse (server, fromUser, msg, date) ->
                    do! printServerMsg $"Received a BroadcastMessageResponse with Server: %s{server}, FromUser: %s{fromUser}, Message: %s{msg} and Timestamp: %s{date.ToShortDateString()}. Discarding."
                | ServerBroadcastMessageResponse (server, msg, date) ->
                    do! printServerMsg $"Received a ServerBroadcastMessageResponse with Server: %s{server}, Message: %s{msg} and Timestamp: %s{date.ToShortDateString()}. Discarding."
                | PrivateMessageRequest (fromUser, toUser, msg, _) ->
                    do! handlePrivateMessageRequest fromUser toUser msg clientEndPoint
                | PrivateMessageResponse (server, fromUser, toUser, msg, date) ->
                    do! printServerMsg $"Received a PrivateMessageResponse with Server: %s{server}, FromUser: %s{fromUser}, ToUser: %s{toUser}, Message: %s{msg} and Timestamp: %s{date.ToShortDateString()}. Discarding."
                | PrivateMessageFailedResponse (server, fromUser, toUser, msg, date) ->
                    do! printServerMsg $"Received a PrivateMessageFailedResponse with Server: %s{server}, FromUser: %s{fromUser}, ToUser: %s{toUser}, Message: %s{msg} and Timestamp: %s{date.ToShortDateString()}. Discarding."
                | OnlineClientsRequest (fromUser, _) ->
                    let clientList = onlineClients ()
                    do! printServerMsg $"""Sent online clients to %s{fromUser}. Online: %s{String.Join(", ", clientList)}."""
                    do! clientSocket.Send
                        <| OnlineClientsResponse (server.Name, fromUser, clientList, DateTime.Now)
                        >? !- "Failed to send OnlineClientsResponse!"
                | OnlineClientsResponse (server, toUser, clientList, date) ->
                    do! printServerMsg $"""Received a OnlineClientsResponse with Server: %s{server}, ToUser: %s{toUser}, ClientList: %s{String.Join(", ", clientList)} and Timestamp: %s{date.ToShortDateString()}. Discarding."""
                | HelpRequest (fromUser, _) ->
                    let! helpMsg = !+ ($"\nHelp: \n" +
                        $"- To send a private message, type: \pm@<user>:<message>\n" +
                        $"- To see online clients, type: \online\n" +
                        $"- To see this help message, type: \help")
                    do! printServerMsg $"Sent help message to %s{fromUser}."
                    do! clientSocket.Send
                        <| HelpResponse (server.Name, fromUser, helpMsg, DateTime.Now)
                        >? !- "Failed to send HelpResponse!"
                | HelpResponse (server, toUser, msg, date) ->
                    do! printServerMsg $"Received a HelpResponse with Server: %s{server}, ToUser: %s{toUser}, Message: %s{msg} and Timestamp: %s{date.ToShortDateString()}. Discarding."
                | KickedResponse (server, toUser, msg, date) ->
                    do! printServerMsg $"Received a KickedResponse with Server: %s{server}, ToUser: %s{toUser}, Message: %s{msg} and Timestamp: %s{date.ToShortDateString()}. Discarding."
                | BannedResponse (server, toUser, msg, date) ->
                    do! printServerMsg $"Received a BannedResponse with Server: %s{server}, ToUser: %s{toUser}, Message: %s{msg} and Timestamp: %s{date.ToShortDateString()}. Discarding."
            }

            let handleDisconnect clientEndPoint = fio {
                let clientUser =
                    match clients |> Seq.tryFind (fun pair -> pair.Value = clientSocket) with
                    | Some pair -> Some pair.Key
                    | _ -> None
                match clientUser with
                | Some clientUser ->
                    let! serverMsg = !+ $"%s{clientUser} (%s{clientEndPoint}) has disconnected."
                    let! clientMsg = !+ $"%s{clientUser} has left the chat. See you soon! 👋"
                    do! !+ (clients.Remove clientUser |> ignore)
                    let! date = !+ DateTime.Now
                    do! printServerMsg date serverMsg
                    do! broadcastMsg None
                        <| (DisconnectionNotify (server.Name, serverName, clientMsg, date))
                    do! clientSocket.Close()
                        >? !- "Failed to close client socket!"
                | None ->
                    do! printServerMsg DateTime.Now $"Bad connection attempt from %s{clientEndPoint}. Client has been disconnected."
                    do! clientSocket.Close()
                        >? !- "Failed to close client socket!"
            }

            fio {
                let! clientEndPoint =
                    clientSocket.RemoteEndPoint()
                    >>= fun endPoint -> !+ endPoint.ToString()
                    >? !- "Failed to get remote endpoint of client socket!"
                try
                    while true do
                        let! msg = 
                            clientSocket.Receive()
                            >? !- "Failed to receive message from client!"
                        do! clearInputPrompt()
                        do! handleMsg msg clientEndPoint
                        do! printInputPrompt server.Name
                with _ ->
                    do! handleDisconnect clientEndPoint
                do! printInputPrompt server.Name
            }

        let handleClients server = fio {
            while true do
                let! clientSocket =
                    server.Socket.Accept()
                    >? !- "Failed to accept incoming client connection!"
                do! !! (handleClient clientSocket server)
        }

        fio {
            do! clearConsole()
            let! server = !+ { Name = serverName; EndPoint = serverUrl; Socket = new ServerWebSocket<Message>() }
            do! server.Socket.Start <| server.EndPoint
                >? !- "Failed to start server!"
            let! date = !+ DateTime.Now
            do! printServerMsg server.Name server.EndPoint date $"Server started on %s{server.EndPoint}. Listening for clients..."
            do! printServerMsg server.Name server.EndPoint date $"Type \commands for a list of available commands."
            do! handleClients server <!> handleCommands server
        }

    override this.effect = fio {
        do! run serverUrl
    }
