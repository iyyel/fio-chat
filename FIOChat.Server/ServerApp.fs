namespace FIOChat.Server

open FSharp.FIO.DSL
open FSharp.FIO.App
open FSharp.FIO.Lib.IO
open FSharp.FIO.Lib.Net.WebSockets

open FIOChat.Common
open FIOChat.Server.Printing

open CircularBuffer

open System
open System.Collections.Generic
open System.Text.Json.Serialization

type Server =
    { Name: string
      EndPoint: string
      Socket: FServerWebSocket<Message, Message, exn> }

and ServerApp (serverUrl, serverName) =
    inherit FIOApp<unit, exn> ()
    
    let clients = Dictionary<string, FWebSocket<Message, Message, exn>>()
    let bannedUsers = Dictionary<string, string>()
    let broadcastMsgCache = CircularBuffer<Message> 100
    let options = JsonFSharpOptions.Default().ToJsonSerializerOptions()

    let run serverUrl =

        let broadcastMsg filteredUser msg =
            fio {
                let! sendMsgs = !<< (fun () ->
                    broadcastMsgCache.PushBack msg
                    match filteredUser with
                    | Some filteredUser ->
                        clients
                        |> Seq.filter (fun pair -> pair.Key <> filteredUser)
                        |> Seq.map _.Value.Send(msg)
                        |> Seq.fold (fun acc eff -> acc <~> eff) !+ ()
                    | None ->
                        clients
                        |> Seq.map _.Value.Send(msg)
                        |> Seq.fold (fun acc eff -> acc <~> eff) !+ ())
                return! sendMsgs
            }

        let onlineClients () =
            $"%s{serverName} (Server)" :: (clients.Keys |> List.ofSeq)

        let handleCommands server =
            let printServerMsg = printServerMsg server.Name server.EndPoint

            let handleKickCommand kickedUser msg server =
                fio {
                    match! !<< (fun () -> clients.TryGetValue kickedUser) with
                    | true, clientSocket ->
                        let! date = !+ DateTime.Now
                        do! clientSocket.Send
                            <| KickedResponse (server.Name, kickedUser, msg, date)
                        do! broadcastMsg (Some kickedUser)
                            <| DisconnectionNotify (server.Name, kickedUser, $"%s{kickedUser} has been kicked with message: %s{msg}", date)
                        do! !<< (fun () -> clients.Remove kickedUser |> ignore)
                        do! clientSocket.Close ()
                    | false, _ -> 
                        do! printServerMsg DateTime.Now $"Failed to kick %s{kickedUser}. User is not connected."
                }

            let handleBanCommand bannedUser msg server =
                fio {
                    do! !<< (fun () -> bannedUsers.Add (bannedUser, msg))
                    let! date = !+ DateTime.Now
                    let! bannedUserMsg = !+ $"You have been banned with message: %s{msg}"
                    let! clientMsg = !+ $"%s{bannedUser} has been banned with message: %s{msg}"
                    match! !<< (fun () -> clients.TryGetValue bannedUser) with
                    | true, clientSocket ->
                        do! clientSocket.Send
                            <| KickedResponse (server.Name, bannedUser, bannedUserMsg, date)
                        do! broadcastMsg (Some bannedUser)
                            <| DisconnectionNotify (server.Name, bannedUser, clientMsg, date)
                        do! !<< (fun () -> clients.Remove bannedUser |> ignore)
                        do! clientSocket.Abort ()
                    | false, _ -> 
                        do! printServerMsg date clientMsg
                        do! broadcastMsg (Some bannedUser)
                            <| ServerBroadcastMessageResponse (server.Name, clientMsg, date)
                }

            let handleUnbanCommand bannedUser msg server =
                fio {
                    let! date = !+ DateTime.Now
                    match! !<< (fun () -> bannedUsers.TryGetValue bannedUser) with
                    | true, _ ->
                        do! !<< (fun () -> bannedUsers.Remove bannedUser |> ignore)
                        let! msg = !+ $"%s{bannedUser} has been unbanned with message: %s{msg}."
                        do! printServerMsg date msg
                        do! broadcastMsg (Some bannedUser)
                            <| ServerBroadcastMessageResponse (server.Name, msg, date)
                    | false, _ ->
                        do! printServerMsg date
                            <| $"%s{bannedUser} is not banned."
                }

            let handlePrivateMessageCommand toUser msg server =
                fio {
                    match! !<< (fun () -> clients.TryGetValue toUser) with
                    | true, clientSocket ->
                        do! clientSocket.Send
                            <| PrivateMessageResponse (server.Name, server.Name, toUser, msg, DateTime.Now)
                    | false, _ ->
                        do! printServerMsg DateTime.Now
                            <| $"Failed to send private message to %s{toUser}. User is not connected."
                }

            let handleMessageCommand msg server =
                fio {
                    do! broadcastMsg None
                        <| ServerBroadcastMessageResponse (server.Name, msg, DateTime.Now)
                }

            let parseAtInput (input: string) =
                fio {
                    let! parts = !<< (fun () -> input.Trim().Split "@")
                    let! info = !<< (fun () -> parts[1].Trim().Split ":")
                    return! !<< (fun () -> info[0].Trim(), info[1].Trim())
                }

            fio {
                while true do
                    do! printInputPrompt server.Name
                    match! FConsole.ReadLine() with
                    | input when input.Trim().Length = 0 ->
                        return ()
                    | input when input.Trim().StartsWith "\kick@" && input.Trim().Contains ':' ->
                        let! kickedUser, msg = parseAtInput input
                        do! handleKickCommand kickedUser msg server
                    | input when input.Trim().StartsWith "\\ban@" && input.Trim().Contains ':'->
                        let! bannedUser, msg = parseAtInput input
                        do! handleBanCommand bannedUser msg server
                    | input when input.Trim().StartsWith "\unban@" && input.Trim().Contains ':' ->
                        let! unbannedUser, msg = parseAtInput input
                        do! handleUnbanCommand unbannedUser msg server
                    | input when input.Trim().StartsWith "\\banned" ->
                        let! banned = !<< (fun () -> bannedUsers |> Seq.map (fun pair -> $"%s{pair.Key} - %s{pair.Value}") |> Seq.toList)
                        let! msg = 
                            if banned.Length = 0 then 
                                !+ "No banned users."
                            else 
                                !+ $"""Banned users: %s{String.Join(", ", banned)}."""
                        do! printServerMsg DateTime.Now msg
                    | input when input.Trim().StartsWith "\pm@" && input.Trim().Contains ':' ->
                        let! toUser, msg = parseAtInput input
                        do! handlePrivateMessageCommand toUser msg server
                    | input when input.Trim().StartsWith "\online" ->
                        do! printServerMsg DateTime.Now $"""Online: %s{String.Join(", ", onlineClients())}."""
                    | input when input.Trim().StartsWith "\msg:" ->
                        let! msg = !+ (input.Trim().Split(":")[1]).Trim()
                        do! handleMessageCommand msg server
                    | input when input.Trim().StartsWith "\commands" ->
                        do! printServerMsg DateTime.Now 
                            <| ($"\nCommands: \n" +
                            $"- To kick a user, type: \kick@<user>:<message>\n" +
                            $"- To ban a user, type: \\ban@<user>:<message>\n" +
                            $"- To unban a user, type: \unban@<user>\n" +
                            $"- To see banned users, type: \\banned\n" +
                            $"- To send a private message, type: \pm@<user>:<message>\n" +
                            $"- To see online clients, type: \online\n" +
                            $"- To broadcast a message, type: \msg:<message>\n" +
                            $"- To see this help message, type: \commands")
                    | _ ->
                        do! printServerMsg DateTime.Now
                            <| $"Invalid command. Type \commands for a list of commands."
            }

        let handleClient (clientSocket: FWebSocket<Message, Message, exn>) server =
            let printServerMsg = printServerMsg server.Name server.EndPoint

            let handleConnectionRequest user clientEndPoint =
                fio {
                    let! date = !+ DateTime.Now
                    do! printServerMsg date
                        <| $"Received connection request from %s{user} (%s{clientEndPoint})."
                    let! isUserBanned = !<< (fun () -> bannedUsers.ContainsKey user)
                    
                    if user = server.Name then
                        let! serverMsg = !+ $"%s{user} is the name of the server. Connection denied!"
                        let! clientMsg = !+ $"%s{user} is a bad name. Change and try again."
                        printServerMsg date serverMsg
                        do! clientSocket.Send
                            <| ConnectionFailedResponse (server.Name, user, clientMsg, date)
                        do! clientSocket.Close ()
                    elif isUserBanned then
                        let! msg = !+ $"%s{user} is banned. Connection denied!"
                        printServerMsg date $"%s{user} is banned. Connection denied!"
                        do! clientSocket.Send
                            <| ConnectionFailedResponse (server.Name, user, msg, date)
                        do! clientSocket.Close ()
                    else
                        match! !<< (fun () -> clients.TryGetValue user) with
                        | true, _ ->
                            let! serverMsg = !+ $"%s{user} is already connected!"
                            let! clientMsg = !+ $"%s{user} is already online!"
                            do! printServerMsg date serverMsg
                            do! clientSocket.Send
                                <| ConnectionFailedResponse (server.Name, user, clientMsg, date)
                        | false, _ ->
                            let! serverMsg = !+ $"%s{user} (%s{clientEndPoint}) has connected."
                            let! clientMsg = !+ $"%s{user} has joined the chat. Welcome to %s{server.Name}! 🪻💜"
                            do! !<< (fun () -> clients.Add(user, clientSocket))
                            do! clientSocket.Send
                                <| ConnectionAcceptedResponse (server.Name, user, clientMsg, date)
                            for msg in broadcastMsgCache.ToArray () do
                                do! clientSocket.Send msg
                            do! printServerMsg date serverMsg
                            do! broadcastMsg None
                                <| ConnectionNotify (server.Name, user, clientMsg, date)
                }

            let handlePrivateMessageRequest fromUser toUser msg fromUrl =
                fio {
                    let! date = !+ DateTime.Now
                    match! !<< (fun () -> clients.TryGetValue toUser) with
                    | true, toSocket ->
                        let! toEndPoint = 
                            toSocket.RemoteEndPoint()
                            >>= fun endPoint -> !+ endPoint.ToString()
                        do! printPrivateMsg $"%s{fromUser} -> %s{toUser}" $"%s{fromUrl} -> %s{toEndPoint}" date msg
                        do! toSocket.Send <| PrivateMessageResponse (server.Name, fromUser, toUser, msg, date)
                    | false, _ ->
                        match! !<< (fun () -> clients.TryGetValue fromUser) with
                        | true, fromSocket ->
                            let! serverMsg = !+ $"%s{fromUser} failed to private message %s{toUser} because %s{toUser} is not connected."
                            let! clientMsg = !+ $"%s{toUser} is not online."
                            do! printServerMsg date serverMsg
                            do! fromSocket.Send <| PrivateMessageFailedResponse (server.Name, fromUser, toUser, clientMsg, date)
                        | false, _ ->
                            do! printServerMsg DateTime.Now $"Failed handling private message request from %s{fromUser} to %s{toUser}. Neither the sender or receiver is connected."
                }

            let handleMsg msg clientEndPoint =
                let printServerMsg = printServerMsg DateTime.Now
                fio {
                    match msg with
                    | ConnectionRequest (user, _) ->
                        do! handleConnectionRequest user clientEndPoint
                    | ConnectionAcceptedResponse (server, user, msg, date) ->
                        do! printServerMsg
                            <| $"Received a ConnectionAcceptedResponse with Server: %s{server}, User: %s{user}, Message: %s{msg} and Timestamp: %s{date.ToShortDateString()}. Discarding."
                    | ConnectionFailedResponse (server, user, msg, date) ->
                        do! printServerMsg
                            <| $"Received a ConnectionFailedResponse with Server: %s{server}, User: %s{user}, Message: %s{msg} and Timestamp: %s{date.ToShortDateString()}. Discarding."
                    | ConnectionNotify (server, user, msg, date) ->
                        do! printServerMsg
                            <| $"Received a ConnectionNotify with Server: %s{server}, User: %s{user}, Message: %s{msg} and Timestamp: %s{date.ToShortDateString()}. Discarding."
                    | DisconnectionNotify (server, user, msg, date) ->
                        do! printServerMsg
                            <| $"Received a DisconnectionNotify with Server: %s{server}, User: %s{user}, Message: %s{msg} and Timestamp: %s{date.ToShortDateString()}. Discarding."
                    | BroadcastMessageRequest (fromUser, msg, _) ->
                        let! date = !+ DateTime.Now
                        do! printClientMsg fromUser clientEndPoint date msg
                        do! broadcastMsg (Some fromUser)
                            <| BroadcastMessageResponse (serverName, fromUser, msg, date)
                    | BroadcastMessageResponse (server, fromUser, msg, date) ->
                        do! printServerMsg
                            <| $"Received a BroadcastMessageResponse with Server: %s{server}, FromUser: %s{fromUser}, Message: %s{msg} and Timestamp: %s{date.ToShortDateString()}. Discarding."
                    | ServerBroadcastMessageResponse (server, msg, date) ->
                        do! printServerMsg
                            <| $"Received a ServerBroadcastMessageResponse with Server: %s{server}, Message: %s{msg} and Timestamp: %s{date.ToShortDateString()}. Discarding."
                    | PrivateMessageRequest (fromUser, toUser, msg, _) ->
                        do! handlePrivateMessageRequest fromUser toUser msg clientEndPoint
                    | PrivateMessageResponse (server, fromUser, toUser, msg, date) ->
                        do! printServerMsg
                            <| $"Received a PrivateMessageResponse with Server: %s{server}, FromUser: %s{fromUser}, ToUser: %s{toUser}, Message: %s{msg} and Timestamp: %s{date.ToShortDateString()}. Discarding."
                    | PrivateMessageFailedResponse (server, fromUser, toUser, msg, date) ->
                        do! printServerMsg
                            <| $"Received a PrivateMessageFailedResponse with Server: %s{server}, FromUser: %s{fromUser}, ToUser: %s{toUser}, Message: %s{msg} and Timestamp: %s{date.ToShortDateString()}. Discarding."
                    | OnlineClientsRequest (fromUser, _) ->
                        let clientList = onlineClients ()
                        do! printServerMsg
                            <| $"""Sent online clients to %s{fromUser}. Online: %s{String.Join(", ", clientList)}."""
                        do! clientSocket.Send
                            <| OnlineClientsResponse (server.Name, fromUser, clientList, DateTime.Now)
                    | OnlineClientsResponse (server, toUser, clientList, date) ->
                        do! printServerMsg
                            <| $"""Received a OnlineClientsResponse with Server: %s{server}, ToUser: %s{toUser}, ClientList: %s{String.Join(", ", clientList)} and Timestamp: %s{date.ToShortDateString()}. Discarding."""
                    | HelpRequest (fromUser, _) ->
                        let! helpMsg = !+ ($"\nHelp: \n" +
                            $"- To send a private message, type: \pm@<user>:<message>\n" +
                            $"- To see online clients, type: \online\n" +
                            $"- To see this help message, type: \help")
                        do! printServerMsg $"Sent help message to %s{fromUser}."
                        do! clientSocket.Send
                            <| HelpResponse (server.Name, fromUser, helpMsg, DateTime.Now)
                    | HelpResponse (server, toUser, msg, date) ->
                        do! printServerMsg
                            <| $"Received a HelpResponse with Server: %s{server}, ToUser: %s{toUser}, Message: %s{msg} and Timestamp: %s{date.ToShortDateString()}. Discarding."
                    | KickedResponse (server, toUser, msg, date) ->
                        do! printServerMsg
                            <| $"Received a KickedResponse with Server: %s{server}, ToUser: %s{toUser}, Message: %s{msg} and Timestamp: %s{date.ToShortDateString()}. Discarding."
                }

            let handleDisconnect clientEndPoint =
                fio {
                    let! clientUser = !<< (fun () -> 
                        match clients |> Seq.tryFind (fun pair -> pair.Value = clientSocket) with
                        | Some pair -> Some pair.Key
                        | _ -> None)
                    match clientUser with
                    | Some clientUser ->
                        let! serverMsg = !+ $"%s{clientUser} (%s{clientEndPoint}) has disconnected."
                        let! clientMsg = !+ $"%s{clientUser} has left the chat. See you soon! 👋"
                        do! !<< (fun () -> clients.Remove clientUser |> ignore)
                        let! date = !+ DateTime.Now
                        do! printServerMsg date serverMsg
                        do! broadcastMsg None
                            <| DisconnectionNotify (server.Name, serverName, clientMsg, date)
                        do! clientSocket.Close ()
                    | None ->
                        do! printServerMsg DateTime.Now $"Bad connection attempt from %s{clientEndPoint}. Client has been disconnected."
                        do! clientSocket.Close ()
                }

            fio {
                let! clientEndPoint =
                    clientSocket.RemoteEndPoint ()
                    >>= fun endPoint -> !+ endPoint.ToString()
                try
                    while true do
                        let! msg = clientSocket.Receive ()
                        do! clearInputPrompt ()
                        do! handleMsg msg clientEndPoint
                        do! printInputPrompt server.Name
                with _ ->
                    do! handleDisconnect clientEndPoint
                do! printInputPrompt server.Name
            }

        let handleClients server =
            fio {
                while true do
                    let! clientSocket = server.Socket.Accept ()
                    do! !!<~ (handleClient clientSocket server)
            }

        fio {
            do! FConsole.Clear ()
            let! serverSocket = FServerWebSocket<Message, Message, exn>.Create options
            let! server = !+ { Name = serverName; EndPoint = serverUrl; Socket = serverSocket }
            do! server.Socket.Start
                <| server.EndPoint
            let! date = !+ DateTime.Now
            do! printServerMsg server.Name server.EndPoint date $"Server started on %s{server.EndPoint}. Listening for clients..."
            do! printServerMsg server.Name server.EndPoint date $"Type \commands for a list of available commands."
            do! handleCommands server <~> handleClients server
        }

    override _.effect =
        fio {
            try
                do! run serverUrl
            with exn ->
                return! !- (Exception exn.Message)
        }
