namespace FIOChat.Client

open System

open FIOChat.Shared
open FIOChat.Client.Printing

open FIO.Core
open FIO.Library.Network.WebSockets

type ClientApp(serverUrl: string, user: string) =
    inherit FIOApp<unit, string>()

    let clientName = "FIOChatClient"

    let run serverUrl user =
        let send (clientSocket: ClientWebSocket<Message>) = fio {
            let err = "Connection to server was lost!"
            while true do
                do! printInputPrompt user
                match Console.ReadLine() with
                | input when input.Trim().Length = 0 ->
                    return ()
                | input when input.Trim().StartsWith("\pm@") ->
                    let parts = (input.Trim().Split("@")[1]).Split(":")
                    let toUser = parts.[0].Trim()
                    let msg = parts.[1].Trim()
                    do! clientSocket.Send
                        <| PrivateMessageRequest (user, toUser, msg, DateTime.Now)
                        >? !- err
                | input when input.Trim().StartsWith("\online") ->
                    do! clientSocket.Send
                        <| OnlineClientsRequest (user, DateTime.Now)
                        >? !- err
                | input when input.Trim().StartsWith("\help") ->
                    do! clientSocket.Send
                        <| HelpRequest (user, DateTime.Now)
                        >? !- err
                | input ->
                    do! clientSocket.Send
                        <| BroadcastMessageRequest (user, input, DateTime.Now)
                        >? !- err
        }

        let receive (clientSocket: ClientWebSocket<Message>) =
            let handleMsg msg = fio {
                let printClientNameMsg = printClientMsg clientName
                match msg with
                | ConnectionRequest (user, date) ->
                    do! printClientNameMsg date $"Received ConnectionRequest with User: %s{user}. Discarding."
                | ConnectionAcceptedResponse (server, acceptedUser, msg, date) ->
                    match user = acceptedUser with
                    | true -> do! printServerMsg server date msg
                    | _ -> do! printClientNameMsg date $"Received ConnectionAcceptedResponse with Server: %s{server}, User: %s{acceptedUser} and Message: %s{msg}. Discarding."
                | ConnectionFailedResponse (server, failedUser, msg, date) ->
                    match user = failedUser with
                    | true -> return! !- msg
                    | _ -> do! printClientNameMsg date $"Received ConnectionFailedResponse with Server: %s{server}, User: %s{failedUser} and Message: %s{msg}. Discarding."
                | ConnectionNotify (server, _, msg, date) ->
                    do! printServerMsg server date msg
                | DisconnectionNotify (server, _, msg, date) ->
                    do! printServerMsg server date msg
                | BroadcastMessageRequest (fromUser, msg, date) ->
                    do! printClientNameMsg date $"Received BroadcastMessageRequest with FromUser: %s{fromUser} and Message: %s{msg}. Discarding."
                | BroadcastMessageResponse (_, fromUser, msg, date) ->
                    do! printClientMsg fromUser date msg
                | ServerBroadcastMessageResponse (server, msg, date) ->
                    do! printServerMsg server date msg
                | PrivateMessageRequest (fromUser, toUser, msg, date) -> 
                    do! printClientNameMsg date $"Received PrivateMessageRequest with FromUser: %s{fromUser}, ToUser: %s{toUser} and Message: %s{msg}. Discarding."
                | PrivateMessageResponse (_, fromUser, _, msg, date) ->
                    do! printPrivateMsg $"PM: %s{fromUser}" date msg
                | PrivateMessageFailedResponse(server, _, _, msg, date) ->
                    do! printServerMsg server date msg
                | OnlineClientsRequest (fromUser, date) ->
                    do! printClientNameMsg date $"Received OnlineClientsRequest with FromUser: %s{fromUser}. Discarding."
                | OnlineClientsResponse (server, _, clients, date) ->
                    do! printServerMsg server date $"""Online: {String.Join(", ", clients)}."""
                | HelpRequest (fromUser, date) ->
                    do! printClientNameMsg date $"Received HelpRequest with FromUser: %s{fromUser}. Discarding."
                | HelpResponse (server, _, msg, date) ->
                    do! printServerMsg server date msg
                | KickedResponse (server, _, msg, date) ->
                    do! printServerMsg server date msg
                | BannedResponse (server, _, msg, date) ->
                    do! printServerMsg server date msg
            }
        
            fio {
                while true do
                    let! msg =
                        clientSocket.Receive()
                        >? !- "Connection to server was lost!"
                    do! clearInputPrompt()
                    do! handleMsg msg
                    do! printInputPrompt user
            }

        let connect (clientSocket: ClientWebSocket<Message>) = fio {
            do! clientSocket.Connect serverUrl
                >? !- "Failed to connect to server!"
            do! clientSocket.Send
                <| ConnectionRequest (user, DateTime.Now)
                >? !- "Failed to send connection request!"
            let! response =
                clientSocket.Receive()
                >? !- "Failed to receive connection response!"
            match response with
            | ConnectionAcceptedResponse(_, acceptedUser, _, _) ->
                if user <> acceptedUser then
                    return! !- "Invalid connection response. Incorrect user!"
            | ConnectionFailedResponse(_, _, msg, _) ->
                return! !- msg
            | _ -> 
                return! !- "Invalid connection response!"
        }

        fio {
            do! clearConsole()
            let! clientSocket = !+ ClientWebSocket<Message>()
            do! connect clientSocket
            do! receive clientSocket <!> send clientSocket
        }

    override this.effect = fio {
        do! run serverUrl user
    }
