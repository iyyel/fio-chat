namespace FIOChat.Client

open System

open FIOChat.Shared
open FIOChat.Client.Printing

open FIO.Core
open FIO.Library.Network.WebSockets

type ClientApp(serverUrl: string, username: string) =
    inherit FIOApp<unit, string>()

    let clientName = "FIOChatClient"

    let runClient serverUrl username =
        let send (clientSocket: ClientWebSocket<Message>) = fio {
            let error = "Connection to server was lost!"
            while true do
                do! printInputPrompt(username)
                match Console.ReadLine() with
                | input when input.Trim().Length = 0 ->
                    return ()
                | input when input.Trim().StartsWith("\pm@") ->
                    let parts = (input.Trim().Split("@")[1]).Split(":")
                    let toUser = parts.[0].Trim()
                    let message = parts.[1].Trim()
                    do! clientSocket.Send
                        <| PrivateMessageRequest(username, toUser, message, DateTime.Now)
                        >? !- error
                | input when input.Trim().StartsWith("\online") ->
                    do! clientSocket.Send
                        <| OnlineClientsRequest(username, DateTime.Now)
                        >? !- error
                | input when input.Trim().StartsWith("\help") ->
                    do! clientSocket.Send
                        <| HelpRequest(username, DateTime.Now)
                        >? !- error
                | input ->
                    do! clientSocket.Send
                        <| BroadcastMessageRequest(username, input, DateTime.Now)
                        >? !- error
        }

        let receive (clientSocket: ClientWebSocket<Message>) =
            let handleMessage message = fio {
                let printMessageWithClientName = printClientMessage clientName
                match message with
                | ConnectionRequest (user, timestamp) ->
                    do! printMessageWithClientName timestamp $"Received ConnectionRequest with User: %s{user}. Discarding."
                | ConnectionAcceptedResponse(server, user, message, timestamp) ->
                    match user = user with
                    | true -> do! printServerMessage server timestamp message
                    | _ -> do! printMessageWithClientName timestamp $"Received ConnectionAcceptedResponse with Server: %s{server}, User: %s{user} and Message: %s{message}. Discarding."
                | ConnectionFailedResponse(server, user, message, timestamp) ->
                    match user = user with
                    | true -> return! !- message
                    | _ -> do! printMessageWithClientName timestamp $"Received ConnectionFailedResponse with Server: %s{server}, User: %s{user} and Message: %s{message}. Discarding."
                | ConnectionNotify(server, _, message, timestamp) ->
                    do! printServerMessage server timestamp message
                | DisconnectionNotify(server, _, message, timestamp) ->
                    do! printServerMessage server timestamp message
                | BroadcastMessageRequest(fromUser, message, timestamp) ->
                    do! printMessageWithClientName timestamp $"Received BroadcastMessageRequest with FromUser: %s{fromUser} and Message: %s{message}. Discarding."
                | BroadcastMessageResponse(_, fromUser, message, timestamp) ->
                    do! printClientMessage fromUser timestamp message
                | ServerBroadcastMessageResponse(server, message, timestamp) ->
                    do! printServerMessage server timestamp message
                | PrivateMessageRequest(fromUser, toUser, message, timestamp) -> 
                    do! printMessageWithClientName timestamp $"Received PrivateMessageRequest with FromUser: %s{fromUser}, ToUser: %s{toUser} and Message: %s{message}. Discarding."
                | PrivateMessageResponse(_, fromUser, _, message, timestamp) ->
                    do! printPrivateMessage $"PM: %s{fromUser}" timestamp message
                | PrivateMessageFailedResponse(server, _, _, message, timestamp) ->
                    do! printServerMessage server timestamp message
                | OnlineClientsRequest(fromUser, timestamp) ->
                    do! printMessageWithClientName timestamp $"Received OnlineClientsRequest with FromUser: %s{fromUser}. Discarding."
                | OnlineClientsResponse(server, _, clientList, timestamp) ->
                    do! printServerMessage server timestamp $"""Online: {String.Join(", ", clientList)}."""
                | HelpRequest(fromUser, timestamp) ->
                    do! printMessageWithClientName timestamp $"Received HelpRequest with FromUser: %s{fromUser}. Discarding."
                | HelpResponse(server, _, message, timestamp) ->
                    do! printServerMessage server timestamp message
                | KickedResponse(server, _, message, timestamp) ->
                    do! printServerMessage server timestamp message
                | BannedResponse(server, _, message, timestamp) ->
                    do! printServerMessage server timestamp message
            }
        
            fio {
                while true do
                    let! message = clientSocket.Receive()
                                   >? !- "Connection to server was lost!"
                    do! clearInputPrompt()
                    do! handleMessage message
                    do! printInputPrompt username
            }

        let connect (clientSocket: ClientWebSocket<Message>) = fio {
            do! clientSocket.Connect serverUrl
                >? !- "Failed to connect to server!"
            do! clientSocket.Send
                <| ConnectionRequest (username, DateTime.Now)
                >? !- "Failed to send connection request!"
            let! connectionResponse =
                clientSocket.Receive()
                >? !- "Failed to receive connection response!"
            match connectionResponse with
            | ConnectionAcceptedResponse(_, user, _, _) ->
                if user <> username then
                    return! !- "Invalid connection response. Incorrect user!"
            | ConnectionFailedResponse(_, _, message, _) ->
                return! !- message
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
        do! runClient serverUrl username
    }
