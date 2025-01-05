namespace FIOChat.Client.Terminal

open FIOChat.Shared.Message
open FIOChat.Client.Terminal.Printing

open System

open FIO.Core
open FIO.Library.Network.WebSockets

type ClientApp(serverUrl: string, username: string) =
    inherit FIOApp<unit, string>()

    let clientName = "FIOChatClient"

    let runClient serverUrl user =
        let send (clientSocket: ClientWebSocket<Message>) fromUser = fio {
            while true do
                try
                    do! printInputPrompt(user)
                    match Console.ReadLine() with
                    | input when input.Trim().Length = 0 ->
                        return ()
                    | input when input.Trim().StartsWith("\pm@") ->
                        let parts = input.Trim().Split("@")[1]
                        let info = parts.Split(":")
                        let toUser = info.[0].Trim()
                        let message = info.[1].Trim()
                        do! clientSocket.Send <| PrivateMessageRequest(fromUser, toUser, message, DateTime.Now)
                    | input when input.Trim().StartsWith("\online") ->
                        do! clientSocket.Send <| OnlineClientsRequest(fromUser, DateTime.Now)
                    | input when input.Trim().StartsWith("\help") ->
                        do! clientSocket.Send <| HelpRequest(fromUser, DateTime.Now)
                    | input ->
                        do! clientSocket.Send <| BroadcastMessageRequest(fromUser, input, DateTime.Now)
                with _ ->
                    return! !- (new Exception("Connection to server was lost!"))
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
                    | true -> return! !- (new Exception(message))
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
            }
        
            fio {
                while true do
                    let! message = clientSocket.Receive()
                                   >? (!- (new Exception("Connection to server was lost!")))
                    do! clearInputPrompt()
                    do! handleMessage message
                    do! printInputPrompt user
            }

        fio {
            do! clearConsole()
            let! clientSocket = !+ ClientWebSocket<Message>()
            do! clientSocket.Connect <| serverUrl
            do! clientSocket.Send <| ConnectionRequest (user, DateTime.Now)
            do! receive clientSocket <!> send clientSocket user
        }

    override this.effect = fio {
        do! runClient serverUrl username
            >>? fun exn -> !- exn.Message
    }