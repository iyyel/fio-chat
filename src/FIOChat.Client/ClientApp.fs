namespace FIOChat.Client

open FSharp.FIO.DSL
open FSharp.FIO.App
open FSharp.FIO.Lib.IO
open FSharp.FIO.Lib.Net.WebSockets

open FIOChat.Common
open FIOChat.Client.Printing

open System
open System.Text.Json.Serialization

type ClientApp (serverUrl: string, user: string) =
    inherit FIOApp<unit, exn> ()

    let clientName = "FIOChatClient"
    let options = JsonFSharpOptions.Default().ToJsonSerializerOptions()

    let run serverUrl user =

        let send (clientSocket: FClientWebSocket<Message, Message, exn>) =
            fio {
                while true do
                    try
                        do! printInputPrompt user
                        match! FConsole.ReadLine () with
                        | input when input.Trim().Length = 0 ->
                            return ()
                        | input when input.Trim().StartsWith "\pm@" ->
                            let parts = (input.Trim().Split("@")[1]).Split ":"
                            let toUser = parts[0].Trim()
                            let msg = parts[1].Trim()
                            do! clientSocket.Send
                                <| PrivateMessageRequest (user, toUser, msg, DateTime.Now)
                        | input when input.Trim().StartsWith "\online" ->
                            do! clientSocket.Send
                                <| OnlineClientsRequest (user, DateTime.Now)
                        | input when input.Trim().StartsWith "\help" ->
                            do! clientSocket.Send
                                <| HelpRequest (user, DateTime.Now)
                        | input ->
                            do! clientSocket.Send
                                <| BroadcastMessageRequest (user, input, DateTime.Now)
                    with exn ->
                        return! !- (Exception exn.Message)
            }

        let receive (clientSocket: FClientWebSocket<Message, Message, exn>) =

            let handleMsg msg =
                let printClientNameMsg = printClientMsg clientName
                fio {
                    match msg with
                    | ConnectionRequest (user, date) ->
                        do! printClientNameMsg date
                            <| $"Received ConnectionRequest with User: %s{user}. Discarding."
                    | ConnectionAcceptedResponse (server, acceptedUser, msg, date) ->
                        match user = acceptedUser with
                        | true -> 
                            do! printServerMsg server date msg
                        | _ -> 
                            do! printClientNameMsg date
                                <| $"Received ConnectionAcceptedResponse with Server: %s{server}, User: %s{acceptedUser} and Message: %s{msg}. Discarding."
                    | ConnectionFailedResponse (server, failedUser, msg, date) ->
                        match user = failedUser with
                        | true ->
                            return! !- (Exception msg)
                        | _ ->
                            do! printClientNameMsg date
                                <| $"Received ConnectionFailedResponse with Server: %s{server}, User: %s{failedUser} and Message: %s{msg}. Discarding."
                    | ConnectionNotify (server, _, msg, date) ->
                        do! printServerMsg server date msg
                    | DisconnectionNotify (server, _, msg, date) ->
                        do! printServerMsg server date msg
                    | BroadcastMessageRequest (fromUser, msg, date) ->
                        do! printClientNameMsg date
                            <| $"Received BroadcastMessageRequest with FromUser: %s{fromUser} and Message: %s{msg}. Discarding."
                    | BroadcastMessageResponse (_, fromUser, msg, date) ->
                        do! printClientMsg fromUser date msg
                    | ServerBroadcastMessageResponse (server, msg, date) ->
                        do! printServerMsg server date msg
                    | PrivateMessageRequest (fromUser, toUser, msg, date) -> 
                        do! printClientNameMsg date
                            <| $"Received PrivateMessageRequest with FromUser: %s{fromUser}, ToUser: %s{toUser} and Message: %s{msg}. Discarding."
                    | PrivateMessageResponse (_, fromUser, _, msg, date) ->
                        do! printPrivateMsg $"PM: %s{fromUser}" date msg
                    | PrivateMessageFailedResponse(server, _, _, msg, date) ->
                        do! printServerMsg server date msg
                    | OnlineClientsRequest (fromUser, date) ->
                        do! printClientNameMsg date
                            <| $"Received OnlineClientsRequest with FromUser: %s{fromUser}. Discarding."
                    | OnlineClientsResponse (server, _, clients, date) ->
                        let! msg = !<< (fun () -> $"""Online: {String.Join(", ", clients)}.""")
                        do! printServerMsg server date msg
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
                    try
                        let! msg = clientSocket.Receive ()
                        do! clearInputPrompt ()
                        do! handleMsg msg
                        do! printInputPrompt user
                    with exn ->
                        return! !- (Exception exn.Message)
            }

        let connect (clientSocket: FClientWebSocket<Message, Message, exn>) =
            fio {
                do! clientSocket.Connect serverUrl
                do! clientSocket.Send
                    <| ConnectionRequest (user, DateTime.Now)
                match! clientSocket.Receive () with
                | ConnectionAcceptedResponse (_, acceptedUser, _, _) ->
                    if user <> acceptedUser then
                        return! !- (Exception "Invalid connection accepted response. Incorrect user!")
                | ConnectionFailedResponse (_, _, msg, _) ->
                    return! !- (Exception msg)
                | _ -> 
                    return! !- (Exception "Invalid connection response!")
            }

        fio {
            do! FConsole.Clear ()
            let! clientSocket = FClientWebSocket<Message, Message, exn>.Create options
            do! connect clientSocket
            do! receive clientSocket <~> send clientSocket
        }

    override _.effect =
        fio {
            do! run serverUrl user
        }
