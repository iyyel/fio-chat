module FIOChat.WebClient

open System

open Elmish
open Elmish.React
open Feliz
open Browser

open Shared

type Model = {
    Messages: string list
    MessageToSend: string
    WebSocket: Types.WebSocket option
}

type Msg =
    | SendMessage
    | ReceivedMessage of string
    | SetMessage of string
    | Connect
    | Connected of Types.WebSocket

let init () =
    { Messages = []
      MessageToSend = "" 
      WebSocket = None }, Cmd.ofMsg Connect

let update msg model =
    match msg with
    | SetMessage msg ->
        { model with MessageToSend = msg }, Cmd.none

    | SendMessage ->
        printfn $"Model: %A{model}"
        match model.WebSocket with
        | Some socket ->
            socket.send model.MessageToSend
        | None ->
            printfn $"No socket found"
        { model with Messages = model.Messages @ [model.MessageToSend] }, Cmd.none

    | ReceivedMessage msg ->
        { model with Messages = model.Messages @ [msg] }, Cmd.none

    | Connect ->
        let socket = WebSocket.Create("ws://178.128.250.33:1337/")

        socket.onopen <- (fun e ->
            printfn "%A" e
            socket.send (ConnectionRequest ("hey", DateTime.Now))
        )

        { model with WebSocket = Some socket }, Cmd.none

    | Connected socket ->
        { model with WebSocket = Some socket }, Cmd.none

let view (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        Html.h1 "FIOChat"
        Html.textarea [
            prop.placeholder ""
            prop.value (List.map (fun m -> m + "\n") model.Messages |> String.concat "")
            prop.rows 50  // Optional: Set rows
            prop.cols 100 // Optional: Set cols
        ]

        Html.input [
            prop.type' "text"
            prop.value model.MessageToSend
            prop.onChange (fun (value: string) ->
                SetMessage value |> dispatch
            )
        ]

        Html.button [
            prop.text "Send message"
            prop.onClick (fun _ -> SendMessage |> dispatch)
        ]
    ]

Program.mkProgram init update view
|> Program.withReactSynchronous "root"
|> Program.run