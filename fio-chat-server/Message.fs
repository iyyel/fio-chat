module ChatClient.Message

type Message =
    | ConnectionRequest of RequestedUsername: string
    | ConnectionAcceptedResponse of ServerName: string * AcceptedUsername: string * Message: string
    | ConnectionFailedResponse of ServerName: string * FailedUsername: string * Message: string
    | NewConnectionResponse of ServerName: string * NewUsername: string * Message: string

    | BroadcastMessageRequest of FromUsername: string * Message: string
    | BroadcastMessageResponse of ServerName: string * FromUsername: string * Message: string

    | PrivateMessageRequest of FromUsername: string * ToUsername: string * Message: string
    | PrivateMessageResponse of ServerName: string * FromUsername: string * ToUsername: string * Message: string
    | PrivateMessageFailedResponse of ServerName: string * FromUsername: string * ToUsername: string * Message: string

    | OnlineClientsRequest of FromUsername: string
    | OnlineClientsResponse of ServerName: string * ToUsername: string * Clients: string list

    | HelpRequest of FromUsername: string
    | HelpResponse of ServerName: string * ToUsername: string * Message: string
