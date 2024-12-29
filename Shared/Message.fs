module FIOChat.Shared.Message

open System

type Message =
    | ConnectionRequest of User: string * Timestamp: DateTime
    | ConnectionAcceptedResponse of Server: string * User: string * Message: string * Timestamp: DateTime
    | ConnectionFailedResponse of Server: string * User: string * Message: string * Timestamp: DateTime

    | ConnectionNotify of Server: string * User: string * Message: string * Timestamp: DateTime
    | DisconnectionNotify of Server: string * User: string * Message: string * Timestamp: DateTime

    | BroadcastMessageRequest of FromUser: string * Message: string * Timestamp: DateTime
    | BroadcastMessageResponse of Server: string * FromUser: string * Message: string * Timestamp: DateTime
    | ServerBroadcastMessageResponse of Server: string * Message: string * Timestamp: DateTime

    | PrivateMessageRequest of FromUser: string * ToUser: string * Message: string * Timestamp: DateTime
    | PrivateMessageResponse of Server: string * FromUser: string * ToUser: string * Message: string * Timestamp: DateTime
    | PrivateMessageFailedResponse of Server: string * FromUser: string * ToUser: string * Message: string * Timestamp: DateTime

    | OnlineClientsRequest of FromUser: string * Timestamp: DateTime
    | OnlineClientsResponse of Server: string * ToUser: string * Clients: string list * Timestamp: DateTime

    | HelpRequest of FromUser: string * Timestamp: DateTime
    | HelpResponse of Server: string * ToUser: string * Message: string * Timestamp: DateTime

    | KickedResponse of Server: string * ToUser: string * Message: string * Timestamp: DateTime
