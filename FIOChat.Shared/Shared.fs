module FIOChat.Shared

open System

type Message =
    | ConnectionRequest of Username: string * Timestamp: DateTime
    | ConnectionAcceptedResponse of Server: string * Username: string * Message: string * Timestamp: DateTime
    | ConnectionFailedResponse of Server: string * Username: string * Message: string * Timestamp: DateTime

    | ConnectionNotify of Server: string * Username: string * Message: string * Timestamp: DateTime
    | DisconnectionNotify of Server: string * Username: string * Message: string * Timestamp: DateTime

    | BroadcastMessageRequest of FromUsername: string * Message: string * Timestamp: DateTime
    | BroadcastMessageResponse of Server: string * FromUsername: string * Message: string * Timestamp: DateTime
    | ServerBroadcastMessageResponse of Server: string * Message: string * Timestamp: DateTime

    | PrivateMessageRequest of FromUsername: string * ToUsername: string * Message: string * Timestamp: DateTime
    | PrivateMessageResponse of Server: string * FromUsername: string * ToUsername: string * Message: string * Timestamp: DateTime
    | PrivateMessageFailedResponse of Server: string * FromUsername: string * ToUsername: string * Message: string * Timestamp: DateTime

    | OnlineClientsRequest of FromUsername: string * Timestamp: DateTime
    | OnlineClientsResponse of Server: string * ToUsername: string * Clients: string list * Timestamp: DateTime

    | HelpRequest of FromUsername: string * Timestamp: DateTime
    | HelpResponse of Server: string * ToUsername: string * Message: string * Timestamp: DateTime

    | KickedResponse of Server: string * ToUsername: string * Message: string * Timestamp: DateTime
    | BannedResponse of Server: string * ToUsername: string * Message: string * Timestamp: DateTime
