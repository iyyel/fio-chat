module FIOChat.Common

open System

type Message =
    | ConnectionRequest of user: string * date: DateTime
    | ConnectionAcceptedResponse of server: string * user: string * msg: string * date: DateTime
    | ConnectionFailedResponse of server: string * user: string * msg: string * date: DateTime

    | ConnectionNotify of server: string * user: string * msg: string * date: DateTime
    | DisconnectionNotify of server: string * user: string * msg: string * date: DateTime

    | BroadcastMessageRequest of fromUser: string * msg: string * date: DateTime
    | BroadcastMessageResponse of server: string * fromUser: string * msg: string * date: DateTime
    | ServerBroadcastMessageResponse of server: string * msg: string * date: DateTime

    | PrivateMessageRequest of fromUser: string * toUser: string * msg: string * date: DateTime
    | PrivateMessageResponse of server: string * fromUser: string * toUser: string * msg: string * date: DateTime
    | PrivateMessageFailedResponse of server: string * fromUser: string * toUser: string * msg: string * date: DateTime

    | OnlineClientsRequest of fromUser: string * date: DateTime
    | OnlineClientsResponse of server: string * toUser: string * clients: string list * date: DateTime

    | HelpRequest of fromUser: string * date: DateTime
    | HelpResponse of server: string * toUser: string * msg: string * date: DateTime

    | KickedResponse of server: string * toUser: string * msg: string * date: DateTime
