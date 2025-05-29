open FIOChat.Server

[<EntryPoint>]
let main args =
    if args.Length = 0 then
        eprintfn "No arguments were provided. Please provide server url and server name!"
        exit 1
    ServerApp(serverUrl = args.[0], serverName = args.[1]).Run()
    exit 0
