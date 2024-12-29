open FIOChat.ServerApp

[<EntryPoint>]
let main args =
    if args.Length = 0 then
        eprintfn "No arguments were provided. Please provide server url and server name!"
        exit 1
    ServerApp(args.[0], args.[1]).Run()
    exit 0
