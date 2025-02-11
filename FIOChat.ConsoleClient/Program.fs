open FIOChat.Client

[<EntryPoint>]
let main args =
    if args.Length = 0 then
        eprintfn "No arguments were provided. Please provide server url and user!"
        exit 1
    ClientApp(serverUrl = args.[0], user = args.[1]).Run()
    exit 0
