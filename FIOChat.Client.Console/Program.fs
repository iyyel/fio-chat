open FIOChat.Client.Console

[<EntryPoint>]
let main args =
    if args.Length = 0 then
        eprintfn "No arguments were provided. Please provide server url and username!"
        exit 1
    ClientApp(args.[0], args.[1]).Run()
    exit 0
