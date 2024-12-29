open FIOChat.Client.Console

[<EntryPoint>]
let main args =
    if args.Length = 0 then
        printfn "No arguments were provided. Please provide server url and username!"
        exit 1
    let serverUrl = args.[0]
    let username = args.[1]
    ClientApp(serverUrl, username).Run()
    exit 0
