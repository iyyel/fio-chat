module FIOChat.Client.Printing

open FSharp.FIO.DSL
open FSharp.FIO.Lib.IO

open BlackFox.ColoredPrintf

open System
open System.Globalization

let private formatDate (date: DateTime) =
    !<< (fun () -> date.ToString("HH:mm:ss", CultureInfo.InvariantCulture))

let printServerMsg user date msg =
    fio {
        let! date = formatDate date
        return! !<< (fun () -> (colorprintfn "$darkblue[[%s]$darkblue[\]] [$darkmagenta[%s]]: $darkmagenta[%s]" date user msg))
    }

let printClientMsg user date msg =
    fio {
        let! date = formatDate date
        return! !<< (fun () -> (colorprintfn "$darkblue[[%s]$darkblue[\]] [$darkyellow[%s]]: $gray[%s]" date user msg))
    }

let printPrivateMsg user date msg =
    fio {
        let! date = formatDate date
        return! !<< (fun () -> (colorprintfn "$darkblue[[%s]$darkblue[\]] [$darkcyan[%s]]: $gray[%s]" date user msg))
    }
    
let printInputPrompt user =
    fio {
        let! date = formatDate DateTime.Now
        return! !<< (fun () -> (colorprintf "$darkblue[[%s]$darkblue[\]] [$darkgreen[%s]]: " date user))
    }

let clearInputPrompt () =
    let str = ("\r" + new string(' ', 100) + "\r")
    FConsole.PrintLine $"%s{str}"

let clearConsole () =
    !<< (fun () -> Console.Clear())
