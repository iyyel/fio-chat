module FIOChat.Server.Printing

open FIO.DSL
open FIO.Lib.IO

open BlackFox.ColoredPrintf

open System
open System.Globalization

let private formatDate (date: DateTime) =
    !<< (fun () -> date.ToString("dd.MM.yy HH:mm:ss", CultureInfo.InvariantCulture))

let printServerMsg user url date msg =
    fio {
        let! date = formatDate date
        return! !<< (fun () -> (colorprintfn "$darkblue[[%s]$darkblue[\]] [$darkmagenta[%s] ($darkgreen[%s])]: $darkmagenta[%s]" date user url msg))
    }

let printClientMsg user url date msg =
    fio {
        let! date = formatDate date
        return! !<< (fun () -> (colorprintfn "$darkblue[[%s]$darkblue[\]] [$darkyellow[%s] ($darkgreen[%s])]: $gray[%s]" date user url msg))
    }
    
let printPrivateMsg user url date msg =
    fio {
        let! date = formatDate date
        return! !<< (fun () -> (colorprintfn "$darkblue[[%s]$darkblue[\]] [$darkcyan[%s] ($darkgreen[%s])]: $gray[%s]" date user url msg))
    }

let printInputPrompt user =
    fio {
        let! date = formatDate DateTime.Now
        return! !<< (fun () -> (colorprintf "$darkblue[[%s]$darkblue[\]] [$darkmagenta[%s]]: " date user))
    }

let clearInputPrompt () =
    let str = ("\r" + new string(' ', 100) + "\r")
    FConsole.PrintLine $"%s{str}"

let clearConsole () =
    !<< (fun () -> Console.Clear())
