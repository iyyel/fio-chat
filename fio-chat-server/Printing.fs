module ChatClient.Printing

open FIO.Core

open BlackFox.ColoredPrintf

open System
open System.Globalization

let private formattedDate () =
    DateTime.Now.ToString("dd.MM.yy HH:mm:ss", CultureInfo.InvariantCulture)

let printServerMessage username url message =
    !+ (colorprintfn "$darkblue[[%s]$darkblue[\]] [$darkmagenta[%s] ($darkgreen[%s])]: $darkmagenta[%s]" (formattedDate()) username url message)

let printClientMessage username url message =
    !+ (colorprintfn "$darkblue[[%s]$darkblue[\]] [$darkyellow[%s] ($darkgreen[%s])]: $gray[%s]" (formattedDate()) username url message)

let printPrivateMessage username url message =
    !+ (colorprintfn "$darkblue[[%s]$darkblue[\]] [$darkcyan[%s] ($darkgreen[%s])]: $gray[%s]" (formattedDate()) username url message)
    
let printInputPrompt username =
    !+ (colorprintf "$darkblue[[%s]$darkblue[\]] [$darkgreen[%s]]: " (formattedDate()) username)

let clearInputPrompt () =
    !+ Console.Write("\r" + new string(' ', 100) + "\r")

let clearConsole () =
    !+ Console.Clear()
