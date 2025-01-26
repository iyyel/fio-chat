module FIOChat.Server.Printing

open System
open System.Globalization

open BlackFox.ColoredPrintf

open FIO.Core

let private formatDate (date: DateTime) =
    date.ToString("dd.MM.yy HH:mm:ss", CultureInfo.InvariantCulture)

let printServerMsg user url date msg =
    !+ (colorprintfn "$darkblue[[%s]$darkblue[\]] [$darkmagenta[%s] ($darkgreen[%s])]: $darkmagenta[%s]" (formatDate date) user url msg)

let printClientMsg user url date msg =
    !+ (colorprintfn "$darkblue[[%s]$darkblue[\]] [$darkyellow[%s] ($darkgreen[%s])]: $gray[%s]" (formatDate date) user url msg)

let printPrivateMsg user url date msg =
    !+ (colorprintfn "$darkblue[[%s]$darkblue[\]] [$darkcyan[%s] ($darkgreen[%s])]: $gray[%s]" (formatDate date) user url msg)
    
let printInputPrompt user =
    !+ (colorprintf "$darkblue[[%s]$darkblue[\]] [$darkmagenta[%s]]: " (formatDate DateTime.Now) user)

let clearInputPrompt () =
    !+ Console.Write("\r" + new string(' ', 100) + "\r")

let clearConsole () =
    !+ Console.Clear()
