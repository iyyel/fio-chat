module FIOChat.Client.Printing

open System
open System.Globalization

open BlackFox.ColoredPrintf

open FIO.Core

let private formatDate (date: DateTime) =
    date.ToString("HH:mm:ss", CultureInfo.InvariantCulture)

let printServerMsg user date msg =
    !+ (colorprintfn "$darkblue[[%s]$darkblue[\]] [$darkmagenta[%s]]: $darkmagenta[%s]" (formatDate date) user msg)

let printClientMsg user date msg =
    !+ (colorprintfn "$darkblue[[%s]$darkblue[\]] [$darkyellow[%s]]: $gray[%s]" (formatDate date) user msg)

let printPrivateMsg user date msg =
    !+ (colorprintfn "$darkblue[[%s]$darkblue[\]] [$darkcyan[%s]]: $gray[%s]" (formatDate date) user msg)
    
let printInputPrompt user =
    !+ (colorprintf "$darkblue[[%s]$darkblue[\]] [$darkgreen[%s]]: " (formatDate DateTime.Now) user)

let clearInputPrompt () =
    !+ Console.Write("\r" + new string(' ', 100) + "\r")

let clearConsole () =
    !+ Console.Clear()