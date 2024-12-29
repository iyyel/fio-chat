module FIOChat.Client.Console.Printing

open FIO.Core

open BlackFox.ColoredPrintf

open System
open System.Globalization

let private formatDate (date: DateTime) =
    date.ToString("HH:mm:ss", CultureInfo.InvariantCulture)

let printServerMessage user date message =
    !+ (colorprintfn "$darkblue[[%s]$darkblue[\]] [$darkmagenta[%s]]: $darkmagenta[%s]" (formatDate date) user message)

let printClientMessage user date message =
    !+ (colorprintfn "$darkblue[[%s]$darkblue[\]] [$darkyellow[%s]]: $gray[%s]" (formatDate date) user message)

let printPrivateMessage user date message =
    !+ (colorprintfn "$darkblue[[%s]$darkblue[\]] [$darkcyan[%s]]: $gray[%s]" (formatDate date) user message)
    
let printInputPrompt user =
    !+ (colorprintf "$darkblue[[%s]$darkblue[\]] [$darkgreen[%s]]: " (formatDate DateTime.Now) user)

let clearInputPrompt () =
    !+ Console.Write("\r" + new string(' ', 100) + "\r")

let clearConsole () =
    !+ Console.Clear()