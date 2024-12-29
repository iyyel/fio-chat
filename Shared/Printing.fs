module FIO.Chat.Shared.Printing

open FIO.Core

open BlackFox.ColoredPrintf

open System
open System.Globalization

module Server =

    let private formatDate (date: DateTime) =
        date.ToString("dd.MM.yy HH:mm:ss", CultureInfo.InvariantCulture)

    let printServerMessage user url date message =
        !+ (colorprintfn "$darkblue[[%s]$darkblue[\]] [$darkmagenta[%s] ($darkgreen[%s])]: $darkmagenta[%s]" (formatDate date) user url message)

    let printClientMessage user url date message =
        !+ (colorprintfn "$darkblue[[%s]$darkblue[\]] [$darkyellow[%s] ($darkgreen[%s])]: $gray[%s]" (formatDate date) user url message)

    let printPrivateMessage user url date message =
        !+ (colorprintfn "$darkblue[[%s]$darkblue[\]] [$darkcyan[%s] ($darkgreen[%s])]: $gray[%s]" (formatDate date) user url message)
    
    let printInputPrompt user =
        !+ (colorprintf "$darkblue[[%s]$darkblue[\]] [$darkmagenta[%s]]: " (formatDate <| DateTime.Now) user)

    let clearInputPrompt () =
        !+ Console.Write("\r" + new string(' ', 100) + "\r")

    let clearConsole () =
        !+ Console.Clear()

module Client =

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
