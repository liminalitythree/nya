// Learn more about F# at http://fsharp.org

open System
open FParsec

[<EntryPoint>]
let main argv =
    ReadLine.HistoryEnabled <- true
    while true do
        let input = ReadLine.Read("~> ")
        let res = Lib.Parser.parse input
        match res with
        | Success(result,_,_) -> printfn "Success: %A" result
        | Failure(errorMsg,_,_) -> printfn "Failure: %s" errorMsg

    0 // return an integer exit code
