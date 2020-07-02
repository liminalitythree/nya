// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    ReadLine.HistoryEnabled <- true
    while true do
        let input = ReadLine.Read("~> ")
        let res = Lib.Lexer.scan (Lib.Lexer.defaultT input "<repl>")
        printfn "%A" ( res.Tokens |> List.map (fun x -> x.T) ) 
    0 // return an integer exit code
