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
        | Success(result,_,_) ->
            let env = ref Map.empty<string, Lib.Type.T>
            let annotated = Lib.Infer.infer env result
            printfn "%A" (Lib.Type.toString (Lib.Infer.typeOfAExpr annotated))
        | Failure(errorMsg,_,_) -> printfn "Parse Error: %s" errorMsg

    0 // return an integer exit code
