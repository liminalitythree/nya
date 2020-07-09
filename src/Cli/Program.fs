// Learn more about F# at http://fsharp.org

open System
open FParsec

open Lib

[<EntryPoint>]
let main argv =
    ReadLine.HistoryEnabled <- true

    let env = ref Map.empty<string, Lib.Type.T>

    env := (!env).Add("+", Type.Lambda (Type.Num, (Type.Lambda (Type.Num, Type.Num))))

    while true do
        let input = ReadLine.Read("~> ")
        let res = Lib.Parser.parse input
        match res with
        | Success(result,_,_) ->
            let annotated = Lib.Infer.infer env result
            printfn "%s" (Lib.Type.toString (Lib.Infer.typeOfAExpr annotated))
        | Failure(errorMsg,_,_) -> printfn "Parse Error: %s" errorMsg

    0 // return an integer exit code
