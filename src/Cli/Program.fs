// Learn more about F# at http://fsharp.org

open System
open FParsec

open Lib

[<EntryPoint>]
let main argv =
    ReadLine.HistoryEnabled <- true

    let env = ref Map.empty<string, Lib.Type.T>

    env := (!env).Add("+", Type.Lambda (Type.Num, (Type.Lambda (Type.Num, Type.Num))))

    // ! btw the repl is broken
    // ! ex
    // ! > let f =\x.(let a = x + 2; a)
    // ! > f 2
    // ! > num
    // ! f "hi"
    // ! string (this should be a type error)
    // ! but it works if you do it all in one input
    // ! ex
    // ! > (let f = \x.(let a = x + 2; a); f "hi")
    // ! type error
    while true do
        let input = ReadLine.Read("~> ")
        let res = Lib.Parser.parse input
        match res with
        | Success(result,_,_) ->
            let annotated = Lib.Infer.infer env result
            printfn "%s" (Lib.Type.toString (Lib.Infer.typeOfAExpr annotated))
        | Failure(errorMsg,_,_) -> printfn "Parse Error: %s" errorMsg

    0 // return an integer exit code
