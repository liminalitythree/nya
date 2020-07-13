// Learn more about F# at http://fsharp.org

open System
open FParsec

open Lib

[<EntryPoint>]
let main argv =
    // Codegen.test ()
    ReadLine.HistoryEnabled <- true

    let env = ref Map.empty<string, Lib.Type.T>

    env := (!env).Add("+", Type.Lambda (Type.Num, (Type.Lambda (Type.Num, Type.Num))))

    let incr = Infer.incrementalFromEnv env

    let rec repl inc =
        let input = ReadLine.Read("~> ")
        let res = Parser.parse input
        match res with
        | Success(result,_,_) ->
            printfn "%A" result
            let (annotated,inc) = Infer.incrementalInfer inc result
            printfn "%s" (Type.toString (Infer.typeOfAExpr annotated))

            let gen = Lib.Misc.IdGen "nyaref^"

            let uniqueNames = Transform.transformUniqueNames (ref Map.empty<string,string>) gen annotated

            let lgen = Lib.Misc.IdGen "l^"
            let lifted,lmap = LambdaLift.lambdaLift lgen uniqueNames

            printfn "Lifted Lambdas: %A" lmap
            printfn "result: %A" lifted

            repl inc
        | Failure(errorMsg,_,_) ->
            printfn "Parse Error: %s" errorMsg
            repl inc
    repl incr

    0 // return an integer exit code
