module Cli

open System

open Lib
open Argu
open Args

// prints the string s as a rainbow maybe
let printAsRainbow (s: string) =
    let chars = s.ToCharArray() |> Seq.chunkBySize 14
    for c in chars do
        Colorful.Console.WriteWithGradient(c, Drawing.Color.Yellow, Drawing.Color.Fuchsia, 14)
    Colorful.Console.ReplaceAllColorsWithDefaults()

let doCompile (args: ParseResults<NyaArgs>): unit =
    let sourcePath =
        ((args.GetResult Compile).GetResult File)

    let source =
        IO.File.ReadAllText(sourcePath, Text.Encoding.UTF8)

    let env = ref Map.empty<string, Type.T>

    let res = Compile.compile source env

    match res with
    | Errors.NyaResult.Ok ok -> sprintf "%A" ok |> printAsRainbow
    | Errors.NyaResult.Error err ->
        err
        |> Errors.prettyPrintErr source
        |> printAsRainbow
    ()


[<EntryPoint>]
let main argv =
    let argParser =
        ArgumentParser.Create<NyaArgs>(programName = "nya")

    let usage = argParser.PrintUsage()

    try
        let res =
            argParser.ParseCommandLine(inputs = argv, raiseOnUsage = true)

        if res.Contains Compile then doCompile res else printAsRainbow usage
    with e -> printAsRainbow e.Message

    // exit code maybe
    0
    (*    ReadLine.HistoryEnabled <- true

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
    repl incr *)
