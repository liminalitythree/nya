module UtilTests

open System
open Xunit
open Xunit.Abstractions
open Lib
open Lib.Infer

type CurryTests(output: ITestOutputHelper) =

    [<Fact>]
    member __.``Util.curry works``() =
        let args: Infer.A<string> list =
            seq { 1 .. 3 }
            |> Seq.fold (fun e x ->
                e
                @ [ (("arg" + x.ToString()) |> Infer.annotate Type.Num) ]) []

        let expr =
            2.0
            |> Number
            |> Infer.annotate Type.Num
            |> Infer.AAtom

        let res = Util.curry args expr Type.Num
        output.WriteLine(sprintf "input: %A * %A" args expr)
        output.WriteLine(sprintf "result: %A" res)

        let arg1 = annotate Type.Num "arg1"
        let arg2 = annotate Type.Num "arg2"
        let arg3 = annotate Type.Num "arg3"

        let lambdaNum = annotate Type.Num >> ALambda

        let expected =
            (arg1, (arg2, (arg3, expr) |> lambdaNum) |> lambdaNum)
            |> lambdaNum

        output.WriteLine(sprintf "expected: %A" expected)

        let a = expected = res
        Assert.True(a)

    [<Fact>]
    member __.``Curry and Uncurry``() =
        let args: Infer.A<string> list =
            seq { 1 .. 3 }
            |> Seq.fold (fun e x ->
                e
                @ [ (("arg" + x.ToString()) |> Infer.annotate Type.Num) ]) []

        let expr =
            2.0
            |> Number
            |> Infer.annotate Type.Num
            |> Infer.AAtom

        output.WriteLine(sprintf "original: %A * %A" args expr)

        let curried = Util.curry args expr Type.Num
        output.WriteLine(sprintf "curried: %A" curried)

        let unCurried = Util.unCurry curried
        output.WriteLine(sprintf "unCurried: %A" unCurried)

        let a = unCurried = (args, expr)
        Assert.True(a)
