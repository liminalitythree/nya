module UtilTests

open System
open Xunit
open Xunit.Abstractions
open FsUnit.Xunit
open Lib
open Lib.Infer

type CurryTests(output: ITestOutputHelper) =
    let args: Infer.A<string> list =
        seq { 1 .. 3 }
        |> Seq.fold (fun e x ->
            e
            @ [ (("arg" + x.ToString()) |> annotate Type.Num) ]) []

    let expr =
        2.0 |> Number |> annotate Type.Num |> AAtom

    [<Fact>]
    member __.``Util.curry works``() =
        let res = Util.curry args expr Type.Num

        let arg1 = annotate Type.Num "arg1"
        let arg2 = annotate Type.Num "arg2"
        let arg3 = annotate Type.Num "arg3"

        let lambdaNum = annotate Type.Num >> ALambda

        let expected =
            (arg1, (arg2, (arg3, expr) |> lambdaNum) |> lambdaNum)
            |> lambdaNum

        expected |> should equal res

    [<Fact>]
    member __.``Curry and Uncurry``() =
        let curried = Util.curry args expr Type.Num
        let unCurried = Util.unCurry curried

        unCurried |> should equal (args, expr)

    [<Fact>]
    member __.``Uncurry returns correct amount of args when there is only 1 arg``() =
        let curried =
            (("x" |> annotate Type.Num), "x" |> Identifier |> annotate Type.Num |> AAtom)
            |> annotate (Type.Lambda(Type.Num, Type.Num))
            |> ALambda

        let args, _ = Util.unCurry curried

        args.Length |> should equal 1
        args.[0]
        |> should equal ("x" |> annotate Type.Num)
