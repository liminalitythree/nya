module InferTests

open System
open Xunit
open Swensen.Unquote
open Lib
open Lib.Infer

let emptyPos =
    let pos1 = FParsec.Position("", 1L, 1L, 1L)
    Errors.Pos(pos1, pos1)

let withPos x = (x, emptyPos)

let num = 2.0 |> Number |> withPos |> Atom
let str = "hi" |> String |> withPos |> Atom
let boolean = true |> Bool |> withPos |> Atom

let mustOk =
    function
    | Errors.NyaResult.Ok ok -> ok
    | Errors.NyaResult.Error err -> failwithf "%A" err

let infer x y = infer x y |> mustOk

[<Fact>]
let ``TypeGenerator works as expected`` () =
    let gen = TypeGenerator()
    test <@ gen.Gen() = ("t^1" |> Type.Ident) @>
    test <@ gen.Gen() = ("t^2" |> Type.Ident) @>

[<Fact>]
let ``annotate function works as expected`` () =
    let expected = { Infer.E = num; Infer.T = Type.Num }
    test <@ (num |> annotate Type.Num) = expected @>

[<Fact>]
let ``Infers type of literals`` () =
    let env = ref Map.empty<string, Type.T>
    let infer = infer env

    test
        <@ infer num =
            (2.0
             |> Number
             |> annotate Type.Num
             |> withPos
             |> AAtom) @>
    test
        <@ infer str =
            ("hi"
             |> String
             |> annotate Type.String
             |> withPos
             |> AAtom) @>
    test
        <@ infer boolean =
            (true
             |> Bool
             |> annotate Type.Bool
             |> withPos
             |> AAtom) @>

[<Fact>]
let ``Infers type of identifiers from env`` () =
    let env = ref (Map.empty.Add("abc", Type.Num))
    let ident = "abc" |> Identifier |> withPos |> Atom

    test
        <@ infer env ident =
            ("abc"
             |> Identifier
             |> annotate Type.Num
             |> withPos
             |> AAtom) @>
