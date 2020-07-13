module InferTests

open System
open Xunit
open Swensen.Unquote
open Lib
open Lib.Infer

let num = 2.0 |> Number |> Atom
let str = "hi" |> String |> Atom
let boolean = true |> Bool |> Atom

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

    test <@ infer num = (2.0 |> Number |> annotate Type.Num |> AAtom) @>
    test <@ infer str = ("hi" |> String |> annotate Type.String |> AAtom) @>
    test <@ infer boolean = (true |> Bool |> annotate Type.Bool |> AAtom) @>

[<Fact>]
let ``Infers type of identifiers from env`` () =
    let env = ref (Map.empty.Add("abc", Type.Num))
    let ident = "abc" |> Identifier |> Atom

    test <@ infer env ident = ("abc" |> Identifier |> annotate Type.Num |> AAtom) @>

