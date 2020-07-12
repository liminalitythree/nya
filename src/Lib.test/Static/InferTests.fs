module InferTests

open System
open Xunit
open Swensen.Unquote
open Lib

[<Fact>]
let ``TypeGenerator works as expected`` () =
    let gen = Infer.TypeGenerator ()
    test <@ gen.Gen() = ("t^1" |> Type.Ident) @>
    test <@ gen.Gen() = ("t^2" |> Type.Ident) @>

