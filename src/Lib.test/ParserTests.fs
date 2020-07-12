module ParserTests

open System
open Xunit
open Swensen.Unquote
open FParsec
open Lib

let getSuccess res =
    match res with
    | Success (result, _, _) -> result
    | Failure (errorMsg, _, _) -> failwithf "Parsing failed: %s" errorMsg

let getParse = Parser.parse >> getSuccess

let parseFails str =
    match Parser.parse str with
    | Success (result, _, _) -> failwithf "Parsing succeeded (but it should have failed): %A" result
    | Failure (_) -> true

let ident = Identifier >> Atom
let num = Number >> Atom

[<Fact>]
let ``Can parse booleans`` () =
    test <@ getParse "true" = (true |> Bool |> Atom) @>
    test <@ getParse "false" = (false |> Bool |> Atom) @>

[<Fact>]
let ``Can parse decimal numbers`` () =
    test <@ getParse "0" = (0.0 |> Number |> Atom) @>
    test <@ getParse "0.0" = (0.0 |> Number |> Atom) @>
    test <@ getParse "1" = (1.0 |> Number |> Atom) @>
    test <@ getParse "1." = (1.0 |> Number |> Atom) @>
    test <@ getParse "12.34" = (12.34 |> Number |> Atom) @>

[<Fact>]
let ``Doesnt parse invalid decimal numbers`` () =
    test <@ parseFails ".0" @>
    test <@ parseFails ".123" @>
    test <@ parseFails "1.2.3" @>

[<Fact>]
let ``Can parse identifiers`` () =
    test <@ getParse "abc" = ("abc" |> Identifier |> Atom) @>
    test <@ getParse "abc123" = ("abc123" |> Identifier |> Atom) @>
    test <@ getParse "abc_123" = ("abc_123" |> Identifier |> Atom) @>
    test <@ getParse "_abc123" = ("_abc123" |> Identifier |> Atom) @>

[<Fact>]
let ``Doesnt parse invalid identifiers`` () =
    test <@ parseFails "123abc" @>
    test <@ parseFails "a^b" @>

[<Fact>]
let ``Parses strings`` () =
    test <@ getParse "\"abc   123!@#$%\"" = ("abc   123!@#$%" |> String |> Atom) @>

[<Fact>]
let ``Parses lambda expressions`` () =
    test <@ getParse "\\x.x" = (("x", ("x" |> Identifier |> Atom)) |> Lambda) @>
    test <@ getParse "\\arg.2" = (("arg", (2.0 |> Number |> Atom)) |> Lambda) @>

[<Fact>]
let ``Parses curried lambda expressions`` () =
    let inner =
        (("y", (2.0 |> Number |> Atom)) |> Lambda)

    test <@ getParse "\\x.\\y.2" = (("x", inner) |> Lambda) @>



[<Fact>]
let ``Parses sequences`` () =
    test <@ getParse "(1)" = ([ num 1.0 ] |> Seq) @>
    test <@ getParse "(1; 2; 3)" = ([ num 1.0; num 2.0; num 3.0 ] |> Seq) @>
    test <@ getParse "()" = ([] |> Seq) @>

[<Fact>]
let ``Doesnt parse invalid sequences`` () =
    test <@ parseFails "(1;)" @>
    test <@ parseFails "(1; 2;)" @>
    test <@ parseFails "(;)" @>
    test <@ parseFails "(;1)" @>
    test <@ parseFails "(;1;2)" @>

[<Fact>]
let ``Parses lists`` () =
    test <@ getParse "[1]" = ([ num 1.0 ] |> List) @>
    test <@ getParse "[1, 2, 3]" = ([ num 1.0; num 2.0; num 3.0 ] |> List) @>
    test <@ getParse "[]" = ([] |> List) @>

[<Fact>]
let ``Doesnt parse invalid lists`` () =
    test <@ parseFails "[1,]" @>
    test <@ parseFails "[1, 2,]" @>
    test <@ parseFails "[,]" @>
    test <@ parseFails "[,1]" @>
    test <@ parseFails "[,1,2]" @>


[<Fact>]
let ``Parses apply expressions`` () =
    test <@ getParse "a b" = ((ident "a", ident "b") |> Apply) @>

    let expected =
        (((ident "a", ident "b") |> Apply), ident "c")
        |> Apply

    test <@ getParse "a b c" = expected @>

[<Fact>]
let ``Parses infix operators`` () =
    let expected =
        (((ident "+", num 2.0) |> Apply), num 3.0)
        |> Apply

    test <@ getParse "2 + 3" = expected @>
    test <@ getParse "2+ 3" = expected @>
    test <@ getParse "2 +3" = expected @>
    test <@ getParse "2+3" = expected @>

    let expected2 =
        (expected, num 4.0) |> Apply
    
    test <@ getParse "2 + 3 + 4" = expected2 @>

[<Fact>]
let ``Parses complex operators`` () =
    let expected =
        (((ident "@if_:", ident "abc") |> Apply), num 123.0)
        |> Apply
    
    test <@ getParse "@if abc: 123" = expected @>

[<Fact>]
let ``Parses let expressions`` () =
    let expected = ("a", num 2.0) |> Let
    test <@ getParse "let a = 2" = expected @>

[<Fact>]
let ``Parses let rec expressions`` () =
    let expected = ("a", num 2.0) |> Letrec
    test <@ getParse "let rec a = 2" = expected @>
