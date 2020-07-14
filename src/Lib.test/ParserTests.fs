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

let emptyPos =
    let pos1 = Position("", 1L, 1L, 1L)
    Errors.Pos(pos1, pos1)

let withPos x = (x, emptyPos)

let ident = Identifier >> withPos >> Atom
let num = Number >> withPos >> Atom

[<Fact>]
let ``Can parse booleans`` () =
    test <@ getParse "true" = (true |> Bool |> withPos |> Atom) @>
    test <@ getParse "false" = (false |> Bool |> withPos |> Atom) @>

[<Fact>]
let ``Can parse decimal numbers`` () =
    test <@ getParse "0" = (0.0 |> Number |> withPos |> Atom) @>
    test <@ getParse "0.0" = (0.0 |> Number |> withPos |> Atom) @>
    test <@ getParse "1" = (1.0 |> Number |> withPos |> Atom) @>
    test <@ getParse "1." = (1.0 |> Number |> withPos |> Atom) @>
    test <@ getParse "12.34" = (12.34 |> Number |> withPos |> Atom) @>

[<Fact>]
let ``Doesnt parse invalid decimal numbers`` () =
    test <@ parseFails ".0" @>
    test <@ parseFails ".123" @>
    test <@ parseFails "1.2.3" @>

[<Fact>]
let ``Can parse identifiers`` () =
    test <@ getParse "abc" = ("abc" |> Identifier |> withPos |> Atom) @>
    test <@ getParse "abc123" = ("abc123" |> Identifier |> withPos |> Atom) @>
    test <@ getParse "abc_123" = ("abc_123" |> Identifier |> withPos |> Atom) @>
    test <@ getParse "_abc123" = ("_abc123" |> Identifier |> withPos |> Atom) @>

[<Fact>]
let ``Doesnt parse invalid identifiers`` () =
    test <@ parseFails "123abc" @>
    test <@ parseFails "a^b" @>

[<Fact>]
let ``Parses strings`` () =
    test <@ getParse "\"abc   123!@#$%\"" = ("abc   123!@#$%" |> String |> withPos |> Atom) @>

[<Fact>]
let ``Parses lambda expressions`` () =
    test
        <@ getParse "\\x.x" =
            (("x", ("x" |> Identifier |> withPos |> Atom), emptyPos)
             |> Lambda) @>
    test
        <@ getParse "\\arg.2" =
            (("arg", (2.0 |> Number |> withPos |> Atom), emptyPos)
             |> Lambda) @>

[<Fact>]
let ``Parses curried lambda expressions`` () =
    let inner =
        (("y", (2.0 |> Number |> withPos |> Atom), emptyPos)
         |> Lambda)

    test <@ getParse "\\x.\\y.2" = (("x", inner, emptyPos) |> Lambda) @>



[<Fact>]
let ``Parses sequences`` () =
    test <@ getParse "(1)" = ([ num 1.0 ] |> withPos |> Seq) @>
    test <@ getParse "(1; 2; 3)" = ([ num 1.0; num 2.0; num 3.0 ] |> withPos |> Seq) @>
    test <@ getParse "()" = ([] |> withPos |> Seq) @>

[<Fact>]
let ``Doesnt parse invalid sequences`` () =
    test <@ parseFails "(1;)" @>
    test <@ parseFails "(1; 2;)" @>
    test <@ parseFails "(;)" @>
    test <@ parseFails "(;1)" @>
    test <@ parseFails "(;1;2)" @>

[<Fact>]
let ``Parses lists`` () =
    test <@ getParse "[1]" = ([ num 1.0 ] |> withPos |> List) @>
    test <@ getParse "[1, 2, 3]" = ([ num 1.0; num 2.0; num 3.0 ] |> withPos |> List) @>
    test <@ getParse "[]" = ([] |> withPos |> List) @>

[<Fact>]
let ``Doesnt parse invalid lists`` () =
    test <@ parseFails "[1,]" @>
    test <@ parseFails "[1, 2,]" @>
    test <@ parseFails "[,]" @>
    test <@ parseFails "[,1]" @>
    test <@ parseFails "[,1,2]" @>


[<Fact>]
let ``Parses apply expressions`` () =
    test <@ getParse "a b" = ((ident "a", ident "b", emptyPos) |> Apply) @>

    let expected =
        (((ident "a", ident "b", emptyPos) |> Apply), ident "c", emptyPos)
        |> Apply

    test <@ getParse "a b c" = expected @>

[<Fact>]
let ``Parses infix operators`` () =
    let expected =
        (((ident "+", num 2.0, emptyPos) |> Apply), num 3.0, emptyPos)
        |> Apply

    test <@ getParse "2 + 3" = expected @>
    test <@ getParse "2+ 3" = expected @>
    test <@ getParse "2 +3" = expected @>
    test <@ getParse "2+3" = expected @>

    let expected2 = (expected, num 4.0, emptyPos) |> Apply

    test <@ getParse "2 + 3 + 4" = expected2 @>

[<Fact>]
let ``Parses complex operators`` () =
    let expected =
        (((ident "@if_:", ident "abc", emptyPos) |> Apply), num 123.0, emptyPos)
        |> Apply

    test <@ getParse "@if abc: 123" = expected @>

[<Fact>]
let ``Parses let expressions`` () =
    let expected = ("a", num 2.0, emptyPos) |> Let
    test <@ getParse "let a = 2" = expected @>

[<Fact>]
let ``Parses let rec expressions`` () =
    let expected = ("a", num 2.0, emptyPos) |> Letrec
    test <@ getParse "let rec a = 2" = expected @>
