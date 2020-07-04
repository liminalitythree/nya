module LexerTests

open System
open Xunit
open Xunit.Abstractions
open Lib

let lexscan input =
    Lib.Lexer.scan (Lib.Lexer.defaultT input "<test>")

let getTokens (res: Lexer.T)  =
    ( res.Tokens |> List.map (fun x -> x.T) )

let seqEqual a b =
    Linq.Enumerable.SequenceEqual(a, b)

type LexerTests(output: ITestOutputHelper) =
    [<Fact>]
    let ``Handles 1-wide characters`` () =
        let input = "()[],:+-/*"
        let res = lexscan input
        Assert.False(res.HadError)
        let tokens = getTokens res
        let expected = [
            Token.LeftParen;
            Token.RightParen;
            Token.LeftBracket;
            Token.RightBracket;
            Token.Seperator;
            Token.Colon;
            (Token.Operator Token.Op.Plus);
            (Token.Operator Token.Op.Minus);
            (Token.Operator Token.Op.Div);
            (Token.Operator Token.Op.Star);
            Token.Eof;
        ]
        output.WriteLine(sprintf "%A" tokens)

        Assert.True(seqEqual tokens expected)

    [<Fact>]
    let ``ignores comments`` () =
        let input = """
    [] # should ignore this() ()
    # should ignore this too []
    :: () # asdf[]asd
    """
        let res = lexscan input
        Assert.False(res.HadError)

        let tokens = getTokens res
        let expected = [
            Token.LeftBracket;
            Token.RightBracket;
            Token.Colon;
            Token.Colon;
            Token.LeftParen;
            Token.RightParen;
            Token.Eof;
        ]
        output.WriteLine(sprintf "%A" tokens)

        Assert.True(seqEqual tokens expected)