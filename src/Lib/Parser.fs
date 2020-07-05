namespace Lib

open FParsec

module Parser =
    type private NyaParser = Parser<NyaExpr, unit>

    let private ws = spaces
    let private ws1 = spaces1

    let private strWs s = pstring s .>> ws

    let private nexpr, nexprImpl = createParserForwardedToRef()

    let private ntrue: NyaParser = stringReturn "true" (Atom (Bool true))
    let private nfalse: NyaParser = stringReturn "false" (Atom (Bool false))
    
    let private nnumber: NyaParser = pfloat |>> (Number >> Atom)

    let private nidentifier: NyaParser = parse {
        let! first = letter
        let! rest = manyChars (letter <|> digit)
        return Identifier (first.ToString() + rest) |> Atom
    }

    let private nstring: NyaParser =
        between (pstring "\"") (pstring "\"") (manySatisfy ((<>) '"')) |>> (String >> Atom)

    let private natom =
        ntrue <|> nfalse <|> nnumber <|> nidentifier <|> nstring
    
    let private ngroup = parse {
        do! skipString "("
        do! ws
        let! first = opt nexpr
        let! rest = many (strWs "," >>. nexpr)
        do! skipString ")"
        return match first with
                | Some(first) -> Seq (first :: rest)
                | None -> Seq []
    }

    let private nlist = parse {
        do! skipString "["
        do! ws
        let! first = opt nexpr
        let! rest = many (strWs "," >>. nexpr)
        do! skipString "]"
        return match first with
                | Some(first) -> Seq (first :: rest)
                | None -> Seq []
    }

    let private nprimary =
        natom <|> nlist <|> ngroup

    let private noperator =
        pstring "+"

    let private nopapply = parse {
        let! first = nprimary
        let! rest = many1 (noperator .>>. nprimary)

        let ops = rest |> List.map (fun (x,_) -> x)
        let things = rest |> List.map (fun (_, y) -> y)

        let ops = ops |> List.reduce (+) |> Identifier |> Atom

        return Apply ([ops; List things])
    }

    let private napply = parse {
        let! first = nopapply
        let! rest = many nopapply
        return Apply (first :: rest)
    }

    do nexprImpl := napply .>> ws

    let private nprogram =
        nexpr .>> eof
    
    let parse str = run nprogram str