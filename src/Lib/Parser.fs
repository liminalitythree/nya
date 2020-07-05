namespace Lib

open FParsec

module Parser =
    type NyaParser = Parser<NyaExpr, unit>

    let nexpr, nexprImpl = createParserForwardedToRef()

    let ntrue: NyaParser = stringReturn "true" (Atom (Bool true))
    let nfalse: NyaParser = stringReturn "false" (Atom (Bool false))
    
    let nnumber: NyaParser = pfloat |>> (Number >> Atom)

    let nidentifier: NyaParser = parse {
        let! first = letter
        let! rest = manyChars (letter <|> digit)
        return Identifier (first.ToString() + rest) |> Atom
    }

    let nstring: NyaParser =
        between (pstring "\"") (pstring "\"") (manySatisfy ((<>) '"')) |>> (String >> Atom)

    let natom =
        ntrue <|> nfalse <|> nnumber <|> nidentifier <|> nstring
    
    let ngroup = parse {
        do! skipString "("
        let! first = opt nexpr
        let! rest = many (pstring "," >>. nexpr)
        do! skipString ")"
        return match first with
                | Some(first) -> Seq (first :: rest)
                | None -> Seq []
    }

    let nlist = parse {
        do! skipString "["
        let! first = opt nexpr
        let! rest = many (pstring "," >>. nexpr)
        do! skipString "]"
        return match first with
                | Some(first) -> Seq (first :: rest)
                | None -> Seq []
    }

    let nprimary =
        natom <|> nlist <|> ngroup

    let noperator =
        pstring "+"

    let nopapply = parse {
        let! first = nprimary
        let! rest = many1 (noperator .>>. nprimary)

        let ops = rest |> List.map (fun (x,_) -> x)
        let things = rest |> List.map (fun (_, y) -> y)

        let ops = ops |> List.reduce (+) |> Identifier |> Atom

        return Apply ([ops; List things])
    }

    let napply = parse {
        let! first = nopapply
        let! rest = many nopapply
        return Apply (first :: rest)
    }

    do nexprImpl := napply

    let nprogram =
        nexpr .>> eof