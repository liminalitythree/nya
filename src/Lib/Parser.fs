namespace Lib

open FParsec

module Parser =
    type NyaParser = Parser<NyaExpr, unit>

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
    
    let noperator =
        pstring "+"
    
    let ngroup = parse {
        do! skipString "("
        let! first = optional nexpr
        let! rest = many (pstring "," >>. nexpr)
        do! skipString ")"
        return Seq (first :: rest)
    }

    let nlist = parse {
        do! skipString "["
        let! first = optional nexpr
        let! rest = many (pstring "," >>. nexpr)
        do! skipString "]"
        return List (first :: rest)
    }

    let nprimary =
        natom <|> nlist <|> ngroup

    let nopapply = parse {
        let! first = nprimary
        let! rest = many1 (noperator .>>. nprimary)
        let ops = List.reduce rest (fun x -> x)
        return Apply ([first])
    }

    let napply = parse {
        let! first = nopapply
        let! rest = many nopapply
        return Apply (first :: rest)
    }

    let nexpr = napply

    let nprogram =
        nexpr .>> eof