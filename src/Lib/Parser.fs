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
