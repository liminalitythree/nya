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

    let private nIdentifierStr =
        let isAsciiIdStart c =
            isAsciiLetter c || c = '_'
        
        let isAsciiIdContinue c =
            isAsciiLetter c || isDigit c || c = '_'
        
        identifier (IdentifierOptions(isAsciiIdStart = isAsciiIdStart,
                                    isAsciiIdContinue = isAsciiIdContinue))

    let private nidentifier = nIdentifierStr |>> (Identifier >> Atom)

    let private nstring: NyaParser =
        between (pstring "\"") (pstring "\"") (manySatisfy ((<>) '"')) |>> (String >> Atom)

    let private natom =
        ntrue <|> nfalse <|> nnumber <|> nidentifier <|> nstring
    
    let private ngroup = strWs "(" >>. sepBy nexpr (strWs ",") .>> strWs ")" |>> Seq

    let private nlist = strWs "[" >>. sepBy nexpr (strWs ",") .>> strWs "]" |>> List

    let private nprimary =
        (natom <|> nlist <|> ngroup) .>> ws

    let private noperator =
        strWs "+"

    let private handleNopapply (x: (NyaExpr * (string * NyaExpr) list) ) =
        let first, xs = x

        if xs.IsEmpty then
            first
        else
            let ops = xs |> List.map (fun (x,_) -> x)
            let things = xs |> List.map (fun (_, y) -> y)

            let ops = ops |> List.reduce (+) |> Identifier |> Atom

            Apply ([ops; List (first :: things)])

    let private nopapply = nprimary .>>. sepBy (noperator .>>. nprimary) ws |>> handleNopapply

    let private napply = sepBy nopapply ws1 |>> Apply

    do nexprImpl := napply .>> ws

    let private nprogram =
        nexpr .>> eof
    
    let parse str = run nprogram str