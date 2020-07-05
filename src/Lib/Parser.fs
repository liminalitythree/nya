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

    // if every element in list is equal
    let private allEqual (x: 'a list): bool =
        let first = x.[0]
        x |> List.fold (fun e x -> (x = first) = e = true) true

    let private handleNopapply (x: (NyaExpr * (string * NyaExpr) list) ) =
        let first, xs = x

        if xs.IsEmpty then
            first
        else
            let ops = xs |> List.map (fun (x,_) -> x)
            let things = xs |> List.map (fun (_, y) -> y)

            let ops = ops |> List.reduce (fun e x -> e + "_" + x) |> Identifier |> Atom

            Apply ([ops; List (first :: things)])

    let private nopapply = nprimary .>>. many (noperator .>>. nprimary) |>> handleNopapply

    let private handleNapply (x: NyaExpr list) =
        if x.Length = 1 then
            x.[0]
        else
            x |> Apply

    let private napply = many nopapply |>> handleNapply

    do nexprImpl := napply .>> ws

    let private nprogram =
        nexpr .>> eof
    
    let parse str = run nprogram str