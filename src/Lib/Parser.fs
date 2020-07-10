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

    let private nOpIdentifierStr =
        let isAsciiIdStart c =
            c = '@'

        let isAsciiIdContinue c =
            isAsciiLetter c || isDigit c

        identifier (IdentifierOptions(isAsciiIdStart = isAsciiIdStart,
                                    isAsciiIdContinue = isAsciiIdContinue))

    let private nidentifier = nIdentifierStr |>> (Identifier >> Atom)

    let private nstring: NyaParser =
        between (pstring "\"") (pstring "\"") (manySatisfy ((<>) '"')) |>> (String >> Atom)

    let private nlambdaChar = pstring "\\" <|> pstring "λ"
    let private nlambda = (nlambdaChar >>. nIdentifierStr .>> strWs ".") .>>. nexpr |>> Lambda

    let private natom =
        ntrue <|> nfalse <|> nnumber <|> nidentifier <|> nstring <|> nlambda

    let private ngroup = strWs "(" >>. sepBy nexpr (strWs ";") .>> strWs ")" |>> Seq

    let private nlist = strWs "[" >>. sepBy nexpr (strWs ",") .>> strWs "]" |>> List

    let private nprimary =
        (natom <|> nlist <|> ngroup) .>> ws

    let private symbolOperators: Parser<string,unit> =
        strWs "+" <|> strWs "-" <|> strWs "/" <|> strWs "*" <|> strWs "=" <|> strWs ":"

    let private noperator =
        symbolOperators <|> nOpIdentifierStr .>> ws

    // if every element in list is equal
    // ! no clue if this works
    let private allEqual (x: 'a list): bool =
        let first = x.[0]
        x |> List.fold (fun e x -> (x = first) = e = true) true

    let private applyFromList (x: NyaExpr list) =
        let rec afl (x: NyaExpr list) (a: Option<NyaExpr>) : NyaExpr =
            if x.Length <= 0 then
                match a with
                | None    -> failwith "this shouldn't be reachable i think"
                | Some(a) -> a
            else
                match a with
                | None ->
                    if x.Length < 2 then
                        failwith "this shouldn't be possible i think"
                    else
                        afl x.[2..] (Some (Apply (x.[0], x.[1])))

                | Some(a) ->
                    afl x.[1..] (Some (Apply (a, x.[0])))
        afl x None

    let private handleNapply (x: NyaExpr list) =
        if x.Length = 1 then
            x.[0]
        else
            applyFromList x

    let private napply = many1 nprimary |>> handleNapply

    type private PossibleOpT =
        | Op of string * NyaExpr
        | NoOp of NyaExpr

    let private possibleOp =
        (napply |>> NoOp )<|> (noperator .>>. napply |>> Op)

    // TODO: make multiple calls of the same op return just one op, eg (1 + 1 + 1) = +, , not +_+
    let private handleNopapply (x: (PossibleOpT * (string * NyaExpr) list) ) =
        let first, xs = x

        if xs.IsEmpty then
            match first with
            | Op(op,expr) -> Apply (op |> Identifier |> Atom, expr)
            | NoOp(expr)  -> expr
        else
            let ops = xs |> List.map (fun (x,_) -> x)
            let things = xs |> List.map (fun (_, y) -> y)

            let ops, first = match first with
                                | Op(op,expr) -> (op :: ops, expr)
                                | NoOp(expr)  -> (ops, expr)

            let fn = ops |> List.reduce (fun e x -> e + "_" + x) |> Identifier |> Atom
            let args = first :: things

            applyFromList (fn :: args)

    // TODO: fix bug where it wont put the operator in the ast when there is no space after the operator
    // eg: 2 +2
    let private nopapply = possibleOp .>>. many (noperator .>>. napply) |>> handleNopapply

    let private handleNlet x =
        let a,expr = x
        let isrec,ident = a
        match isrec with
        | Some(_) -> (ident, expr) |> Letrec
        | None    -> (ident, expr) |> Let

    let private nlet = (strWs "let" >>.
                            ((opt (strWs "rec") .>>. (nIdentifierStr .>> ws .>> strWs "=") .>>. nexpr) |>> handleNlet))
                        <|> nopapply

    do nexprImpl := nlet .>> ws

    let private nprogram =
        nexpr .>> eof

    let parse str = run nprogram str