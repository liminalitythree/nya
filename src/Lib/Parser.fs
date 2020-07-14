namespace Lib

open FParsec

module Parser =
    type private NyaParser = Parser<NyaExpr, unit>

    // ─── UTILITY FUNCTIONS ──────────────────────────────────────────────────────────

    let private applyFromList (x: NyaExpr list) =
        // determine the position of the list
        let pStart = Misc.getPos x.Head
        let pEnd = Misc.getPos x.[x.Length - 1]
        let listPos = Misc.mergePos pStart pEnd
        // ---
        let rec afl (x: NyaExpr list) (a: Option<NyaExpr>): NyaExpr =
            if x.Length <= 0 then
                match a with
                | None -> failwith "this shouldn't be reachable i think"
                | Some (a) -> a
            else
                match a with
                | None ->
                    if x.Length < 2
                    then failwith "this shouldn't be possible i think"
                    else afl x.[2..] ((x.[0], x.[1], listPos) |> Apply |> Some)

                | Some (a) -> afl x.[1..] ((a, x.[0], listPos) |> Apply |> Some)

        afl x None

    // if every element in list is equal
    // ! no clue if this works
    let private allEqual (x: 'a list): bool =
        let first = x.[0]
        x
        |> List.fold (fun e x -> (x = first) = e = true) true

    // Get the previous position on the same line.
    let leftOf (p: Position) =
        if p.Column > 1L
        then Position(p.StreamName, p.Index - 1L, p.Line, p.Column - 1L)
        else p

    // get the Pos of a stream
    let posFromStream (stream: CharStream<_>): Errors.Pos =
        let pos = stream.Position
        ((leftOf pos), pos)

    // returns the left and right pos
    let getPos: Parser<_, _> =
        fun stream -> Reply(posFromStream stream)

    // ─── BUILDING BLOCKS ────────────────────────────────────────────────────────────

    let private ws = spaces
    let private ws1 = spaces1

    let private strWs s = pstring s .>> ws

    let private nexpr, nexprImpl = createParserForwardedToRef ()

    let private ntrue: NyaParser =
        attempt (pstring "true")
        >>. getPos
        |>> (fun x -> ((true |> Bool), x) |> Atom)

    let private nfalse: NyaParser =
        attempt (pstring "false")
        >>. getPos
        |>> (fun x -> ((false |> Bool), x) |> Atom)

    let private nnumber: NyaParser =
        pfloat
        .>>. getPos
        |>> (fun (x, p) -> ((x |> Number), p) |> Atom)

    // ─── IDENTIFIERS ────────────────────────────────────────────────────────────────

    let private nIdentifierStr =
        let isAsciiIdStart c = isAsciiLetter c || c = '_'

        let isAsciiIdContinue c = isAsciiLetter c || isDigit c || c = '_'

        identifier (IdentifierOptions(isAsciiIdStart = isAsciiIdStart, isAsciiIdContinue = isAsciiIdContinue))

    let private nOpIdentifierStr =
        let isAsciiIdStart c = c = '@'

        let isAsciiIdContinue c = isAsciiLetter c || isDigit c

        identifier (IdentifierOptions(isAsciiIdStart = isAsciiIdStart, isAsciiIdContinue = isAsciiIdContinue))

    let private nidentifier =
        nIdentifierStr
        .>>. getPos
        |>> (fun (x, p) -> ((x |> Identifier), p) |> Atom)

    // ─── STRINGS ────────────────────────────────────────────────────────────────────

    let private nstring: NyaParser =
        (between (pstring "\"") (pstring "\"") (manySatisfy ((<>) '"')))
        .>>. getPos
        |>> (fun (x, p) -> ((x |> String), p) |> Atom)

    // ─── LAMBDAS ────────────────────────────────────────────────────────────────────

    let private nlambdaChar = pstring "\\" <|> pstring "λ"

    let private nlambda =
        ((nlambdaChar >>. nIdentifierStr .>> strWs ".")
         .>>. nexpr)
        .>>. getPos
        |>> (fun ((i, e), p) -> (i, e, p) |> Lambda)

    // ─── ATOMS ──────────────────────────────────────────────────────────────────────

    let private natom =
        ntrue
        <|> nfalse
        <|> nnumber
        <|> nidentifier
        <|> nstring
        <|> nlambda

    // ─── SEQUENCES ──────────────────────────────────────────────────────────────────

    let private nseq =
        (strWs "("
         >>. sepBy nexpr (strWs ";")
         .>> strWs ")")
        .>>. getPos
        |>> Seq

    // ─── LISTS ──────────────────────────────────────────────────────────────────────

    let private nlist =
        (strWs "["
         >>. sepBy nexpr (strWs ",")
         .>> strWs "]")
        .>>. getPos
        |>> List

    // ─── PRIMARY - FOR OPERATOR PRECEDENCE - ────────────────────────────────────────

    let private nprimary = (natom <|> nlist <|> nseq) .>> ws

    // ─── APPLICATION EXPRESSIONS ────────────────────────────────────────────────────

    let private handleNapply (x: NyaExpr list) =
        if x.Length = 1 then x.[0] else applyFromList x

    let private napply = many1 nprimary |>> handleNapply

    // ─── OPERATORS ──────────────────────────────────────────────────────────────────

    let private symbolOperators: Parser<string, unit> =
        strWs "+"
        <|> strWs "-"
        <|> strWs "/"
        <|> strWs "*"
        <|> strWs "="
        <|> strWs ":"

    let private noperator =
        symbolOperators <|> nOpIdentifierStr .>> ws

    type private PossibleOpT =
        | Op of string * NyaExpr
        | NoOp of NyaExpr

    let private possibleOp =
        (napply |>> NoOp)
        <|> (noperator .>>. napply |>> Op)

    // TODO: make multiple calls of the same op return just one op, eg (1 + 1 + 1) = +, , not +_+
    let private handleNopapply (x: ((Option<string> * NyaExpr) * (string * NyaExpr) list) * Errors.Pos) =
        let ((firstOp, firstExpr), xs), pos = x

        if xs.IsEmpty then
            match firstOp with
            | Some (op) ->
                ((op |> Identifier, pos) |> Atom, firstExpr, pos)
                |> Apply
            | None -> firstExpr
        else
            let ops = xs |> List.map (fun (x, _) -> x)
            let things = xs |> List.map (fun (_, y) -> y)

            let ops, first =
                match firstOp with
                | Some (op) -> (op :: ops, firstExpr)
                | None -> (ops, firstExpr)


            let wholePos =
                let firstPos = Misc.getPos first
                let lastPos = Misc.getPos things.[things.Length - 1]
                Misc.mergePos firstPos lastPos

            let fn =
                ((ops
                  |> List.reduce (fun e x -> e + "_" + x)
                  |> Identifier),
                 wholePos)
                |> Atom

            let args = first :: things

            applyFromList (fn :: args)

    // TODO: fix bug where it wont put the operator in the ast when there is no space after the operator
    // eg: 2 +2
    let private nopapply =
        ((opt noperator .>>. napply)
         .>>. many (noperator .>>. napply))
        .>>. getPos
        |>> handleNopapply

    // ─── LET EXPRESSIONS ────────────────────────────────────────────────────────────

    let private handleNlet (x: ((string option * string) * NyaExpr) * Errors.Pos) =
        let (((isrec, ident), expr), pos) = x
        match isrec with
        | Some (_) -> (ident, expr, pos) |> Letrec
        | None -> (ident, expr, pos) |> Let

    let private nlet =
        (strWs "let"
         >>. ((opt (strWs "rec")
               .>>. (nIdentifierStr .>> ws .>> strWs "=")
               .>>. nexpr)
              .>>. getPos
              |>> handleNlet))
        <|> nopapply

    // ─── MAIN PARSER ────────────────────────────────────────────────────────────────

    do nexprImpl := nlet .>> ws

    let private nprogram = nexpr .>> eof

    let parse str = run nprogram str
