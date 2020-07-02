namespace Lib

module Lexer =
    type T = {
        Source: string
        FileName: string
        Tokens: Errors.W<Token.T> list
        Start: int
        Current: int
        Line: int
        Col: int
        HadError: bool
    }

    let defaultT source fileName = {
        Source = source
        FileName = fileName
        Tokens = []
        Start = 0
        Current = 0
        Line = 1
        Col = 0
        HadError = false
    }

    let private lineInfo t =
        { Errors.Line = t.Line; Errors.Col = t.Col; Errors.FileName = t.FileName }

    let private errFrom t token  =
        { Errors.T = token; Errors.LineInfo = Some (lineInfo t) }

    let private isAtEnd t =
        t.Current >= t.Source.Length

    let private add t ele =
        { t with Tokens = t.Tokens @ [ele] }
    
    let private addE t ele =
        add t (errFrom t ele)
    
    let private advance t =
        let t = {t with Current = t.Current + 1}
        (t, t.Source.[t.Current - 1])
    
    let private err t msg =
        (Errors.Error t (lineInfo t) msg) |> ignore
        {t with HadError = true}

    let private scanT t =
        let (t, c) = advance t
        match c with
        | '(' -> addE t Token.T.LeftParen
        | ')' -> addE t Token.T.RightParen
        | '[' -> addE t Token.T.LeftBracket
        | ']' -> addE t Token.T.RightBracket
        | ',' -> addE t Token.T.Seperator
        | ':' -> addE t Token.T.Colon
        | '+' -> addE t (Token.Operator Token.Op.Plus)
        | '-' -> addE t (Token.Operator Token.Op.Minus)
        | '*' -> addE t (Token.Operator Token.Op.Star)
        | '/' -> addE t (Token.Operator Token.Op.Div)
        | _ -> err t (sprintf "Unexpected character, %c" c)
 
    let rec scan t =
        if isAtEnd t then
            addE t Token.T.Eof
        else if t.HadError then
            t
        else
            scan ( scanT { t with Start = t.Current } )
            
       