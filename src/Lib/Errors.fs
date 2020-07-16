namespace Lib

module Errors =
    type Pos = FParsec.Position * FParsec.Position

    // ─── A PRETTY-PRINTED COMPILATON ERROR MAYBE ────────────────────────────────────

    type NErrorType =
        | ParseError of string
        | TypeError

    [<RequireQualifiedAccess>]
    type NyaError =
        {
          // Error message
          Msg: string
          // Position where error occured
          ErrPos: Pos option
          Type: NErrorType }

    // ─── CUSTOM RESULT TYPE ─────────────────────────────────────────────────────────

    [<RequireQualifiedAccess>]
    [<Struct>]
    type NyaResult<'a> =
        | Ok of ok: 'a
        | Error of error: NyaError

    let inline nOk x = NyaResult.Ok x
    let inline nError x = NyaResult.Error x

    let inline nBind (uf: 'a -> NyaResult<'b>) (t: NyaResult<'a>): NyaResult<'b> =
        match t with
        | NyaResult.Ok tok -> uf tok
        | NyaResult.Error terror -> nError terror

    let inline nMap (m: 'a -> 'b) (t: NyaResult<'a>): NyaResult<'b> =
        match t with
        | NyaResult.Ok tok -> nOk (m tok)
        | NyaResult.Error terr -> nError terr

    type NyaResult<'a> with
        static member inline (>>=)(x, uf) = nBind uf x
        static member inline (|>>)(x, m) = nMap m x

    // ─── EXCEPTION TYPE ─────────────────────────────────────────────────────────────

    exception NyaException of NyaError

    let nyaFailWith pos errType msg =
        let nError =
            { NyaError.Msg = msg
              NyaError.ErrPos = Some pos
              NyaError.Type = errType }

        raise (NyaException nError)

    let nyaFailWithNoPos errType msg =
        let nError =
            { NyaError.Msg = msg
              NyaError.ErrPos = None
              NyaError.Type = errType }

        raise (NyaException nError)

    // ─── PRETTY-PRINTER ─────────────────────────────────────────────────────────────

    let private posAsLineCol (position: FParsec.Position) =
        sprintf "Line: %d, Col: %d" position.Line position.Column

    let prettyPrintErr (source: string) (err: NyaError): string =
        match err.Type with
        | ParseError msg -> msg
        | TypeError ->
            let lines =
                source.Replace('\r', ' ').Split [| '\n' |]

            let posText =
                match err.ErrPos with
                | Some pos ->
                    let start, finish = pos
                    let line = lines.[int (finish.Line) - 1]
                    sprintf "In: %s\n%s\nFrom: (%s), To: (%s)" finish.StreamName line (posAsLineCol start)
                        (posAsLineCol finish)

                | None -> ""

            sprintf "A Type error occurred:\n%s\n%s" err.Msg posText
