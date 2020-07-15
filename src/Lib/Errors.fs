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
