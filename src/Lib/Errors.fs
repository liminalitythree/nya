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
