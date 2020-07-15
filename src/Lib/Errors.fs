namespace Lib

module Errors =
    type Pos = FParsec.Position * FParsec.Position

    // ─── A PRETTY-PRINTED COMPILATON ERROR MAYBE ────────────────────────────────────

    type NyaError =
        {
          // Error message
          Msg: string
          // Position where error occured
          ErrPos: Pos }
