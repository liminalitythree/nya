namespace Lib

type NyaAtom =
    | Identifier of string
    | Number of float
    | String of string
    | Bool of bool
    | Lambda of string * NyaExpr

and NyaExpr =
    | Seq of NyaExpr list
    | List of NyaExpr list
    | Apply of NyaExpr * NyaExpr
    | Atom of NyaAtom
