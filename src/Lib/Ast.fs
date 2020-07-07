namespace Lib

type NyaAtom =
    | Identifier of string
    | Number of float
    | String of string
    | Bool of bool

type NyaExpr =
    | Seq of NyaExpr list
    | List of NyaExpr list
    | Apply of NyaExpr * NyaExpr
    | Atom of NyaAtom
    | Let of string list * NyaExpr
