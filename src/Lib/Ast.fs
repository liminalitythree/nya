namespace Lib

type NyaAtom =
    | Identifier of string
    | Number of float
    | String of string
    | Bool of bool

type NyaExpr =
    | Seq of NyaExpr list
    | List of NyaExpr list
    | Apply of NyaExpr list
    | Atom of NyaAtom