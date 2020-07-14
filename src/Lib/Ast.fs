namespace Lib

type NyaAtom =
    | Identifier of string
    | Number of float
    | String of string
    | Bool of bool

type NyaExpr =
    | Seq of NyaExpr list * Errors.Pos
    | List of NyaExpr list * Errors.Pos
    | Apply of NyaExpr * NyaExpr * Errors.Pos
    | Atom of NyaAtom * Errors.Pos
    | Lambda of string * NyaExpr * Errors.Pos
    | Let of string * NyaExpr * Errors.Pos
    | Letrec of string * NyaExpr * Errors.Pos
