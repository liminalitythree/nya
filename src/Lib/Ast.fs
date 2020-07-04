namespace Lib

type NyaOp =
    | Named of string
    | Plus
    | Minus
    | Div
    | Mul
    | Equality
    | Inequality
    | Greater
    | GreaterEq
    | Less
    | LessEq

type NyaValue =
    | Identifier of string
    | Number of float
    | String of string
    | Bool of bool
    | Operator of NyaOp

type NyaExpr =
    | Seq of NyaExpr list
    | List of NyaExpr list
    | Value of NyaValue
