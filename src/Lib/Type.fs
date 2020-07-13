namespace Lib

module Type =
    type T =
        | Num
        | Bool
        | String
        | Lambda of T * T
        // type variable
        | Ident of string

    let rec toString t =
        match t with
        | Num -> "num"
        | Bool -> "bool"
        | String -> "string"

        | Lambda (fn, arg) -> sprintf "%s -> %s" (toString fn) (toString arg)

        | Ident (s) -> "'" + s
