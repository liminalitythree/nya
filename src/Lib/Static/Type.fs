namespace Lib

module Type =
    type T =
        | Num
        | Bool
        | String
        | Fun of T * T
        // binds T to string
        | Let of string * T
        // type variable
        | Ident of string

    let rec toString t =
        match t with
        | Num    -> "num"
        | Bool   -> "bool"
        | String -> "string"

        | Fun(fn, arg) ->
            sprintf "%s -> %s" (toString fn) (toString arg)

        | Let(v, defn) ->
            sprintf "(let %s = %s)" v (toString defn)

        | Ident(s) -> "'" + s




