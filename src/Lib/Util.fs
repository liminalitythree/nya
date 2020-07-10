// contains various utility functions maybe

namespace Lib

module Util =
    // transforms a function definition with arguments specified into a curried function
    // NOTE: this curries backwards, eg \xyz. would curry to \z.\y.\x. maybe
    let rec private curryBackwards (args: Infer.A<string> list) (expr: Infer.ANyaExpr) (retType: Type.T) =
        match args with
        | [] -> expr
        | head::xs ->
            let newExpr = (head, expr) |> Infer.annotate retType |> Infer.ALambda
            curryBackwards xs newExpr retType

    // reverses the input list and calls curryBackwards maybe
    let curry (args: Infer.A<string> list) (expr: Infer.ANyaExpr) (retType: Type.T) =
        curryBackwards (List.rev args) expr retType

    // turns a curried lambda into one with an argument list i think maybe... maybe
    let rec private doUnCurry (l: Infer.A<string> list) (expr: Infer.ANyaExpr) : Infer.A<string> list * Infer.ANyaExpr  =
        match expr with
        | Infer.ALambda(lam) ->
            let ident,inner = lam.E
            doUnCurry (l @ [ident]) inner

        | _ -> (l, expr)

    let unCurry = doUnCurry []