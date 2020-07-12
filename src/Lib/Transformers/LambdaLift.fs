namespace Lib

open Infer

module LambdaLift =
    // note, returned list may have duplicates
    // ignore - list of variables/identifiers to ignore maybe
    let rec private getFreeVariables (ignore: string list) (expr: ANyaExpr) : (string * Type.T) list =
        match expr with
        | AList(_) -> failwith "lists are not supported yet i think maybe sorry maybe"

        | ASeq(seq) ->
            seq.E |> List.fold (fun e x -> e @ getFreeVariables ignore x) []

        | AApply(apply) ->
            let f,x = apply.E
            (getFreeVariables ignore f) @ (getFreeVariables ignore x)

        | AAtom(atom) ->
            match atom.E with
            | Number(_) | String(_) | Bool(_) -> []
            | Identifier(ident) -> [(ident, atom.T)]

        | ALambda(lam) ->
            let arg,expr = lam.E
            getFreeVariables (ignore @ [arg.E]) expr

        | ALet(lett) ->
            let _,expr = lett.E
            getFreeVariables ignore expr

        | ALetrec(_, letrec) ->
            let self,expr = letrec.E
            getFreeVariables (ignore @ [self]) expr

    let getFreeVarsNoDupes ignore expr = getFreeVariables ignore expr |> Seq.distinct |> Seq.toList

    type LambdaId = string


