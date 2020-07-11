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


    // lambda-lifted nyaexpr maybe
    type LNyaExpr =
        | LSeq of Infer.A<LNyaExpr list>
        | LList of Infer.A<LNyaExpr list>
        | LApply of Infer.A<LNyaExpr * LNyaExpr>
        | LAtom of Infer.A<NyaAtom>
        // id * arg type * return type
        | LLambdaRef of LambdaId * Type.T * Type.T
        | LLet of Infer.A<string * ANyaExpr>
        | LLetrec of Type.T * A<string * LNyaExpr>

    // Lambda Id * list of free variable arguments the lambda needs maybe
    type private LiftedLambda = LambdaId * Infer.A<string> list

    let private mergeMaps map1 map2 = Map.fold (fun acc key value -> Map.add key value acc) map1 map2

    type LambdaLifter () =
        // map < identifier of lambda,
        let lifteds = ref Map.empty<string, LiftedLambda>

        let rec lambdaLift (expr: ANyaExpr) : LNyaExpr * Map<LambdaId, LNyaExpr> =
            match expr with
            | AList(_) -> failwith "list is not supported for now maybe maybe maybe"

            | ASeq(seq) ->
                let l,m = seq.E
                        |> List.map lambdaLift
                        |> List.fold (fun (e,emap) (exp,map) -> (e @ [exp]), (mergeMaps emap map)) ([], Map.empty<LambdaId,LNyaExpr>)

                ((l |> annotate seq.T |> LSeq), m)

            | AApply(apply) ->
                let f,x = apply.E
                let lf,m1 = lambdaLift f
                let lx,m2 = lambdaLift x
                (((lf,lx) |> annotate apply.T |> LApply), mergeMaps m1 m2)

            | AAtom(atom) -> (atom |> LAtom), Map.empty
