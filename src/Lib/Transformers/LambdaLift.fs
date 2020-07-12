namespace Lib

open Infer

module LambdaLift =
    // note, returned list may have duplicates
    // ignore - list of variables/identifiers to ignore maybe
    let rec private getFreeVariables (ignore: Set<string>) (expr: ANyaExpr): (string * Type.T) list =
        match expr with
        | AList (_) -> failwith "lists are not supported yet i think maybe sorry maybe"

        | ASeq (seq) ->
            seq.E
            |> List.fold (fun e x -> e @ getFreeVariables ignore x) []

        | AApply (apply) ->
            let f, x = apply.E
            (getFreeVariables ignore f)
            @ (getFreeVariables ignore x)

        | AAtom (atom) ->
            match atom.E with
            | Number (_)
            | String (_)
            | Bool (_) -> []
            | Identifier (ident) -> if ignore.Contains ident then [] else [ (ident, atom.T) ]

        | ALambda (lam) ->
            let arg, expr = lam.E
            getFreeVariables (ignore.Add arg.E) expr

        | ALet (lett) ->
            let _, expr = lett.E
            getFreeVariables ignore expr

        | ALetrec (_, letrec) ->
            let self, expr = letrec.E
            getFreeVariables (ignore.Add self) expr

    let getFreeVarsNoDupes expr ignore =
        getFreeVariables ignore expr
        |> Seq.distinct
        |> Seq.toList

    type LambdaId = string


    // lambda-lifted nyaexpr maybe
    type LNyaExpr =
        | LSeq of Infer.A<LNyaExpr list>
        | LList of Infer.A<LNyaExpr list>
        | LApply of Infer.A<LNyaExpr * LNyaExpr>
        | LAtom of Infer.A<NyaAtom>
        // id * arg types * return type
        | LLambdaRef of LambdaId * Type.T list * Type.T
        | LLet of Infer.A<string * LNyaExpr>
        | LLetrec of Type.T * A<string * LNyaExpr>

    // Lambda Id, argument to the lambda, list of free variable arguments the lambda needs maybe
    type LiftedLambda =
        { Id: string
          Args: Infer.A<string> list
          Expr: LNyaExpr }

    let private mergeMaps map1 map2 =
        Map.fold (fun acc key value -> Map.add key value acc) map1 map2

    // (hopefully) does lambda lifting
    // ! NOTE: expects the input expression to only have unique identifiers,
    // ! eg with Transform.transformUniqueNames
    // ! maybe
    let rec lambdaLift (gen: Util.IdGen) (expr: ANyaExpr): LNyaExpr * Map<LambdaId, LiftedLambda> =
        let lambdaLift = lambdaLift gen
        match expr with
        | AList (_) -> failwith "list is not supported for now maybe maybe maybe"

        | ASeq (seq) ->
            let l, m =
                seq.E
                |> List.map lambdaLift
                |> List.fold (fun (e, emap) (exp, map) -> (e @ [ exp ]), (mergeMaps emap map))
                       ([], Map.empty<LambdaId, LiftedLambda>)

            ((l |> annotate seq.T |> LSeq), m)

        | AApply (apply) ->
            let f, x = apply.E
            let lf, m1 = lambdaLift f
            let lx, m2 = lambdaLift x
            (((lf, lx) |> annotate apply.T |> LApply), mergeMaps m1 m2)

        | AAtom (atom) -> (atom |> LAtom), Map.empty

        | ALambda (lam) ->
            let _, exp = lam.E
            let args, innerExp = Util.unCurry expr

            let ignore =
                args
                |> List.fold (fun (e: Set<string>) x -> e.Add x.E) (Set.empty<string>)

            printfn "IGNORE: %A" ignore

            let freeVars =
                (getFreeVarsNoDupes exp ignore)
                |> List.map (fun (i, t) -> i |> annotate t)

            printfn "FREEVARS: %A" freeVars
            let args = freeVars @ args

            let lInnerExp, lmap = lambdaLift innerExp
            let lId = gen.Gen()

            let lref =
                (lId, (args |> List.map (fun x -> x.T)), lam.T)
                |> LLambdaRef

            let llambda =
                { Id = lId
                  Args = args
                  Expr = lInnerExp }

            if freeVars.IsEmpty then
                (lref, lmap.Add(lId, llambda))
            else
                let lapply =
                    freeVars
                    |> List.fold (fun acc x ->
                        (acc, (x.E |> Identifier |> annotate x.T |> LAtom))
                        |> annotate (typeOfAExpr innerExp)
                        |> LApply) (lref)

                (lapply, lmap.Add(lId, llambda))

        | ALet (lett) ->
            let i, expr = lett.E
            let lexpr, lmap = lambdaLift expr
            ((i, lexpr) |> annotate lett.T |> LLet), lmap

        | ALetrec (t, letrec) ->
            let i, expr = letrec.E
            let lexpr, lmap = lambdaLift expr
            ((t, (i, lexpr) |> annotate letrec.T) |> LLetrec), lmap
