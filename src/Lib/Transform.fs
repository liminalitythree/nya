// TODO:
// this is just a guess of what would be good
// move all sequences to top level (idk if this would be useful enough)
// possibly replace references to variables that are constants with the constant
// maybe

namespace Lib

open Infer

module Transform =

    // =================================================================
    // Renaming all variables to unique names

    let private genAndAdd (gen: Misc.IdGen) (localEnv: Map<string,string> ref) old =
        let newName = gen.Gen()
        localEnv := ((!localEnv).Add(old, newName))
        newName

    // gives every variable/identifier to a unique name maybe
    let rec transformUniqueNames (localEnv: Map<string, string> ref) (gen: Misc.IdGen) (expr: ANyaExpr) : ANyaExpr =
        match expr with
        | AList(_) -> failwith "list is not supported for now maybe"

        | ASeq(seq) ->
            let newEnv = ref localEnv.Value
            seq.E
                |> List.map (transformUniqueNames newEnv gen)
                |> annotate seq.T
                |> ASeq

        | AAtom(atom) ->
            match atom.E with
            | Number(_) | Bool(_) | String(_) -> expr

            | Identifier(ident) ->
                match (!localEnv).TryFind(ident) with
                | Some(uniqueName) ->
                    uniqueName |> Identifier |> annotate atom.T |> AAtom

                | None ->
                    let newName = genAndAdd gen localEnv ident
                    newName |> Identifier |> annotate atom.T |> AAtom

        | AApply(apply) ->
            let f,x = apply.E
            let uf = transformUniqueNames localEnv gen f
            let ux = transformUniqueNames localEnv gen x

            (uf, ux) |> annotate apply.T |> AApply

        | ALambda(lam) ->
            let arg,expr = lam.E
            let newEnv = ref localEnv.Value

            let newArg = genAndAdd gen newEnv arg.E |> annotate arg.T
            let uexpr = transformUniqueNames newEnv gen expr

            (newArg, uexpr) |> annotate lam.T |> ALambda

        | ALet(lett) ->
            let ident,expr = lett.E

            let newIdent = genAndAdd gen localEnv ident
            let uexpr = transformUniqueNames localEnv gen expr

            (newIdent, uexpr) |> annotate lett.T |> ALet

        | ALetrec(t, letrec) ->
            let ident,expr = letrec.E
            let newEnv = ref localEnv.Value

            let newIdent = genAndAdd gen newEnv ident
            let uexpr = transformUniqueNames newEnv gen expr

            (t, ((newIdent, uexpr) |> annotate letrec.T)) |> ALetrec
