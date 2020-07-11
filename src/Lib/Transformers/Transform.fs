// TODO:
// this is just a guess of what would be good
// move all sequences and lambdas to top level
// give each variable a unique name (in scopes too) so we can ignore scoping
//     this might be better to do in the typechecker idk
// transform curried functions to normal functions (maybe, idk if we need to do this)
// maybe change constant/non-ref atoms (strings, numbers, bools, etc) to a seperate type
// possibly replace references to variables that are constants with the constant
// maybe

namespace Lib

open Infer

module Transform =

    // =================================================================
    // Renaming all variables to unique names

    type UniqueNameGenerator () =
        let curid = ref 1

        member this.Gen () =
            incr curid
            (!curid - 1).ToString()

    let private genAndAdd (gen: UniqueNameGenerator) (localEnv: Map<string,string> ref) old =
        let newName = gen.Gen()
        localEnv := ((!localEnv).Add(old, newName))
        newName

    let rec transformUniqueNames (localEnv: Map<string, string> ref) (gen: UniqueNameGenerator) (expr: ANyaExpr) : ANyaExpr =
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
