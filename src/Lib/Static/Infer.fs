namespace Lib


module Infer =
    type private Environment = Map<string, Type.T>

    // Annotated with type information
    type A<'a> = {
        T: Type.T
        E: 'a
    }

    // wraps the annotate generic in easy function i think maybe
    let private an e v =
        { T = v ; E = e}

    // Annotated Expression
    type ANyaExpr =
        | ASeq of A<ANyaExpr list>
        | AList of A<ANyaExpr list>
        | AApply of A<ANyaExpr * ANyaExpr>
        | AAtom of A<NyaAtom>

    let private typeOfAExpr e =
        match e with
        | ASeq(t)   -> t.T
        | AList(t)  -> t.T
        | AApply(t) -> t.T
        | AAtom(t)  -> t.T

    // =================================================================
    // generates generic types maybe
    // =================================================================
    type private TypeGenerator() =
        // for generating unique generic variable names
        let typeNumber = ref 1

        member this.Gen () =
            let c1 = !typeNumber
            incr typeNumber
            Type.Ident ("t^" + c1.ToString())

    // =================================================================
    // annotate ast with types
    // =================================================================
    let defaultEnv = ref Map.empty<string, Type.T>

    let annotateAtom a env =
        match a with
        | Number        -> an a Type.Num    |> AAtom
        | Bool          -> an a Type.Bool   |> AAtom
        | String        -> an a Type.String |> AAtom
        | Identifier(s) ->
            match Map.tryFind s env with
            | Some(t) -> an a t |> AAtom
            | None    -> sprintf "Unknowon identifier: %s" s |> failwith

    let rec private annotateExpr (e: NyaExpr) (env: Environment ref) (gen: TypeGenerator): ANyaExpr =
        match e with
        | Seq(_) | List(_) -> failwith "List and seq are unsupported for now i think"

        | Atom(a) -> annotateAtom a !env

        | Apply(f, x) ->
            let af = annotateExpr f env gen
            let ax = annotateExpr x env gen
            an (af, ax) (gen.Gen()) |> AApply
