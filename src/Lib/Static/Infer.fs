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
        | ALambda of A<A<string> * ANyaExpr>

    let private typeOfAExpr e =
        match e with
        | ASeq(t)    -> t.T
        | AList(t)   -> t.T
        | AApply(t)  -> t.T
        | AAtom(t)   -> t.T
        | ALambda(t) -> t.T

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
    let private defaultEnv = ref Map.empty<string, Type.T>

    let private annotateAtom a env =
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
        
        | Lambda(x, e) ->
            let ax = an x (gen.Gen())
            let newEnv = ref ((!env).Add(x, ax.T))
            let ae = annotateExpr e newEnv gen
            let alam = an (ax, ae) ((ax.T, (typeOfAExpr ae)) |> Type.Lambda)
            alam |> ALambda

    // =================================================================
    // collect type constraints
    // =================================================================
    
    type private Constrait = (Type.T * Type.T)

    let rec collectExpr (ae: ANyaExpr) : Constrait list =
        match ae with
        // no constraints to impose on literals & identifier gives us no info maybe
        // ! i hope this is correct
        | AAtom(_) -> []

        | ALambda(t) ->
            let _,ae = t.E 
            match t.T with
            | Type.Lambda(_, e) ->
                (collectExpr ae) @ [(typeOfAExpr ae, e)]
            | _ -> failwith "not a lambda"
        
        