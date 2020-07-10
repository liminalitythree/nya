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

    // takes the arguments the other way around maybe
    let annotate v e =
        { T = v ; E = e}

    // Annotated Expression
    type ANyaExpr =
        | ASeq of A<ANyaExpr list>
        | AList of A<ANyaExpr list>
        | AApply of A<ANyaExpr * ANyaExpr>
        | AAtom of A<NyaAtom>
        | ALambda of A<A<string> * ANyaExpr>
        | ALet of A<string * ANyaExpr>
        | ALetrec of Type.T * A<string * ANyaExpr>

    let typeOfAExpr e =
        match e with
        | ASeq(t)      -> t.T
        | AList(t)     -> t.T
        | AApply(t)    -> t.T
        | AAtom(t)     -> t.T
        | ALambda(t)   -> t.T
        | ALet(t)      -> t.T
        | ALetrec(_,t) -> t.T

    // =================================================================
    // generates generic types maybe
    // =================================================================
    type TypeGenerator() =
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
        | List(_) -> failwith "List is unsupported for now i think"

        | Seq(seq) ->
            let newEnv = ref env.Value
            (seq.Tail |> List.fold
                (fun acc x -> acc @ [annotateExpr x newEnv gen])
                [annotateExpr seq.Head newEnv gen]
            |> an) (typeOfAExpr (annotateExpr seq.[seq.Length - 1] newEnv gen))
            |> ASeq

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

        | Let(i, e) ->
            let ae = annotateExpr e env gen
            let t = typeOfAExpr ae
            env := (!env).Add(i, t)
            an (i, ae) t |> ALet

        | Letrec(i, e) ->
            let recType = gen.Gen()
            let newEnv = ref ((!env).Add(i, recType))
            let ae = annotateExpr e newEnv gen
            let t = typeOfAExpr ae
            env := (!env).Add(i, t)
            (recType, (an (i, ae) t)) |> ALetrec

    // =================================================================
    // collect type constraints
    // =================================================================

    type private Constrait = (Type.T * Type.T)

    let rec collectExpr (ae: ANyaExpr) : Constrait list =
        match ae with
        | AList(_) -> failwith "List is unsupported for now i think"

        | ASeq(seq) ->
            let cs = seq.E.Tail |> List.fold (fun e x -> e @ (collectExpr x)) (collectExpr seq.E.Head)
            cs @ [(typeOfAExpr seq.E.[seq.E.Length - 1], seq.T)]


        // ! i dont know whether this is right
        | ALet(e) ->
            let _,expr = e.E
            collectExpr expr

        | ALetrec(t, e) ->
            let _,expr = e.E
            [(t, e.T)] @ collectExpr expr

        // no constraints to impose on literals & identifier gives us no info maybe
        // ! i hope this is correct
        | AAtom(_) -> []

        | ALambda(t) ->
            let _,ae = t.E
            match t.T with
            | Type.Lambda(_, e) ->
                (collectExpr ae) @ [(typeOfAExpr ae, e)]
            | _ -> failwith "not a lambda"

        // this is copy-pasted :)
       (* 1. In application expressions, the first expression should be of TFun type or it
             could be a unknown type placeholder. Otherwise it's an error.
          2. Case 1: TFun(argt, ret_type)
             - In this case the parameter type of the function should be same as that of
               the argument passed in the function.
             - Second, the return type of the function, will be equal to the return type
               of the function application expression.
          3. Case 2: T(_)  (unknown type placeholder)
             - Since we do not know the type information of the first expression in an
               application expression, we cannot use the above approach.
             - But we do know that the first expression has to be a function. Also a function
               whose parameter type is same as that of argument type and that has a return type
               same as that of the entire expression.
             - Thus we use this information to impose a contraint on the unknown type placeholder.
       *)

        | AApply(at) ->
            let fn,arg = at.E
            let t = at.T
            match (typeOfAExpr fn) with
            | Type.Lambda(argt, retType) ->
                (collectExpr fn) @ (collectExpr arg) @ [(t, retType); (argt, typeOfAExpr arg)]

            | Type.Ident(_) ->
                (collectExpr fn) @ (collectExpr arg) @ [(typeOfAExpr fn, Type.Lambda (typeOfAExpr arg, t))]

            | _ -> failwith "incorrect function application"

    // =================================================================
    // Unification
    // =================================================================

    // unknown type, resolved type
    type private Substitutions = (string * Type.T) list

    // replaces all occurences of type placeholder x with u, in type t
    let rec private substitute (u: Type.T) (x: string) (t: Type.T) : Type.T =
        match t with
        | Type.Num | Type.Bool | Type.String -> t
        | Type.Ident(c) -> if c = x then u else t
        | Type.Lambda(t1, t2) -> Type.Lambda(substitute u x t1, substitute u x t2)

    // returns t after all the given substitutions have been made in it
    let private apply (subs: Substitutions) (t: Type.T) : Type.T =
        List.foldBack (fun (x, u) t -> substitute u x t) subs t

    // turns constraints into substitutions
    let rec private unify (constraints: Constrait list) : Substitutions =
        match constraints with
        | [] -> []
        | (x, y) :: xs ->
            // generate substitutions of the rest of the list
            let t2 = unify xs
            // resolve the LHS and RHS of the constraints from the previous substitutions
            let t1 = unifyOne (apply t2 x) (apply t2 y)
            t1 @ t2

    and private unifyOne (t1: Type.T) (t2: Type.T) : Substitutions =
        match t1, t2 with
        | Type.Num, Type.Num | Type.Bool, Type.Bool
        | Type.String, Type.String -> []

        | Type.Ident(x), z | z, Type.Ident(x) -> [(x, z)]

        // this is useful for calling a function that returns a function maybe?
        | Type.Lambda(a, b), Type.Lambda(x, y) -> unify [(a, x); (b, y)]

        | _ -> failwith "mismatched types"

    // applies a final set of substitutions on the annotated expr
    let rec private applyExpr (subs: Substitutions) (ae: ANyaExpr) : ANyaExpr =
        match ae with
        | AList(_) -> failwith "List is unsupported for now i think"

        | ASeq(seq) ->
            let e = seq.E
            let applied = e |> List.map (fun x -> applyExpr subs x)
            an applied (apply subs seq.T) |> ASeq


        // ! i don't know whether this is right maybe
        | ALet(e) | ALetrec(_,e) ->
            let _,expr = e.E
            applyExpr subs expr

        | AAtom(atom) ->
            an atom.E (apply subs atom.T) |> AAtom

        | ALambda(lam) ->
            let id,e = lam.E
            an (id, applyExpr subs e) (apply subs lam.T) |> ALambda

        | AApply(app) ->
            let fn,arg = app.E
            an (applyExpr subs fn, applyExpr subs arg) (apply subs app.T) |> AApply

    (* runs HMTI step-by-step
   1. annotate expression with placeholder types
   2. generate constraints
   3. unify types based on constraints
   4. run the final set of substitutions on still unresolved types
   5. obtain a final annotated expression with resolved types *)
    let infer (env: Environment ref) (e: NyaExpr) : ANyaExpr =
        let gen = TypeGenerator ()

        let annotatedExpr = annotateExpr e env gen
        let constraints = collectExpr annotatedExpr
        let subs = unify constraints
        applyExpr subs annotatedExpr

    type IncrementalInfer = {
        Env: Environment ref;
        Gen: TypeGenerator;
        Constraints: Option<Constrait list>;
    }

    let incrementalFromEnv env =
        { Env = env ; Gen = TypeGenerator () ; Constraints = None }

    // this is for inferring things in the same space thing in the repl maybe
    let incrementalInfer (t: IncrementalInfer) (e: NyaExpr) =
        let annotatedExpr = annotateExpr e t.Env t.Gen
        let constraints = collectExpr annotatedExpr
        let constraints = match t.Constraints with
                            | None -> constraints
                            | Some(prev) -> prev @ constraints
        let subs = unify constraints
        ((applyExpr subs annotatedExpr), { t with Constraints = Some constraints })