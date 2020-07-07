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

    type private Stateful() =
        // for generating unique generic variable names
        let typeNumber = ref 1

        let env = ref Map.empty<string, Type.T>
        
        let genNewType =
            let c1 = !typeNumber
            incr typeNumber
            Type.Ident ("t^" + c1.ToString())
        
        let annotateAtom a =
            match a with
            | Number        -> an a Type.Num    |> AAtom
            | Bool          -> an a Type.Bool   |> AAtom
            | String        -> an a Type.String |> AAtom
            | Identifier(s) ->
                match Map.tryFind s !env with
                | Some(t) -> an a t |> AAtom
                | None    -> sprintf "Unknwon variable: %s" s |> failwith


        let rec annotateExpr (e: NyaExpr) : ANyaExpr =
            match e with
            | Seq(_) | List(_) -> failwith "List and seq are unsupported for now i think"

            | Atom(a) -> annotateAtom a

            | Apply(f, x) ->
                let af = annotateExpr f
                let ax = annotateExpr x
                an (af, ax) genNewType |> AApply
            