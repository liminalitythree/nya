namespace Lib

open Errors
open Infer

module FinalAst =
    // ─── THE AST THAT GOES INTO THE CODEGEN - I THINK ───────────────────────────────

    type FunId = string

    type LNyaExpr =
        | LSeq of A<LNyaExpr list> * Pos
        | LList of A<LNyaExpr list> * Pos
        | LApply of A<LNyaExpr * LNyaExpr> * Pos
        | LAtom of A<NyaAtom> * Pos
        // id * arg types * return type
        | LLambdaRef of FunId * Type.T list * Type.T * Pos
        | LLet of A<string * LNyaExpr> * Pos
        | LLetrec of Type.T * A<string * LNyaExpr> * Pos

    // ─── FUNCTION DATATYPES ─────────────────────────────────────────────────────────

    // Lambda Id, argument to the lambda, list of free variable arguments the lambda needs maybe
    type LiftedLambda =
        { Id: string
          Args: Infer.A<string> list
          Expr: LNyaExpr }

    [<RequireQualifiedAccess>]
    type NFunction =
        | Lambda of LiftedLambda
        | Builtin

    // table of functions in the final program maybe
    type FunTable = Map<FunId, NFunction>
