namespace Lib

open FinalAst

module BuiltinUtil =
    // list of all the builtins in the Nya language maybe
    let private nyaBuiltins: Builtin list = [ Builtins.Add() ]

    // get the Env for type checking and stuff like that
    // maybe
    let Env: Map<string, Type.T> =
        nyaBuiltins
        |> List.fold (fun e x -> x.AddToEnv e) Map.empty<string, Type.T>

    let AddToFunTable (funTable: FunTable): FunTable =
        nyaBuiltins
        |> List.fold (fun e x -> e.Add(x.Identifier, x |> NFunction.Builtin)) funTable
