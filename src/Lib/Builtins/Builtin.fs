// a Nya function built into the compiler
namespace Lib

[<AbstractClass>]
type Builtin() =
    abstract Type: Type.T

    // the name of the function as its called in the Nya lang maybe
    abstract Identifier: string

    abstract GenMethodDefination: Mono.Cecil.ModuleDefinition -> Mono.Cecil.MethodDefinition

    // adds the builtin to an environment for type checking / inferring,
    // and returns the new environment
    member this.AddToEnv(env: Map<string, Type.T>) = env.Add(this.Identifier, this.Type)

// a collection of all the built ins in Nya maybe
type BuiltinCollection(builtins: Builtin list) =
    // makes an Environment for type checking out of all the
    // builtins we have maybe
    member __.MakeEnv =
        builtins
        |> List.fold (fun e x -> x.AddToEnv e) Map.empty<string, Type.T>
