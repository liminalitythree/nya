// Runs all the steps to compile a program maybe
// maybe
// im sorry
// maybe

namespace Lib

module Compile =
    let compile (source: string) =
        let uniqueNameGen = Misc.IdGen("nyaref^")
        let lambdaIdGen = Misc.IdGen("lam^")
        let env: Infer.Environment ref = ref BuiltinUtil.Env

        source
        |> Parser.parse
        >>= (Infer.infer env)
        |>> (Transform.transformUniqueNames (ref Map.empty<string, string>) uniqueNameGen)
        |>> (LambdaLift.lambdaLift lambdaIdGen)
