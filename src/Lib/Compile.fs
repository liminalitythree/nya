// Runs all the steps to compile a program maybe
// maybe
// im sorry
// maybe

namespace Lib

module Compile =
    let compile (source: string) (env: Infer.Environment ref) =
        let uniqueNameGen = Misc.IdGen("nyaref^")
        let lambdaIdGen = Misc.IdGen("lam^")

        source
        |> Parser.parse
        >>= (Infer.infer env)
        |>> (Transform.transformUniqueNames (ref Map.empty<string, string>) uniqueNameGen)
        |>> (LambdaLift.lambdaLift lambdaIdGen)
