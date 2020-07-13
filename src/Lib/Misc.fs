namespace Lib

module Misc =
    // =============================================
    // unique id generator maybe
    type IdGen(prefix: string) =
        let curid = ref 1

        member __.Gen() =
            incr curid
            prefix + ((!curid - 1).ToString())
