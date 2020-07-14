namespace Lib

module Misc =
    // =============================================
    // unique id generator maybe
    type IdGen(prefix: string) =
        let curid = ref 1

        member __.Gen() =
            incr curid
            prefix + ((!curid - 1).ToString())

    //
    // ──────────────────────────────────────────────────────────────────────────── I ──────────
    //   :::::: P O S I T I O N   U T I L I T I E S : :  :   :    :     :        :          :
    // ──────────────────────────────────────────────────────────────────────────────────────
    //

    // ─── GET THE POSITION OF A NYAEXPR ──────────────────────────────────────────────

    let getPos expr =
        match expr with
        | Seq (_, pos)
        | List (_, pos)
        | Apply (_, _, pos)
        | Atom (_, pos)
        | Lambda (_, _, pos)
        | Let (_, _, pos)
        | Letrec (_, _, pos) -> pos

    // ─── GET A POSITION THAT ENCOMPASSES BOTH INPUT POSITIONS ───────────────────────

    let mergePos (pos1: Errors.Pos) (pos2: Errors.Pos) =
        let pStart, _ = pos1
        let _, pEnd = pos2
        (pStart, pEnd) |> Errors.Pos
