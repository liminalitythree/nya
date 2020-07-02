namespace Lib

module Errors =
    type LineInfo = {
        FileName: string
        Line: int
        Col: int
    }

    let Error t lineInfo msg =
        printf 
            "Error at (line: %i, col: %i) in %s:\n%s\n"
            lineInfo.Line
            lineInfo.Col
            lineInfo.FileName
            msg
        t

    // wrapper for adding line information and stuff like that
    // to any type
    type W<'t> = {
        T: 't
        LineInfo: Option<LineInfo>
    }


