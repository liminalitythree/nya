namespace Lib

open FParsec

module Parser =
    type NyaParser = Parser<NyaExpr, unit>