namespace Lib

module Token =
    type Op =
        | Plus | Minus | Star | Div
        | Named of string

    type T =
        | LeftParen | RightParen
        | LeftBracket | RightBracket
        | Seperator
        | Colon
        | Operator of Op
        | Identifier of string
        | StringLiteral of string
        | IntLiteral of int
        | DecLiteral of float
        | Eof
