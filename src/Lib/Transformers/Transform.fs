// TODO:
// this is just a guess of what would be good
// move all sequences and lambdas to top level
// give each variable a unique name (in scopes too) so we can ignore scoping
//     this might be better to do in the typechecker idk
// transform curried functions to normal functions (maybe, idk if we need to do this)
// maybe change constant/non-ref atoms (strings, numbers, bools, etc) to a seperate type
// possibly replace references to variables that are constants with the constant
// maybe

namespace Lib

module Transform =
    let a = 2
