namespace Lib.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Lib

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.TestMethodPassing () =
        Assert.IsTrue(false);
