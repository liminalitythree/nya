namespace Lib

open Mono.Cecil
open Mono.Cecil.Cil

module Codegen =
    let test () =
        let assembly =
            AssemblyDefinition.CreateAssembly
                (AssemblyNameDefinition("HelloWorld", System.Version()), "HelloWorld", ModuleKind.Console)

        let modul = assembly.MainModule

        let programType =
            TypeDefinition
                ("HelloWorld", "Program", (TypeAttributes.Class ||| TypeAttributes.Public), modul.TypeSystem.Object)

        modul.Types.Add(programType)

        let mainMethod =
            MethodDefinition
                ("Main",
                 MethodAttributes.Public
                 ||| MethodAttributes.Static,
                 modul.TypeSystem.Void)

        programType.Methods.Add(mainMethod)

        let il = mainMethod.Body.GetILProcessor()

        il.Emit(OpCodes.Ldstr, "Hello World")

        let writline =
            modul.ImportReference((typeof<System.Console>).GetMethod("WriteLine", [| typeof<System.String> |]))

        il.Emit(OpCodes.Call, writline)

        il.Emit(OpCodes.Nop)
        il.Emit(OpCodes.Ret)

        assembly.EntryPoint <- mainMethod
        assembly.Write("test.exe")
        ()
