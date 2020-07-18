namespace Lib

open Mono.Cecil
open Mono.Cecil.Cil
open FinalAst
open Errors

module Codegen =
    // ─── UTILITY FUNCTIONS ──────────────────────────────────────────────────────────

    // FunTable with methods added to each function in it maybe
    type MFunTable = Map<FunId, NFunction * MethodDefinition>

    // creates methods (with no IL other than the builtins) on the function in the funtable,
    // returning a MFunTable
    // adds builtin methods too maybe
    let createMethods (modul: ModuleDefinition) (table: FunTable): MFunTable =
        let mainDefinition =
            MethodDefinition
                ("NYA^MAIN",
                 MethodAttributes.Public
                 ||| MethodAttributes.Static,
                 modul.TypeSystem.Void)

        table
        |> Map.fold (fun acc funId nFun ->
            match nFun with
            | NFunction.Builtin builtin -> acc.Add(funId, (nFun, builtin.GenMethodDefination modul))
            | NFunction.MainFunction _ -> acc.Add(funId, (nFun, mainDefinition))
            | NFunction.Lambda _ -> failwith "Lambda functions are not supported in codegen for now maybe")
               Map.empty<FunId, NFunction * MethodDefinition>


    // ─── CODE GENERATION FUNCTIONS ──────────────────────────────────────────────────

    // generates code from a FunTable maybe
    let fromFunTable (table: MFunTable) = 2

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
