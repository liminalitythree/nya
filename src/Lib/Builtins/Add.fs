// Adds two numbers together

namespace Lib.Builtins

open Lib.Type
open Mono.Cecil
open Mono.Cecil.Cil

type Add() =
    inherit Lib.Builtin()

    override __.Identifier = "+"
    override __.Type = (Lambda(Num, (Lambda(Num, Num))))

    override __.GenMethodDefination(cecilModule: ModuleDefinition) =
        let addMethod =
            MethodDefinition
                ("NYA^ADD",
                 MethodAttributes.Public
                 ||| MethodAttributes.Static,
                 cecilModule.TypeSystem.Double)

        addMethod.Parameters.Add(ParameterDefinition("a", ParameterAttributes.In, cecilModule.TypeSystem.Double))
        addMethod.Parameters.Add(ParameterDefinition("b", ParameterAttributes.In, cecilModule.TypeSystem.Double))

        let il = addMethod.Body.GetILProcessor()

        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg_1)

        il.Emit(OpCodes.Add)

        il.Emit(OpCodes.Ret)

        addMethod
