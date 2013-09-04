module TypeManagement
open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler.AbstractIL.ILBinaryReader
open Microsoft.FSharp.Compiler.AbstractIL.IL 

let getType (currentModule: ILModuleReader) (ilTypeSpec: ILTypeSpec) =
    let typesModule = 
        if ilTypeSpec.Scope.IsLocalRef then
            currentModule
        else
            Loader.getModule ilTypeSpec.Scope.AssemblyRef
    let targetType = 
        typesModule.ILModuleDef.TypeDefs 
        |> Seq.append ( typesModule.ILModuleDef.TypeDefs |> Seq.collect(fun x -> x.NestedTypes)) 
        |> Seq.tryFind (fun x -> x.Name = ilTypeSpec.Name)
    match targetType with
    | Some x -> x, typesModule
    | None -> failwithf "Failed to load %A" ilTypeSpec


