open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler.AbstractIL.ILBinaryReader
open Microsoft.FSharp.Compiler.AbstractIL.IL 

[<EntryPoint>]
let main argv = 
    let entryModule = Loader.loadModuleFromPath argv.[0]
    try
        let entryPoint =
            entryModule.ILModuleDef.TypeDefs |> Seq.collect (fun x -> x.Methods |> Seq.map (fun m -> x, m) ) |> Seq.tryFind (fun (t,m) -> m.IsEntryPoint)
        match entryPoint with
        | Some (t, entryPoint) -> 
            ExecutionEngine.exeMethod entryModule [||] t entryPoint |> ignore
        | None -> failwith "No entry point found"
    finally
        Loader.closeModules()
    0 

