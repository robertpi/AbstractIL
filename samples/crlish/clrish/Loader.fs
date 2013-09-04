module Loader

open System
open System.Collections.Generic
open System.IO
open Microsoft.FSharp.Compiler.AbstractIL.ILBinaryReader
open Microsoft.FSharp.Compiler.AbstractIL.IL 

let assemblyPath = Environment.ExpandEnvironmentVariables(@"%windir%\Microsoft.NET\Framework\v4.0.30319")

let modules = new Dictionary<string,ILModuleReader>()

let loadModuleFromPath path =
    let ``module`` = OpenILModuleReader path defaults
    match ``module``.ILModuleDef.Manifest with
    | Some manifest ->
        modules.Add(manifest.Name, ``module``)
    | None -> failwith "Module has no manifest, only full assemblies are support"
    ``module``

let getModule (assemblyRef: ILAssemblyRef) =
    if modules.ContainsKey assemblyRef.Name then
        modules.[assemblyRef.Name]
    else
        // sooner or later we'll need to search more places/file type, but this will get the frawework .dlls
        loadModuleFromPath (Path.Combine(assemblyPath, assemblyRef.Name + ".dll"))

let closeModules() =
    for ``module`` in modules.Values do
        CloseILModuleReader ``module``
