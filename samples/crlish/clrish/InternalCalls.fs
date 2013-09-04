module InternalCalls
open System
open System.Threading
open Microsoft.FSharp.Compiler.AbstractIL.ILBinaryReader
open Microsoft.FSharp.Compiler.AbstractIL.IL 

let execInternalCall (meth: ILMethodDef) (parameters: obj[]) =
    match meth.Name with
    | "_CompareExchange" ->
        Interlocked.CompareExchange(ref parameters.[0], parameters.[1], parameters.[2]) 
    | _ -> failwith "internal call not supported"
