module MemoryManagement
open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler.AbstractIL.ILBinaryReader
open Microsoft.FSharp.Compiler.AbstractIL.IL 

let memoryChunkSize = 5120
let memoryArrays: obj[][] = Array.zeroCreate memoryChunkSize

let getSlotIndexes i =
    let chunkIndex = i / memoryChunkSize
    let index = i - (chunkIndex * memoryChunkSize)
    chunkIndex, index

let getSlotValue i =
    let chunkIndex, index = getSlotIndexes i
    memoryArrays.[chunkIndex].[index]

let setSlotValue i value =
    let chunkIndex, index = getSlotIndexes i
    if memoryArrays.[chunkIndex] = null then
        memoryArrays.[chunkIndex] <- Array.zeroCreate memoryChunkSize
    memoryArrays.[chunkIndex].[index] <- value


let heapPointer = ref (5 * memoryChunkSize)
let threadStackPointer = ref 1

let pushThreadParameters (parameters: obj[]) (targetMethod: ILMethodDef) = 
    for parameter in parameters do
        setSlotValue !threadStackPointer parameter
        incr threadStackPointer
    setSlotValue !threadStackPointer (targetMethod, parameters.Length)
    incr threadStackPointer
        
let popThreadParameters () = 
    decr threadStackPointer
    let _, parameterCount = (getSlotValue !threadStackPointer) :?> (ILMethodDef * int)
    threadStackPointer := !threadStackPointer - parameterCount

let getParameterAddress i = 
    let methodDesLocaction = !threadStackPointer - 1
    let _, parameterCount = (getSlotValue methodDesLocaction) :?> (ILMethodDef * int)
    !threadStackPointer - (parameterCount + 1) + i

let staticFieldLocations = new Dictionary<string,int>()

let rec getDefaultValue (t: ILType) =
    match t with
    | ILType.Void -> failwith "Unsupported type"
    | ILType.Array (_) -> null
    | ILType.Value (ilTypeSpec) -> Activator.CreateInstance(Type.GetType(ilTypeSpec.TypeRef.QualifiedName))
    | ILType.Boxed (ilTypeSpec) -> null
    | ILType.Ptr (ilType) -> IntPtr.Zero :> obj
    | ILType.Byref (ilType) -> failwith "Unsupported type"
    | ILType.FunctionPointer (ilCallingSignature) -> failwith "Unsupported type"
    | ILType.TypeVar i -> failwith "Unsupported type"
    | ILType.Modified (required,  ilTypeRef, ilType) -> getDefaultValue ilType

let getFieldStaticFieldOffset t name =
    t.Fields.AsList 
    |> List.filter (fun x -> x.IsStatic) 
    |> List.findIndex(fun x -> x.Name = name)

let getStaticFieldAddress (tModule: ILModuleReader) (t: ILTypeDef) (field: ILFieldSpec) (execMethod: (ILMethodDef -> 'a)) =
    // TODO this is will not provide a key that's unique for each type
    let typeFullname = sprintf "%s, %s" t.Name tModule.ILModuleDef.Name

    if not (staticFieldLocations.ContainsKey typeFullname) then
        staticFieldLocations.Add(typeFullname, !heapPointer)
        for field in t.Fields.AsList |> List.filter (fun x -> x.IsStatic) do
            setSlotValue !heapPointer (getDefaultValue field.Type)
            incr heapPointer
        
        let staticCtor = t.Methods.AsList |> List.find (fun x -> x.IsClassInitializer)
        execMethod staticCtor |> ignore
    
    staticFieldLocations.[typeFullname] + (getFieldStaticFieldOffset t field.Name)

let getStaticField (tModule: ILModuleReader) (t: ILTypeDef) (field: ILFieldSpec) (execMethod: (ILMethodDef -> 'a)) =
    let location = getStaticFieldAddress tModule t field execMethod
    getSlotValue location


let getFieldInstranceFieldOffset t name =
    t.Fields.AsList 
    |> List.filter (fun x -> not x.IsStatic) 
    |> List.findIndex(fun x -> x.Name = name)

let getBlankObject (t: ILTypeDef) =
    let objRef = !heapPointer
    setSlotValue (!heapPointer) t
    incr heapPointer
    for field in t.Fields.AsList |> List.filter (fun x -> not x.IsStatic) do
        setSlotValue !heapPointer (getDefaultValue field.Type)
        incr heapPointer
    objRef

let storeValueInObjectField objRef value (currentModule: ILModuleReader) (fieldSpec: ILFieldSpec) =
    let t, _ = TypeManagement.getType currentModule fieldSpec.EnclosingType.TypeSpec
    let offset = getFieldInstranceFieldOffset t fieldSpec.Name
    setSlotValue (objRef + offset) value

let storeBoxedValue (value: obj) =
    let objRef = !heapPointer
    setSlotValue !heapPointer value
    incr heapPointer
    objRef
