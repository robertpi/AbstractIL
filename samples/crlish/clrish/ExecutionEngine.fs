module ExecutionEngine
open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler.AbstractIL.ILBinaryReader
open Microsoft.FSharp.Compiler.AbstractIL.IL 

type InstructionResult =
    | NoResult
    | Return of obj
    | Branch of ILCodeLabel
    | ExceptionThrown of Exception


let rec exeMethod (currentModule: ILModuleReader) (parameters: obj[]) (typeDef: ILTypeDef) (methodDef: ILMethodDef) =
    let stack = new Stack<obj>()
    let locals = Array.zeroCreate methodDef.Locals.Length

    let exeTargetMethod (ilMethodSpec: ILMethodSpec) objPtr =
        let innerExe ilTypeSpec =
            let targetType, targetModule = TypeManagement.getType currentModule ilTypeSpec
            let possibleMethods = targetType.Methods.FindByName(ilMethodSpec.Name)
            let targetMethod = possibleMethods |> Seq.tryFind (fun x -> x.CallingSignature.ArgTypes = ilMethodSpec.FormalArgTypes)
            match targetMethod with
            | Some targetMethod ->
                let parameters = 
                    match  objPtr with
                    | Some x ->
                        let parameters = Array.zeroCreate (stack.Count + 1)
                        stack.CopyTo(parameters, 0)
                        parameters.[stack.Count] <-x 
                        parameters |> Array.rev
                    | None -> stack.ToArray()
                MemoryManagement.pushThreadParameters parameters targetMethod
                let result = exeMethod targetModule parameters targetType targetMethod 
                stack.Clear()
                MemoryManagement.popThreadParameters ()
                let result' =
                    match result with
                    | Return result -> 
                        stack.Push result
                        NoResult
                    | _ -> result
                result'
            | None -> failwith "failed to find method %A" ilMethodSpec
        match ilMethodSpec.EnclosingType with
        | ILType.Void -> failwith "Unsupported type"
        | ILType.Array (ilArrayShape, ilType) -> failwith "Unsupported type"
        | ILType.Value (ilTypeSpec) -> innerExe ilTypeSpec
        | ILType.Boxed (ilTypeSpec) -> innerExe ilTypeSpec
        | ILType.Ptr (ilType) -> failwith "Unsupported type" 
        | ILType.Byref (ilType) -> failwith "Unsupported type"
        | ILType.FunctionPointer (ilCallingSignature) -> failwith "Unsupported type"
        | ILType.TypeVar i -> failwith "Unsupported type"
        | ILType.Modified (required,  ilTypeRef, ilType) -> failwith "Unsupported type"                     

    let execILBlock ilBasicBlock =
        match ilBasicBlock with
        // Basic 
        | AI_add                        -> 
            let v1 = stack.Pop()
            let v2 = stack.Pop()
            match v1, v2 with
            | (:? int as v1), (:? int as v2) -> stack.Push(v1 + v2)
            | _ -> failwith "adding supported combination of types"
            NoResult
        | AI_add_ovf                    -> failwith "Instruction AI_add_ovf unsupported"
        | AI_add_ovf_un                 -> failwith "Instruction AI_add_ovf_un unsupported"
        | AI_and                        -> failwith "Instruction AI_and unsupported"
        | AI_div                        -> failwith "Instruction AI_div unsupported"
        | AI_div_un                     -> failwith "Instruction AI_div_un unsupported"
        | AI_ceq                        -> failwith "Instruction AI_ceq unsupported"
        | AI_cgt                        -> failwith "Instruction AI_cgt unsupported"
        | AI_cgt_un                     -> failwith "Instruction AI_cgt_un unsupported"
        | AI_clt                        -> failwith "Instruction AI_clt unsupported"
        | AI_clt_un                     -> failwith "Instruction AI_clt_un unsupported"
        | AI_conv (ilBasicType)         -> failwith "Instruction AI_conv unsupported"
        | AI_conv_ovf (ilBasicType)     -> failwith "Instruction AI_conv_ovf unsupported"
        | AI_conv_ovf_un (ilBasicType)  -> failwith "Instruction AI_conv_ovf_un unsupported"
        | AI_mul                        -> failwith "Instruction AI_mul unsupported"
        | AI_mul_ovf                    -> failwith "Instruction AI_mul_ovf unsupported"
        | AI_mul_ovf_un                 -> failwith "Instruction AI_mul_ovf_un unsupported"
        | AI_rem                        -> failwith "Instruction AI_rem unsupported"
        | AI_rem_un                     -> failwith "Instruction AI_rem_un unsupported"
        | AI_shl                        -> failwith "Instruction AI_shl unsupported"
        | AI_shr                        -> failwith "Instruction AI_shr unsupported"
        | AI_shr_un                     -> failwith "Instruction AI_shr unsupported"
        | AI_sub                        -> failwith "Instruction AI_sub unsupported"
        | AI_sub_ovf                    -> failwith "Instruction AI_sub_ovf unsupported"
        | AI_sub_ovf_un                 -> failwith "Instruction AI_sub_ovf_un unsupported"
        | AI_xor                        -> failwith "Instruction AI_xor unsupported"
        | AI_or                         -> failwith "Instruction AI_xor unsupported"
        | AI_neg                        -> failwith "Instruction AI_neg unsupported"
        | AI_not                        -> failwith "Instruction AI_not unsupported"
        | AI_ldnull                     ->
            stack.Push null
            NoResult
        | AI_dup                        -> failwith "Instruction AI_dup unsupported"
        | AI_pop                        -> 
            stack.Pop() |> ignore
            NoResult
        | AI_ckfinite                   -> failwith "Instruction AI_ckfinite unsupported"
        | AI_nop                        -> NoResult
        | AI_ldc (ilBasicType, ilConst) -> 
            match ilConst with
            | ILConst.I4 x -> stack.Push x
            | ILConst.I8 x -> stack.Push x
            | ILConst.R4 x -> stack.Push x
            | ILConst.R8 x -> stack.Push x
            NoResult

        | I_ldarg  (i)                                      ->
            stack.Push(parameters.[int i])
            NoResult
        | I_ldarga (i)                                      -> 
            stack.Push (MemoryManagement.getParameterAddress (int i))
            NoResult
        | I_ldind  (ilAlignment, ilVolatility, ilBasicType) -> failwith "Instruction I_ldind unsupported"
        | I_ldloc  (i)                                      ->
            stack.Push(locals.[int i])
            NoResult

        | I_ldloca (i)                                      -> failwith "Instruction I_ldloca unsupported"
        | I_starg  (i)                                      -> failwith "Instruction I_starg unsupported"
        | I_stind  (ilAlignment, ilVolatility, ilBasicType) -> failwith "Instruction I_stind unsupported"
        | I_stloc  (i)                                      ->
            locals.[int i] <- stack.Pop()
            NoResult

        // Control transfer 
        | I_br (ilCodeLabel)                                    -> 
            Branch ilCodeLabel
        | I_jmp (ilMethodSpec)                                  -> failwith "Instruction I_jmp unsupported"
        | I_brcmp (ilComparisonInstr, ilCodeLabel1, ilCodeLabel2) -> 
            let firstBranch =
                match ilComparisonInstr with
                | BI_brfalse | BI_brtrue ->
                    let v1 = stack.Pop()
                    match ilComparisonInstr with
                    | BI_brfalse -> 
                        match v1 with
                        | null -> true
                        | :? bool as v1 when v1 = false -> true 
                        | :? int as v1 when v1 = 0 -> true 
                        | :? int16 as v1 when v1 = 0s -> true
                        | :? int64 as v1 when v1 = 0L -> true
                        | _ -> false 
                    | BI_brtrue ->
                        match v1 with
                        | null -> false
                        | :? bool as v1 when v1 = true -> true 
                        | :? int as v1 when v1 <> 0 -> true 
                        | :? int16 as v1 when v1 <> 0s -> true
                        | :? int64 as v1 when v1 <> 0L -> true
                        | _ -> false 
                    | _ -> failwith "can't happen"
                | _ ->
                    let v1 = stack.Pop()
                    let v2 = stack.Pop()
                    match ilComparisonInstr with
                    | BI_beq -> unbox v1 = unbox v2
                    | BI_bge -> unbox v1 >= unbox v2        
                    | BI_bge_un -> unbox v1 >= unbox v2
                    | BI_bgt -> unbox v1 > unbox v2
                    | BI_bgt_un -> unbox v1 > unbox v2
                    | BI_ble -> unbox v1 <= unbox v2
                    | BI_ble_un -> unbox v1 <= unbox v2
                    | BI_blt -> unbox v1 < unbox v2
                    | BI_blt_un -> unbox v1 < unbox v2
                    | BI_bne_un -> unbox v1 <> unbox v2
                    | _ -> failwith "can't happen"
            if firstBranch then Branch  ilCodeLabel1 else Branch ilCodeLabel2
        | I_switch (ilCodeLabels,ilCodeLabel)                   -> failwith "Instruction I_switch unsupported" (* last label is fallthrough *)
        | I_ret                                                 -> 
            match stack.Count, methodDef.CallingSignature.ReturnType with
            | 0, ILType.Void -> Return null
            | 1, _ -> Return (stack.Pop())
            | _ -> failwith "Error unblanced stack at return instruction"

         // Method call 
        | I_call  (ilTailcall, ilMethodSpec, ilVarArgs) ->
            exeTargetMethod ilMethodSpec None
        | I_callvirt (ilTailcall, ilMethodSpec, ilVarArgs) -> failwith "Instruction I_callvirt unsupported"
        | I_callconstraint (ilTailcall, ilType, ilMethodSpec, ilVarArgs) -> failwith "Instruction I_callconstraint unsupported"
        | I_calli    (ilTailcall, ilCallingSignature, ilVarArgs) -> failwith "Instruction I_calli unsupported"
        | I_ldftn    ilMethodSpec ->  failwith "Instruction I_ldftn unsupported"
        | I_newobj   (ilMethodSpec, ilVarArgs) -> 
            let t, _ = TypeManagement.getType currentModule ilMethodSpec.EnclosingType.TypeSpec
            let newObjRef = MemoryManagement.getBlankObject t
           
            let result = exeTargetMethod ilMethodSpec (Some (box newObjRef))
            result
    
        // Exceptions 
        | I_throw -> failwith "Instruction I_throw unsupported"
        | I_endfinally -> failwith "Instruction I_endfinally unsupported"
        | I_endfilter -> failwith "Instruction I_endfilter unsupported"
        | I_leave ilCodeLabel -> failwith "Instruction I_leave unsupported"
        | I_rethrow -> failwith "Instruction I_rethrow unsupported"

        // Object instructions 
        | I_ldsfld      (ilVolatility, ilFieldSpec) -> 
            let t, tModule = TypeManagement.getType currentModule ilFieldSpec.EnclosingType.TypeSpec
            let fieldValue = MemoryManagement.getStaticField tModule t ilFieldSpec (exeMethod tModule [||] t)
            stack.Push fieldValue
            NoResult
        | I_ldfld       (ilAlignment, ilVolatility, ilFieldSpec) -> failwith "Instruction I_ldfld unsupported"
        | I_ldsflda     (ilFieldSpec) -> 
            let t, tModule = TypeManagement.getType currentModule ilFieldSpec.EnclosingType.TypeSpec
            let fieldAddress = MemoryManagement.getStaticFieldAddress tModule t ilFieldSpec (exeMethod tModule [||] t)
            stack.Push fieldAddress
            NoResult
        | I_ldflda      (ilFieldSpec) -> failwith "Instruction I_ldflda unsupported"
        | I_stsfld      (ilVolatility, ilFieldSpec) -> failwith "Instruction I_stsfld unsupported"
        | I_stfld       (ilAlignment, ilVolatility, ilFieldSpec) ->
            let value = stack.Pop()
            let objRef = stack.Pop() :?> int
            MemoryManagement.storeValueInObjectField objRef value currentModule ilFieldSpec
            NoResult
        | I_ldstr       (stringToLoad) -> failwith "Instruction I_ldstr unsupported"
        | I_isinst      (ilType) -> failwith "Instruction I_isinst unsupported"
        | I_castclass   (ilType) -> failwith "Instruction I_castclass unsupported"
        | I_ldtoken     (ilToken) -> failwith "Instruction I_ldtoken unsupported"
        | I_ldvirtftn   (ilMethodSpec) -> failwith "Instruction I_ldvirtftn unsupported"

        // Value type instructions 
        | I_cpobj       (ilType) -> failwith "Instruction I_cpobj unsupported"
        | I_initobj     (ilType) -> failwith "Instruction I_initobj unsupported"
        | I_ldobj       (ilAlignment, ilVolatility, ilType) -> failwith "Instruction I_ldobj unsupported"
        | I_stobj       (ilAlignment, ilVolatility, ilType) -> failwith "Instruction I_stobj unsupported"
        | I_box         (ilType) -> 
            let value = stack.Pop()
            let addr = MemoryManagement.storeBoxedValue value
            stack.Push addr
            NoResult
        | I_unbox       (ilType) -> failwith "Instruction I_unbox unsupported"
        | I_unbox_any   (ilType) -> failwith "Instruction I_unbox_any unsupported"
        | I_sizeof      (ilType) -> failwith "Instruction I_sizeof unsupported"

        // Generalized array instructions. In AbsIL these instructions include 
        // both the single-dimensional variants (with ILArrayShape == ILArrayShape.SingleDimensional) 
        // and calls to the "special" multi-dimensional "methods" such as 
        //   newobj void string[,]::.ctor(int32, int32) 
        //   call string string[,]::Get(int32, int32) 
        //   call string& string[,]::Address(int32, int32) 
        //   call void string[,]::Set(int32, int32,string) 
        // The IL reader transforms calls of this form to the corresponding 
        // generalized instruction with the corresponding ILArrayShape 
        // argument. This is done to simplify the IL and make it more uniform. 
        // The IL writer then reverses this when emitting the binary. 
        | I_ldelem      (ilBasicType) -> failwith "Instruction I_ldelem unsupported"
        | I_stelem      (ilBasicType) -> failwith "Instruction I_stelem unsupported"
        | I_ldelema     (ilReadonly, whatDoesThisBoolMean, ilArrayShape, ilType)  -> failwith "Instruction I_ldelema unsupported"(* ILArrayShape = ILArrayShape.SingleDimensional for single dimensional arrays *)
        | I_ldelem_any  (ilArrayShap, ilType)  -> failwith "Instruction I_ldelem_any unsupported" (* ILArrayShape = ILArrayShape.SingleDimensional for single dimensional arrays *)
        | I_stelem_any  (ilArrayShape, ilType)  -> failwith "Instruction I_stelem_any unsupported" (* ILArrayShape = ILArrayShape.SingleDimensional for single dimensional arrays *)
        | I_newarr      (ilArrayShape, ilType)  -> failwith "Instruction I_newarr unsupported" (* ILArrayShape = ILArrayShape.SingleDimensional for single dimensional arrays *)
        | I_ldlen -> failwith "Instruction I_ldlen unsupported"

        // "System.TypedReference" related instructions: almost 
        // no languages produce these, though they do occur in mscorlib.dll 
        // System.TypedReference represents a pair of a type and a byref-pointer
        // to a value of that type. 
        | I_mkrefany   (ilType)  ->
            stack.Push ilType // seems wrong
            NoResult
        | I_refanytype   -> failwith "Instruction I_refanytype unsupported"
        | I_refanyval   (ilType)  -> failwith "Instruction I_refanyval unsupported"
    
        // Debug-specific 
        // I_seqpoint is a fake instruction to represent a sequence point: 
        // the next instruction starts the execution of the 
        // statement covered by the given range - this is a 
        // dummy instruction and is not emitted 
        | I_break -> failwith "Instruction I_break unsupported"
        | I_seqpoint ilSourceMarker -> failwith "Instruction I_seqpoint unsupported"

        // Varargs - C++ only 
        | I_arglist  -> failwith "Instruction I_arglist unsupported"

        // Local aggregates, i.e. stack allocated data (alloca) : C++ only 
        | I_localloc -> failwith "Instruction I_localloc unsupported"
        | I_cpblk (ilAlignment, ilVolatility) -> failwith "Instruction I_cpblk unsupported"
        | I_initblk (ilAlignment, ilVolatility) -> failwith "Instruction I_initblk unsupported"

        // EXTENSIONS, e.g. MS-ILX 
        | EI_ilzero (ilType)  -> failwith "Instruction EI_ilzero unsupported"
        | EI_ldlen_multi      (x, y) -> failwith "Instruction EI_ldlen_multi unsupported"
        | I_other    (ilxExtensionInstr) -> failwith "Instruction I_other unsupported" 
    let rec matchLabel label code =
        match code with
        | ILBasicBlock ilBasicBlock -> ilBasicBlock.Label = label
        | GroupBlock (ilDebugMappings, ilCodes) -> ilCodes |> List.exists (fun x -> matchLabel label x)
        | RestrictBlock (ilCodeLabels, ilCode) -> matchLabel label ilCode || ilCodeLabels |> List.exists (fun x -> x = label)
        | TryBlock (ilCode, ilExceptionBlock) -> matchLabel label ilCode
    let rec execCodeBlock code =
        match code with
        | ILBasicBlock ilBasicBlock ->
            let rec loop index =
                let instr = ilBasicBlock.Instructions.[index]
                let result = execILBlock instr
                let nextIndex = index + 1
                match result with
                | Return _ -> result
                | Branch _ -> result
                | ExceptionThrown _ -> result
                | NoResult -> 
                    if ilBasicBlock.Instructions.Length = nextIndex then
                        result
                    else
                        loop nextIndex
            loop 0
        | GroupBlock (ilDebugMappings, ilCodes) -> 
            let firstBlock = List.head ilCodes
            let result = execCodeBlock firstBlock
            match result with
            | Branch targetLabel -> 
                let targetBlock =
                    ilCodes |> List.find (fun code -> matchLabel targetLabel code)
                execCodeBlock targetBlock
            | _ -> result
        | RestrictBlock (ilCodeLabels, ilCode) -> 
            execCodeBlock ilCode
        | TryBlock (ilCode, ilExceptionBlock) -> 
            // TODO error handling :)
            execCodeBlock ilCode
    printfn "%s.%s %A" typeDef.Name methodDef.Name parameters
    if methodDef.IsUnmanagedExport then
        printfn "IsUnmanagedExport"
        NoResult
    elif methodDef.IsInternalCall then
        let result = InternalCalls.execInternalCall methodDef parameters
        stack.Push result
        NoResult
    else
        execCodeBlock methodDef.MethodBody.Code
