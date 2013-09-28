// This is a generated file; the original input is 'C:\code\clrish\src\fsharp\FSComp.txt'
namespace FSComp
            
open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
open Microsoft.FSharp.Reflection
open System.Reflection
// (namespaces below for specific case of using the tool to compile FSharp.Core itself)
open Microsoft.FSharp.Core
open Microsoft.FSharp.Core.Operators
open Microsoft.FSharp.Text
open Microsoft.FSharp.Collections
open Printf

type internal SR private() =
            
    // BEGIN BOILERPLATE        
    static let resources = lazy (new System.Resources.ResourceManager("FSComp", System.Reflection.Assembly.GetExecutingAssembly()))

    static let GetString(name:string) =        
        let s = resources.Value.GetString(name, System.Globalization.CultureInfo.CurrentUICulture)
#if DEBUG
        if null = s then
            System.Diagnostics.Debug.Assert(false, sprintf "**RESOURCE ERROR**: Resource token %s does not exist!" name)
#endif
        s

    static let mkFunctionValue (tys: System.Type[]) (impl:obj->obj) = 
        FSharpValue.MakeFunction(FSharpType.MakeFunctionType(tys.[0],tys.[1]), impl)
        
    static let funTyC = typeof<(obj -> obj)>.GetGenericTypeDefinition()  

    static let isNamedType(ty:System.Type) = not (ty.IsArray ||  ty.IsByRef ||  ty.IsPointer)
    static let isFunctionType (ty1:System.Type)  = 
        isNamedType(ty1) && ty1.IsGenericType && (ty1.GetGenericTypeDefinition()).Equals(funTyC)

    static let rec destFunTy (ty:System.Type) =
        if isFunctionType ty then 
            ty, ty.GetGenericArguments() 
        else
            match ty.BaseType with 
            | null -> failwith "destFunTy: not a function type" 
            | b -> destFunTy b 

    static let buildFunctionForOneArgPat (ty: System.Type) impl = 
        let _,tys = destFunTy ty 
        let rty = tys.[1]
        // PERF: this technique is a bit slow (e.g. in simple cases, like 'sprintf "%x"') 
        mkFunctionValue tys (fun inp -> impl rty inp)
                
    static let capture1 (fmt:string) i args ty (go : obj list -> System.Type -> int -> obj) : obj = 
        match fmt.[i] with
        | '%' -> go args ty (i+1) 
        | 'd'
        | 'f'
        | 's' -> buildFunctionForOneArgPat ty (fun rty n -> go (n::args) rty (i+1))
        | _ -> failwith "bad format specifier"
        
    // newlines and tabs get converted to strings when read from a resource file
    // this will preserve their original intention    
    static let postProcessString (s : string) =
        s.Replace("\\n","\n").Replace("\\t","\t").Replace("\\r","\r").Replace("\\\"", "\"")
        
    static let createMessageString (messageString : string) (fmt : Printf.StringFormat<'T>) : 'T = 
        let fmt = fmt.Value // here, we use the actual error string, as opposed to the one stored as fmt
        let len = fmt.Length 

        /// Function to capture the arguments and then run.
        let rec capture args ty i = 
            if i >= len ||  (fmt.[i] = '%' && i+1 >= len) then 
                let b = new System.Text.StringBuilder()    
                b.AppendFormat(messageString, [| for x in List.rev args -> x |]) |> ignore
                box(b.ToString())
            // REVIEW: For these purposes, this should be a nop, but I'm leaving it
            // in incase we ever decide to support labels for the error format string
            // E.g., "<name>%s<foo>%d"
            elif System.Char.IsSurrogatePair(fmt,i) then 
               capture args ty (i+2)
            else
                match fmt.[i] with
                | '%' ->
                    let i = i+1 
                    capture1 fmt i args ty capture
                | _ ->
                    capture args ty (i+1) 

        (unbox (capture [] (typeof<'T>) 0) : 'T)

    static let mutable swallowResourceText = false
    
    static let GetStringFunc((messageID : string),(fmt : Printf.StringFormat<'T>)) : 'T =
        if swallowResourceText then
            sprintf fmt
        else
            let mutable messageString = GetString(messageID)
            messageString <- postProcessString messageString                            
            createMessageString messageString fmt
        
    /// If set to true, then all error messages will just return the filled 'holes' delimited by ',,,'s - this is for language-neutral testing (e.g. localization-invariant baselines).
    static member SwallowResourceText with get () = swallowResourceText
                                      and set (b) = swallowResourceText <- b
    // END BOILERPLATE        
    
    /// The namespace '%s' is not defined
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:2)
    static member undefinedNameNamespace(a0 : System.String) = (GetStringFunc("undefinedNameNamespace",",,,%s,,,") a0)
    /// The namespace or module '%s' is not defined
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:3)
    static member undefinedNameNamespaceOrModule(a0 : System.String) = (GetStringFunc("undefinedNameNamespaceOrModule",",,,%s,,,") a0)
    /// The field, constructor or member '%s' is not defined
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:4)
    static member undefinedNameFieldConstructorOrMember(a0 : System.String) = (GetStringFunc("undefinedNameFieldConstructorOrMember",",,,%s,,,") a0)
    /// The value, constructor, namespace or type '%s' is not defined
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:5)
    static member undefinedNameValueConstructorNamespaceOrType(a0 : System.String) = (GetStringFunc("undefinedNameValueConstructorNamespaceOrType",",,,%s,,,") a0)
    /// The value or constructor '%s' is not defined
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:6)
    static member undefinedNameValueOfConstructor(a0 : System.String) = (GetStringFunc("undefinedNameValueOfConstructor",",,,%s,,,") a0)
    /// The value, namespace, type or module '%s' is not defined
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:7)
    static member undefinedNameValueNamespaceTypeOrModule(a0 : System.String) = (GetStringFunc("undefinedNameValueNamespaceTypeOrModule",",,,%s,,,") a0)
    /// The constructor, module or namespace '%s' is not defined
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:8)
    static member undefinedNameConstructorModuleOrNamespace(a0 : System.String) = (GetStringFunc("undefinedNameConstructorModuleOrNamespace",",,,%s,,,") a0)
    /// The type '%s' is not defined
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:9)
    static member undefinedNameType(a0 : System.String) = (GetStringFunc("undefinedNameType",",,,%s,,,") a0)
    /// The record label or namespace '%s' is not defined
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:10)
    static member undefinedNameRecordLabelOrNamespace(a0 : System.String) = (GetStringFunc("undefinedNameRecordLabelOrNamespace",",,,%s,,,") a0)
    /// The record label '%s' is not defined
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:11)
    static member undefinedNameRecordLabel(a0 : System.String) = (GetStringFunc("undefinedNameRecordLabel",",,,%s,,,") a0)
    /// The type parameter '%s' is not defined
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:12)
    static member undefinedNameTypeParameter(a0 : System.String) = (GetStringFunc("undefinedNameTypeParameter",",,,%s,,,") a0)
    /// The pattern discriminator '%s' is not defined
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:13)
    static member undefinedNamePatternDiscriminator(a0 : System.String) = (GetStringFunc("undefinedNamePatternDiscriminator",",,,%s,,,") a0)
    /// The non-generic type '%s' does not expect any type arguments, but here is given %d type argument(s)
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:17)
    static member buildUnexpectedTypeArgs(a0 : System.String, a1 : System.Int32) = (GetStringFunc("buildUnexpectedTypeArgs",",,,%s,,,%d,,,") a0 a1)
    /// Invalid warning number '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:18)
    static member buildInvalidWarningNumber(a0 : System.String) = (203, GetStringFunc("buildInvalidWarningNumber",",,,%s,,,") a0)
    /// Invalid version string '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:19)
    static member buildInvalidVersionString(a0 : System.String) = (204, GetStringFunc("buildInvalidVersionString",",,,%s,,,") a0)
    /// Invalid version file '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:20)
    static member buildInvalidVersionFile(a0 : System.String) = (205, GetStringFunc("buildInvalidVersionFile",",,,%s,,,") a0)
    /// F# Compiler for F# 3.0 %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:21)
    static member buildProductName(a0 : System.String) = (GetStringFunc("buildProductName",",,,%s,,,") a0)
    /// Problem with filename '%s': %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:22)
    static member buildProblemWithFilename(a0 : System.String, a1 : System.String) = (206, GetStringFunc("buildProblemWithFilename",",,,%s,,,%s,,,") a0 a1)
    /// No inputs specified
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:23)
    static member buildNoInputsSpecified() = (207, GetStringFunc("buildNoInputsSpecified",",,,") )
    /// The output name extension doesn't match the options used. If '-a' or '--target:library' is used the output file name must end with '.dll', if '--target:module' is used the output extension must be '.netmodule', otherwise '.exe'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:24)
    static member buildMismatchOutputExtension() = (208, GetStringFunc("buildMismatchOutputExtension",",,,") )
    /// The '--pdb' option requires the '--debug' option to be used
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:25)
    static member buildPdbRequiresDebug() = (209, GetStringFunc("buildPdbRequiresDebug",",,,") )
    /// The search directory '%s' is invalid
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:26)
    static member buildInvalidSearchDirectory(a0 : System.String) = (210, GetStringFunc("buildInvalidSearchDirectory",",,,%s,,,") a0)
    /// The search directory '%s' could not be found
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:27)
    static member buildSearchDirectoryNotFound(a0 : System.String) = (211, GetStringFunc("buildSearchDirectoryNotFound",",,,%s,,,") a0)
    /// '%s' is not a valid filename
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:28)
    static member buildInvalidFilename(a0 : System.String) = (212, GetStringFunc("buildInvalidFilename",",,,%s,,,") a0)
    /// '%s' is not a valid assembly name
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:29)
    static member buildInvalidAssemblyName(a0 : System.String) = (213, GetStringFunc("buildInvalidAssemblyName",",,,%s,,,") a0)
    /// Unrecognized privacy setting '%s' for managed resource, valid options are 'public' and 'private'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:30)
    static member buildInvalidPrivacy(a0 : System.String) = (214, GetStringFunc("buildInvalidPrivacy",",,,%s,,,") a0)
    /// Multiple references to '%s.dll' are not permitted
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:31)
    static member buildMultipleReferencesNotAllowed(a0 : System.String) = (215, GetStringFunc("buildMultipleReferencesNotAllowed",",,,%s,,,") a0)
    /// The file '%s' is a CLI 1.x version of mscorlib. F# requires CLI version 2.0 or greater.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:32)
    static member buildRequiresCLI2(a0 : System.String) = (216, GetStringFunc("buildRequiresCLI2",",,,%s,,,") a0)
    /// Could not read version from mscorlib.dll
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:33)
    static member buildCouldNotReadVersionInfoFromMscorlib() = (GetStringFunc("buildCouldNotReadVersionInfoFromMscorlib",",,,") )
    /// The referenced or default base CLI library 'mscorlib' is binary-incompatible with the referenced library '%s'. Consider recompiling the library or making an explicit reference to a version of this library that matches the CLI version you are using.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:34)
    static member buildMscorlibAndReferencedAssemblyMismatch(a0 : System.String) = (217, GetStringFunc("buildMscorlibAndReferencedAssemblyMismatch",",,,%s,,,") a0)
    /// Unable to read assembly '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:35)
    static member buildCannotReadAssembly(a0 : System.String) = (218, GetStringFunc("buildCannotReadAssembly",",,,%s,,,") a0)
    /// The referenced or default base CLI library 'mscorlib' is binary-incompatible with the referenced F# core library '%s'. Consider recompiling the library or making an explicit reference to a version of this library that matches the CLI version you are using.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:36)
    static member buildMscorLibAndFSharpCoreMismatch(a0 : System.String) = (219, GetStringFunc("buildMscorLibAndFSharpCoreMismatch",",,,%s,,,") a0)
    /// Assembly resolution failure at or near this location
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:37)
    static member buildAssemblyResolutionFailed() = (220, GetStringFunc("buildAssemblyResolutionFailed",",,,") )
    /// The declarations in this file will be placed in an implicit module '%s' based on the file name '%s'. However this is not a valid F# identifier, so the contents will not be accessible from other files. Consider renaming the file or adding a 'module' or 'namespace' declaration at the top of the file.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:38)
    static member buildImplicitModuleIsNotLegalIdentifier(a0 : System.String, a1 : System.String) = (221, GetStringFunc("buildImplicitModuleIsNotLegalIdentifier",",,,%s,,,%s,,,") a0 a1)
    /// Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:39)
    static member buildMultiFileRequiresNamespaceOrModule() = (222, GetStringFunc("buildMultiFileRequiresNamespaceOrModule",",,,") )
    /// This file contains multiple declarations of the form 'module SomeNamespace.SomeModule'. Only one declaration of this form is permitted in a file. Change your file to use an initial namespace declaration and/or use 'module ModuleName = ...' to define your modules.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:40)
    static member buildMultipleToplevelModules() = (223, GetStringFunc("buildMultipleToplevelModules",",,,") )
    /// ParseInput: unknown file suffix for '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:41)
    static member buildUnknownFileSuffix(a0 : System.String) = (GetStringFunc("buildUnknownFileSuffix",",,,%s,,,") a0)
    /// Option requires parameter: %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:42)
    static member buildOptionRequiresParameter(a0 : System.String) = (224, GetStringFunc("buildOptionRequiresParameter",",,,%s,,,") a0)
    /// Source file '%s' could not be found
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:43)
    static member buildCouldNotFindSourceFile(a0 : System.String) = (225, GetStringFunc("buildCouldNotFindSourceFile",",,,%s,,,") a0)
    /// The file extension of '%s' is not recognized. Source files must have extension .fs, .fsi, .fsx, .fsscript, .ml or .mli.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:44)
    static member buildInvalidSourceFileExtension(a0 : System.String) = (226, GetStringFunc("buildInvalidSourceFileExtension",",,,%s,,,") a0)
    /// Could not resolve assembly '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:45)
    static member buildCouldNotResolveAssembly(a0 : System.String) = (227, GetStringFunc("buildCouldNotResolveAssembly",",,,%s,,,") a0)
    /// Could not resolve assembly '%s' required by '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:46)
    static member buildCouldNotResolveAssemblyRequiredByFile(a0 : System.String, a1 : System.String) = (228, GetStringFunc("buildCouldNotResolveAssemblyRequiredByFile",",,,%s,,,%s,,,") a0 a1)
    /// Error opening binary file '%s': %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:47)
    static member buildErrorOpeningBinaryFile(a0 : System.String, a1 : System.String) = (229, GetStringFunc("buildErrorOpeningBinaryFile",",,,%s,,,%s,,,") a0 a1)
    /// The F#-compiled DLL '%s' needs to be recompiled to be used with this version of F#
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:48)
    static member buildDifferentVersionMustRecompile(a0 : System.String) = (231, GetStringFunc("buildDifferentVersionMustRecompile",",,,%s,,,") a0)
    /// Invalid directive. Expected '#I \"<path>\"'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:49)
    static member buildInvalidHashIDirective() = (232, GetStringFunc("buildInvalidHashIDirective",",,,") )
    /// Invalid directive. Expected '#r \"<file-or-assembly>\"'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:50)
    static member buildInvalidHashrDirective() = (233, GetStringFunc("buildInvalidHashrDirective",",,,") )
    /// Invalid directive. Expected '#load \"<file>\" ... \"<file>\"'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:51)
    static member buildInvalidHashloadDirective() = (234, GetStringFunc("buildInvalidHashloadDirective",",,,") )
    /// Invalid directive. Expected '#time', '#time \"on\"' or '#time \"off\"'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:52)
    static member buildInvalidHashtimeDirective() = (235, GetStringFunc("buildInvalidHashtimeDirective",",,,") )
    /// Directives inside modules are ignored
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:53)
    static member buildDirectivesInModulesAreIgnored() = (236, GetStringFunc("buildDirectivesInModulesAreIgnored",",,,") )
    /// A signature for the file or module '%s' has already been specified
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:54)
    static member buildSignatureAlreadySpecified(a0 : System.String) = (237, GetStringFunc("buildSignatureAlreadySpecified",",,,%s,,,") a0)
    /// An implementation of file or module '%s' has already been given. Compilation order is significant in F# because of type inference. You may need to adjust the order of your files to place the signature file before the implementation. In Visual Studio files are type-checked in the order they appear in the project file, which can be edited manually or adjusted using the solution explorer.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:55)
    static member buildImplementationAlreadyGivenDetail(a0 : System.String) = (238, GetStringFunc("buildImplementationAlreadyGivenDetail",",,,%s,,,") a0)
    /// An implementation of the file or module '%s' has already been given
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:56)
    static member buildImplementationAlreadyGiven(a0 : System.String) = (239, GetStringFunc("buildImplementationAlreadyGiven",",,,%s,,,") a0)
    /// The signature file '%s' does not have a corresponding implementation file. If an implementation file exists then check the 'module' and 'namespace' declarations in the signature and implementation files match.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:57)
    static member buildSignatureWithoutImplementation(a0 : System.String) = (240, GetStringFunc("buildSignatureWithoutImplementation",",,,%s,,,") a0)
    /// '%s' is not a valid integer argument
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:58)
    static member buildArgInvalidInt(a0 : System.String) = (241, GetStringFunc("buildArgInvalidInt",",,,%s,,,") a0)
    /// '%s' is not a valid floating point argument
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:59)
    static member buildArgInvalidFloat(a0 : System.String) = (242, GetStringFunc("buildArgInvalidFloat",",,,%s,,,") a0)
    /// Unrecognized option: '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:60)
    static member buildUnrecognizedOption(a0 : System.String) = (243, GetStringFunc("buildUnrecognizedOption",",,,%s,,,") a0)
    /// Invalid module or namespace name
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:61)
    static member buildInvalidModuleOrNamespaceName() = (244, GetStringFunc("buildInvalidModuleOrNamespaceName",",,,") )
    /// Error reading/writing metadata for the F# compiled DLL '%s'. Was the DLL compiled with an earlier version of the F# compiler? (error: '%s').
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:65)
    static member pickleErrorReadingWritingMetadata(a0 : System.String, a1 : System.String) = (GetStringFunc("pickleErrorReadingWritingMetadata",",,,%s,,,%s,,,") a0 a1)
    /// The type/module '%s' is not a concrete module or type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:69)
    static member tastTypeOrModuleNotConcrete(a0 : System.String) = (245, GetStringFunc("tastTypeOrModuleNotConcrete",",,,%s,,,") a0)
    /// The type '%s' has an inline assembly code representation
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:70)
    static member tastTypeHasAssemblyCodeRepresentation(a0 : System.String) = (GetStringFunc("tastTypeHasAssemblyCodeRepresentation",",,,%s,,,") a0)
    /// A namespace and a module named '%s' both occur in two parts of this assembly
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:71)
    static member tastNamespaceAndModuleWithSameNameInAssembly(a0 : System.String) = (247, GetStringFunc("tastNamespaceAndModuleWithSameNameInAssembly",",,,%s,,,") a0)
    /// Two modules named '%s' occur in two parts of this assembly
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:72)
    static member tastTwoModulesWithSameNameInAssembly(a0 : System.String) = (248, GetStringFunc("tastTwoModulesWithSameNameInAssembly",",,,%s,,,") a0)
    /// Two type definitions named '%s' occur in namespace '%s' in two parts of this assembly
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:73)
    static member tastDuplicateTypeDefinitionInAssembly(a0 : System.String, a1 : System.String) = (249, GetStringFunc("tastDuplicateTypeDefinitionInAssembly",",,,%s,,,%s,,,") a0 a1)
    /// A module and a type definition named '%s' occur in namespace '%s' in two parts of this assembly
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:74)
    static member tastConflictingModuleAndTypeDefinitionInAssembly(a0 : System.String, a1 : System.String) = (250, GetStringFunc("tastConflictingModuleAndTypeDefinitionInAssembly",",,,%s,,,%s,,,") a0 a1)
    /// Invalid member signature encountered because of an earlier error
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:78)
    static member tastInvalidMemberSignature() = (251, GetStringFunc("tastInvalidMemberSignature",",,,") )
    /// This value does not have a valid property setter type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:79)
    static member tastValueDoesNotHaveSetterType() = (252, GetStringFunc("tastValueDoesNotHaveSetterType",",,,") )
    /// Invalid form for a property getter. At least one '()' argument is required when using the explicit syntax.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:80)
    static member tastInvalidFormForPropertyGetter() = (253, GetStringFunc("tastInvalidFormForPropertyGetter",",,,") )
    /// Invalid form for a property setter. At least one argument is required.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:81)
    static member tastInvalidFormForPropertySetter() = (254, GetStringFunc("tastInvalidFormForPropertySetter",",,,") )
    /// Unexpected use of a byref-typed variable
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:82)
    static member tastUnexpectedByRef() = (255, GetStringFunc("tastUnexpectedByRef",",,,") )
    /// A value must be mutable in order to mutate the contents or take the address of a value type, e.g. 'let mutable x = ...'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:83)
    static member tastValueMustBeLocalAndMutable() = (256, GetStringFunc("tastValueMustBeLocalAndMutable",",,,") )
    /// Invalid mutation of a constant expression. Consider copying the expression to a mutable local, e.g. 'let mutable x = ...'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:84)
    static member tastInvalidMutationOfConstant() = (257, GetStringFunc("tastInvalidMutationOfConstant",",,,") )
    /// The value has been copied to ensure the original is not mutated by this operation
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:85)
    static member tastValueHasBeenCopied() = (GetStringFunc("tastValueHasBeenCopied",",,,") )
    /// Recursively defined values cannot appear directly as part of the construction of a tuple value within a recursive binding
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:86)
    static member tastRecursiveValuesMayNotBeInConstructionOfTuple() = (259, GetStringFunc("tastRecursiveValuesMayNotBeInConstructionOfTuple",",,,") )
    /// Recursive values cannot appear directly as a construction of the type '%s' within a recursive binding. This feature has been removed from the F# language. Consider using a record instead.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:87)
    static member tastRecursiveValuesMayNotAppearInConstructionOfType(a0 : System.String) = (260, GetStringFunc("tastRecursiveValuesMayNotAppearInConstructionOfType",",,,%s,,,") a0)
    /// Recursive values cannot be directly assigned to the non-mutable field '%s' of the type '%s' within a recursive binding. Consider using a mutable field instead.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:88)
    static member tastRecursiveValuesMayNotBeAssignedToNonMutableField(a0 : System.String, a1 : System.String) = (261, GetStringFunc("tastRecursiveValuesMayNotBeAssignedToNonMutableField",",,,%s,,,%s,,,") a0 a1)
    /// Unexpected decode of AutoOpenAttribute
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:89)
    static member tastUnexpectedDecodeOfAutoOpenAttribute() = (GetStringFunc("tastUnexpectedDecodeOfAutoOpenAttribute",",,,") )
    /// Unexpected decode of InternalsVisibleToAttribute
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:90)
    static member tastUnexpectedDecodeOfInternalsVisibleToAttribute() = (GetStringFunc("tastUnexpectedDecodeOfInternalsVisibleToAttribute",",,,") )
    /// Unexpected decode of InterfaceDataVersionAttribute
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:91)
    static member tastUnexpectedDecodeOfInterfaceDataVersionAttribute() = (GetStringFunc("tastUnexpectedDecodeOfInterfaceDataVersionAttribute",",,,") )
    /// Active patterns cannot return more than 7 possibilities
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:92)
    static member tastActivePatternsLimitedToSeven() = (265, GetStringFunc("tastActivePatternsLimitedToSeven",",,,") )
    /// This constant cannot be used as a custom attribute value
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:93)
    static member tastConstantCannotBeCustomAttribute() = (266, GetStringFunc("tastConstantCannotBeCustomAttribute",",,,") )
    /// This is not a constant expression or valid custom attribute value
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:94)
    static member tastNotAConstantExpression() = (267, GetStringFunc("tastNotAConstantExpression",",,,") )
    /// Module '%s' contains\n    %s    \nbut its signature specifies\n    %s    \nThe mutability attributes differ
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:98)
    static member ValueNotContainedMutabilityAttributesDiffer(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("ValueNotContainedMutabilityAttributesDiffer",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// Module '%s' contains\n    %s    \nbut its signature specifies\n    %s    \nThe names differ
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:99)
    static member ValueNotContainedMutabilityNamesDiffer(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("ValueNotContainedMutabilityNamesDiffer",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// Module '%s' contains\n    %s    \nbut its signature specifies\n    %s    \nThe compiled names differ
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:100)
    static member ValueNotContainedMutabilityCompiledNamesDiffer(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("ValueNotContainedMutabilityCompiledNamesDiffer",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// Module '%s' contains\n    %s    \nbut its signature specifies\n    %s    \nThe display names differ
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:101)
    static member ValueNotContainedMutabilityDisplayNamesDiffer(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("ValueNotContainedMutabilityDisplayNamesDiffer",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// Module '%s' contains\n    %s    \nbut its signature specifies\n    %s    \nThe accessibility specified in the signature is more than that specified in the implementation
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:102)
    static member ValueNotContainedMutabilityAccessibilityMore(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("ValueNotContainedMutabilityAccessibilityMore",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// Module '%s' contains\n    %s    \nbut its signature specifies\n    %s    \nThe inline flags differ
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:103)
    static member ValueNotContainedMutabilityInlineFlagsDiffer(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("ValueNotContainedMutabilityInlineFlagsDiffer",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// Module '%s' contains\n    %s    \nbut its signature specifies\n    %s    \nThe literal constant values and/or attributes differ
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:104)
    static member ValueNotContainedMutabilityLiteralConstantValuesDiffer(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("ValueNotContainedMutabilityLiteralConstantValuesDiffer",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// Module '%s' contains\n    %s    \nbut its signature specifies\n    %s    \nOne is a type function and the other is not. The signature requires explicit type parameters if they are present in the implementation.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:105)
    static member ValueNotContainedMutabilityOneIsTypeFunction(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("ValueNotContainedMutabilityOneIsTypeFunction",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// Module '%s' contains\n    %s    \nbut its signature specifies\n    %s    \nThe respective type parameter counts differ
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:106)
    static member ValueNotContainedMutabilityParameterCountsDiffer(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("ValueNotContainedMutabilityParameterCountsDiffer",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// Module '%s' contains\n    %s    \nbut its signature specifies\n    %s    \nThe types differ
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:107)
    static member ValueNotContainedMutabilityTypesDiffer(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("ValueNotContainedMutabilityTypesDiffer",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// Module '%s' contains\n    %s    \nbut its signature specifies\n    %s    \nOne is an extension member and the other is not
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:108)
    static member ValueNotContainedMutabilityExtensionsDiffer(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("ValueNotContainedMutabilityExtensionsDiffer",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// Module '%s' contains\n    %s    \nbut its signature specifies\n    %s    \nAn arity was not inferred for this value
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:109)
    static member ValueNotContainedMutabilityArityNotInferred(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("ValueNotContainedMutabilityArityNotInferred",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// Module '%s' contains\n    %s    \nbut its signature specifies\n    %s    \nThe number of generic parameters in the signature and implementation differ (the signature declares %s but the implementation declares %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:110)
    static member ValueNotContainedMutabilityGenericParametersDiffer(a0 : System.String, a1 : System.String, a2 : System.String, a3 : System.String, a4 : System.String) = (GetStringFunc("ValueNotContainedMutabilityGenericParametersDiffer",",,,%s,,,%s,,,%s,,,%s,,,%s,,,") a0 a1 a2 a3 a4)
    /// Module '%s' contains\n    %s    \nbut its signature specifies\n    %s    \nThe generic parameters in the signature and implementation have different kinds. Perhaps there is a missing [<Measure>] attribute.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:111)
    static member ValueNotContainedMutabilityGenericParametersAreDifferentKinds(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("ValueNotContainedMutabilityGenericParametersAreDifferentKinds",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// Module '%s' contains\n    %s    \nbut its signature specifies\n    %s    \nThe arities in the signature and implementation differ. The signature specifies that '%s' is function definition or lambda expression accepting at least %s argument(s), but the implementation is a computed function value. To declare that a computed function value is a permitted implementation simply parenthesize its type in the signature, e.g.\n\tval %s: int -> (int -> int)\ninstead of\n\tval %s: int -> int -> int.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:112)
    static member ValueNotContainedMutabilityAritiesDiffer(a0 : System.String, a1 : System.String, a2 : System.String, a3 : System.String, a4 : System.String, a5 : System.String, a6 : System.String) = (GetStringFunc("ValueNotContainedMutabilityAritiesDiffer",",,,%s,,,%s,,,%s,,,%s,,,%s,,,%s,,,%s,,,") a0 a1 a2 a3 a4 a5 a6)
    /// Module '%s' contains\n    %s    \nbut its signature specifies\n    %s    \nThe CLI member names differ
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:113)
    static member ValueNotContainedMutabilityDotNetNamesDiffer(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("ValueNotContainedMutabilityDotNetNamesDiffer",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// Module '%s' contains\n    %s    \nbut its signature specifies\n    %s    \nOne is static and the other isn't
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:114)
    static member ValueNotContainedMutabilityStaticsDiffer(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("ValueNotContainedMutabilityStaticsDiffer",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// Module '%s' contains\n    %s    \nbut its signature specifies\n    %s    \nOne is virtual and the other isn't
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:115)
    static member ValueNotContainedMutabilityVirtualsDiffer(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("ValueNotContainedMutabilityVirtualsDiffer",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// Module '%s' contains\n    %s    \nbut its signature specifies\n    %s    \nOne is abstract and the other isn't
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:116)
    static member ValueNotContainedMutabilityAbstractsDiffer(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("ValueNotContainedMutabilityAbstractsDiffer",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// Module '%s' contains\n    %s    \nbut its signature specifies\n    %s    \nOne is final and the other isn't
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:117)
    static member ValueNotContainedMutabilityFinalsDiffer(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("ValueNotContainedMutabilityFinalsDiffer",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// Module '%s' contains\n    %s    \nbut its signature specifies\n    %s    \nOne is marked as an override and the other isn't
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:118)
    static member ValueNotContainedMutabilityOverridesDiffer(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("ValueNotContainedMutabilityOverridesDiffer",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// Module '%s' contains\n    %s    \nbut its signature specifies\n    %s    \nOne is a constructor/property and the other is not
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:119)
    static member ValueNotContainedMutabilityOneIsConstructor(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("ValueNotContainedMutabilityOneIsConstructor",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// Module '%s' contains\n    %s    \nbut its signature specifies\n    %s    \nThe compiled representation of this method is as a static member but the signature indicates its compiled representation is as an instance member
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:120)
    static member ValueNotContainedMutabilityStaticButInstance(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("ValueNotContainedMutabilityStaticButInstance",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// Module '%s' contains\n    %s    \nbut its signature specifies\n    %s    \nThe compiled representation of this method is as an instance member, but the signature indicates its compiled representation is as a static member
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:121)
    static member ValueNotContainedMutabilityInstanceButStatic(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("ValueNotContainedMutabilityInstanceButStatic",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// The %s definitions in the signature and implementation are not compatible because the names differ
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:122)
    static member DefinitionsInSigAndImplNotCompatibleNamesDiffer(a0 : System.String) = (290, GetStringFunc("DefinitionsInSigAndImplNotCompatibleNamesDiffer",",,,%s,,,") a0)
    /// The %s definitions in the signature and implementation are not compatible because the respective type parameter counts differ
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:123)
    static member DefinitionsInSigAndImplNotCompatibleParameterCountsDiffer(a0 : System.String) = (291, GetStringFunc("DefinitionsInSigAndImplNotCompatibleParameterCountsDiffer",",,,%s,,,") a0)
    /// The %s definitions in the signature and implementation are not compatible because the accessibility specified in the signature is more than that specified in the implementation
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:124)
    static member DefinitionsInSigAndImplNotCompatibleAccessibilityDiffer(a0 : System.String) = (292, GetStringFunc("DefinitionsInSigAndImplNotCompatibleAccessibilityDiffer",",,,%s,,,") a0)
    /// The %s definitions in the signature and implementation are not compatible because the signature requires that the type supports the interface %s but the interface has not been implemented
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:125)
    static member DefinitionsInSigAndImplNotCompatibleMissingInterface(a0 : System.String, a1 : System.String) = (293, GetStringFunc("DefinitionsInSigAndImplNotCompatibleMissingInterface",",,,%s,,,%s,,,") a0 a1)
    /// The %s definitions in the signature and implementation are not compatible because the implementation says this type may use nulls as a representation but the signature does not
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:126)
    static member DefinitionsInSigAndImplNotCompatibleImplementationSaysNull(a0 : System.String) = (294, GetStringFunc("DefinitionsInSigAndImplNotCompatibleImplementationSaysNull",",,,%s,,,") a0)
    /// The %s definitions in the signature and implementation are not compatible because the implementation says this type may use nulls as an extra value but the signature does not
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:127)
    static member DefinitionsInSigAndImplNotCompatibleImplementationSaysNull2(a0 : System.String) = (294, GetStringFunc("DefinitionsInSigAndImplNotCompatibleImplementationSaysNull2",",,,%s,,,") a0)
    /// The %s definitions in the signature and implementation are not compatible because the signature says this type may use nulls as a representation but the implementation does not
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:128)
    static member DefinitionsInSigAndImplNotCompatibleSignatureSaysNull(a0 : System.String) = (295, GetStringFunc("DefinitionsInSigAndImplNotCompatibleSignatureSaysNull",",,,%s,,,") a0)
    /// The %s definitions in the signature and implementation are not compatible because the signature says this type may use nulls as an extra value but the implementation does not
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:129)
    static member DefinitionsInSigAndImplNotCompatibleSignatureSaysNull2(a0 : System.String) = (295, GetStringFunc("DefinitionsInSigAndImplNotCompatibleSignatureSaysNull2",",,,%s,,,") a0)
    /// The %s definitions in the signature and implementation are not compatible because the implementation type is sealed but the signature implies it is not. Consider adding the [<Sealed>] attribute to the signature.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:130)
    static member DefinitionsInSigAndImplNotCompatibleImplementationSealed(a0 : System.String) = (296, GetStringFunc("DefinitionsInSigAndImplNotCompatibleImplementationSealed",",,,%s,,,") a0)
    /// The %s definitions in the signature and implementation are not compatible because the implementation type is not sealed but signature implies it is. Consider adding the [<Sealed>] attribute to the implementation.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:131)
    static member DefinitionsInSigAndImplNotCompatibleImplementationIsNotSealed(a0 : System.String) = (297, GetStringFunc("DefinitionsInSigAndImplNotCompatibleImplementationIsNotSealed",",,,%s,,,") a0)
    /// The %s definitions in the signature and implementation are not compatible because the implementation is an abstract class but the signature is not. Consider adding the [<AbstractClass>] attribute to the signature.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:132)
    static member DefinitionsInSigAndImplNotCompatibleImplementationIsAbstract(a0 : System.String) = (298, GetStringFunc("DefinitionsInSigAndImplNotCompatibleImplementationIsAbstract",",,,%s,,,") a0)
    /// The %s definitions in the signature and implementation are not compatible because the signature is an abstract class but the implementation is not. Consider adding the [<AbstractClass>] attribute to the implementation.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:133)
    static member DefinitionsInSigAndImplNotCompatibleSignatureIsAbstract(a0 : System.String) = (299, GetStringFunc("DefinitionsInSigAndImplNotCompatibleSignatureIsAbstract",",,,%s,,,") a0)
    /// The %s definitions in the signature and implementation are not compatible because the types have different base types
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:134)
    static member DefinitionsInSigAndImplNotCompatibleTypesHaveDifferentBaseTypes(a0 : System.String) = (300, GetStringFunc("DefinitionsInSigAndImplNotCompatibleTypesHaveDifferentBaseTypes",",,,%s,,,") a0)
    /// The %s definitions in the signature and implementation are not compatible because the number of %ss differ
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:135)
    static member DefinitionsInSigAndImplNotCompatibleNumbersDiffer(a0 : System.String, a1 : System.String) = (301, GetStringFunc("DefinitionsInSigAndImplNotCompatibleNumbersDiffer",",,,%s,,,%s,,,") a0 a1)
    /// The %s definitions in the signature and implementation are not compatible because the signature defines the %s '%s' but the implementation does not (or does, but not in the same order)
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:136)
    static member DefinitionsInSigAndImplNotCompatibleSignatureDefinesButImplDoesNot(a0 : System.String, a1 : System.String, a2 : System.String) = (302, GetStringFunc("DefinitionsInSigAndImplNotCompatibleSignatureDefinesButImplDoesNot",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// The %s definitions in the signature and implementation are not compatible because the implementation defines the %s '%s' but the signature does not (or does, but not in the same order)
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:137)
    static member DefinitionsInSigAndImplNotCompatibleImplDefinesButSignatureDoesNot(a0 : System.String, a1 : System.String, a2 : System.String) = (303, GetStringFunc("DefinitionsInSigAndImplNotCompatibleImplDefinesButSignatureDoesNot",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// The %s definitions in the signature and implementation are not compatible because the implementation defines a struct but the signature defines a type with a hidden representation
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:138)
    static member DefinitionsInSigAndImplNotCompatibleImplDefinesStruct(a0 : System.String) = (304, GetStringFunc("DefinitionsInSigAndImplNotCompatibleImplDefinesStruct",",,,%s,,,") a0)
    /// The %s definitions in the signature and implementation are not compatible because a CLI type representation is being hidden by a signature
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:139)
    static member DefinitionsInSigAndImplNotCompatibleDotNetTypeRepresentationIsHidden(a0 : System.String) = (305, GetStringFunc("DefinitionsInSigAndImplNotCompatibleDotNetTypeRepresentationIsHidden",",,,%s,,,") a0)
    /// The %s definitions in the signature and implementation are not compatible because a type representation is being hidden by a signature
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:140)
    static member DefinitionsInSigAndImplNotCompatibleTypeIsHidden(a0 : System.String) = (306, GetStringFunc("DefinitionsInSigAndImplNotCompatibleTypeIsHidden",",,,%s,,,") a0)
    /// The %s definitions in the signature and implementation are not compatible because the types are of different kinds
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:141)
    static member DefinitionsInSigAndImplNotCompatibleTypeIsDifferentKind(a0 : System.String) = (307, GetStringFunc("DefinitionsInSigAndImplNotCompatibleTypeIsDifferentKind",",,,%s,,,") a0)
    /// The %s definitions in the signature and implementation are not compatible because the IL representations differ
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:142)
    static member DefinitionsInSigAndImplNotCompatibleILDiffer(a0 : System.String) = (308, GetStringFunc("DefinitionsInSigAndImplNotCompatibleILDiffer",",,,%s,,,") a0)
    /// The %s definitions in the signature and implementation are not compatible because the representations differ
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:143)
    static member DefinitionsInSigAndImplNotCompatibleRepresentationsDiffer(a0 : System.String) = (309, GetStringFunc("DefinitionsInSigAndImplNotCompatibleRepresentationsDiffer",",,,%s,,,") a0)
    /// The %s definitions in the signature and implementation are not compatible because the field %s was present in the implementation but not in the signature
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:144)
    static member DefinitionsInSigAndImplNotCompatibleFieldWasPresent(a0 : System.String, a1 : System.String) = (311, GetStringFunc("DefinitionsInSigAndImplNotCompatibleFieldWasPresent",",,,%s,,,%s,,,") a0 a1)
    /// The %s definitions in the signature and implementation are not compatible because the order of the fields is different in the signature and implementation
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:145)
    static member DefinitionsInSigAndImplNotCompatibleFieldOrderDiffer(a0 : System.String) = (312, GetStringFunc("DefinitionsInSigAndImplNotCompatibleFieldOrderDiffer",",,,%s,,,") a0)
    /// The %s definitions in the signature and implementation are not compatible because the field %s was required by the signature but was not specified by the implementation
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:146)
    static member DefinitionsInSigAndImplNotCompatibleFieldRequiredButNotSpecified(a0 : System.String, a1 : System.String) = (313, GetStringFunc("DefinitionsInSigAndImplNotCompatibleFieldRequiredButNotSpecified",",,,%s,,,%s,,,") a0 a1)
    /// The %s definitions in the signature and implementation are not compatible because the field '%s' was present in the implementation but not in the signature. Struct types must now reveal their fields in the signature for the type, though the fields may still be labelled 'private' or 'internal'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:147)
    static member DefinitionsInSigAndImplNotCompatibleFieldIsInImplButNotSig(a0 : System.String, a1 : System.String) = (314, GetStringFunc("DefinitionsInSigAndImplNotCompatibleFieldIsInImplButNotSig",",,,%s,,,%s,,,") a0 a1)
    /// The %s definitions in the signature and implementation are not compatible because the abstract member '%s' was required by the signature but was not specified by the implementation
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:148)
    static member DefinitionsInSigAndImplNotCompatibleAbstractMemberMissingInImpl(a0 : System.String, a1 : System.String) = (315, GetStringFunc("DefinitionsInSigAndImplNotCompatibleAbstractMemberMissingInImpl",",,,%s,,,%s,,,") a0 a1)
    /// The %s definitions in the signature and implementation are not compatible because the abstract member '%s' was present in the implementation but not in the signature
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:149)
    static member DefinitionsInSigAndImplNotCompatibleAbstractMemberMissingInSig(a0 : System.String, a1 : System.String) = (316, GetStringFunc("DefinitionsInSigAndImplNotCompatibleAbstractMemberMissingInSig",",,,%s,,,%s,,,") a0 a1)
    /// The %s definitions in the signature and implementation are not compatible because the signature declares a %s while the implementation declares a %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:150)
    static member DefinitionsInSigAndImplNotCompatibleSignatureDeclaresDiffer(a0 : System.String, a1 : System.String, a2 : System.String) = (317, GetStringFunc("DefinitionsInSigAndImplNotCompatibleSignatureDeclaresDiffer",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// The %s definitions in the signature and implementation are not compatible because the abbreviations differ: %s versus %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:151)
    static member DefinitionsInSigAndImplNotCompatibleAbbreviationsDiffer(a0 : System.String, a1 : System.String, a2 : System.String) = (318, GetStringFunc("DefinitionsInSigAndImplNotCompatibleAbbreviationsDiffer",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// The %s definitions in the signature and implementation are not compatible because an abbreviation is being hidden by a signature. The abbreviation must be visible to other CLI languages. Consider making the abbreviation visible in the signature.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:152)
    static member DefinitionsInSigAndImplNotCompatibleAbbreviationHiddenBySig(a0 : System.String) = (319, GetStringFunc("DefinitionsInSigAndImplNotCompatibleAbbreviationHiddenBySig",",,,%s,,,") a0)
    /// The %s definitions in the signature and implementation are not compatible because the signature has an abbreviation while the implementation does not
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:153)
    static member DefinitionsInSigAndImplNotCompatibleSigHasAbbreviation(a0 : System.String) = (320, GetStringFunc("DefinitionsInSigAndImplNotCompatibleSigHasAbbreviation",",,,%s,,,") a0)
    /// The module contains the constructor\n    %s    \nbut its signature specifies\n    %s    \nThe names differ
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:154)
    static member ModuleContainsConstructorButNamesDiffer(a0 : System.String, a1 : System.String) = (GetStringFunc("ModuleContainsConstructorButNamesDiffer",",,,%s,,,%s,,,") a0 a1)
    /// The module contains the constructor\n    %s    \nbut its signature specifies\n    %s    \nThe respective number of data fields differ
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:155)
    static member ModuleContainsConstructorButDataFieldsDiffer(a0 : System.String, a1 : System.String) = (GetStringFunc("ModuleContainsConstructorButDataFieldsDiffer",",,,%s,,,%s,,,") a0 a1)
    /// The module contains the constructor\n    %s    \nbut its signature specifies\n    %s    \nThe types of the fields differ
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:156)
    static member ModuleContainsConstructorButTypesOfFieldsDiffer(a0 : System.String, a1 : System.String) = (GetStringFunc("ModuleContainsConstructorButTypesOfFieldsDiffer",",,,%s,,,%s,,,") a0 a1)
    /// The module contains the constructor\n    %s    \nbut its signature specifies\n    %s    \nthe accessibility specified in the signature is more than that specified in the implementation
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:157)
    static member ModuleContainsConstructorButAccessibilityDiffers(a0 : System.String, a1 : System.String) = (GetStringFunc("ModuleContainsConstructorButAccessibilityDiffers",",,,%s,,,%s,,,") a0 a1)
    /// The module contains the field\n    %s    \nbut its signature specifies\n    %s    \nThe names differ
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:158)
    static member FieldNotContainedNamesDiffer(a0 : System.String, a1 : System.String) = (GetStringFunc("FieldNotContainedNamesDiffer",",,,%s,,,%s,,,") a0 a1)
    /// The module contains the field\n    %s    \nbut its signature specifies\n    %s    \nthe accessibility specified in the signature is more than that specified in the implementation
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:159)
    static member FieldNotContainedAccessibilitiesDiffer(a0 : System.String, a1 : System.String) = (GetStringFunc("FieldNotContainedAccessibilitiesDiffer",",,,%s,,,%s,,,") a0 a1)
    /// The module contains the field\n    %s    \nbut its signature specifies\n    %s    \nThe 'static' modifiers differ
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:160)
    static member FieldNotContainedStaticsDiffer(a0 : System.String, a1 : System.String) = (GetStringFunc("FieldNotContainedStaticsDiffer",",,,%s,,,%s,,,") a0 a1)
    /// The module contains the field\n    %s    \nbut its signature specifies\n    %s    \nThe 'mutable' modifiers differ
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:161)
    static member FieldNotContainedMutablesDiffer(a0 : System.String, a1 : System.String) = (GetStringFunc("FieldNotContainedMutablesDiffer",",,,%s,,,%s,,,") a0 a1)
    /// The module contains the field\n    %s    \nbut its signature specifies\n    %s    \nThe 'literal' modifiers differ
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:162)
    static member FieldNotContainedLiteralsDiffer(a0 : System.String, a1 : System.String) = (GetStringFunc("FieldNotContainedLiteralsDiffer",",,,%s,,,%s,,,") a0 a1)
    /// The module contains the field\n    %s    \nbut its signature specifies\n    %s    \nThe types differ
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:163)
    static member FieldNotContainedTypesDiffer(a0 : System.String, a1 : System.String) = (GetStringFunc("FieldNotContainedTypesDiffer",",,,%s,,,%s,,,") a0 a1)
    /// The implicit instantiation of a generic construct at or near this point could not be resolved because it could resolve to multiple unrelated types, e.g. '%s' and '%s'. Consider using type annotations to resolve the ambiguity
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:164)
    static member typrelCannotResolveImplicitGenericInstantiation(a0 : System.String, a1 : System.String) = (331, GetStringFunc("typrelCannotResolveImplicitGenericInstantiation",",,,%s,,,%s,,,") a0 a1)
    /// Could not resolve the ambiguity inherent in the use of the operator '%s' at or near this program point. Consider using type annotations to resolve the ambiguity.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:165)
    static member typrelCannotResolveAmbiguityInOverloadedOperator(a0 : System.String) = (332, GetStringFunc("typrelCannotResolveAmbiguityInOverloadedOperator",",,,%s,,,") a0)
    /// Could not resolve the ambiguity inherent in the use of a 'printf'-style format string
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:166)
    static member typrelCannotResolveAmbiguityInPrintf() = (333, GetStringFunc("typrelCannotResolveAmbiguityInPrintf",",,,") )
    /// Could not resolve the ambiguity in the use of a generic construct with an 'enum' constraint at or near this position
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:167)
    static member typrelCannotResolveAmbiguityInEnum() = (334, GetStringFunc("typrelCannotResolveAmbiguityInEnum",",,,") )
    /// Could not resolve the ambiguity in the use of a generic construct with a 'delegate' constraint at or near this position
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:168)
    static member typrelCannotResolveAmbiguityInDelegate() = (335, GetStringFunc("typrelCannotResolveAmbiguityInDelegate",",,,") )
    /// Invalid value
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:169)
    static member typrelInvalidValue() = (337, GetStringFunc("typrelInvalidValue",",,,") )
    /// The signature and implementation are not compatible because the respective type parameter counts differ
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:170)
    static member typrelSigImplNotCompatibleParamCountsDiffer() = (338, GetStringFunc("typrelSigImplNotCompatibleParamCountsDiffer",",,,") )
    /// The signature and implementation are not compatible because the type parameter in the class/signature has a different compile-time requirement to the one in the member/implementation
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:171)
    static member typrelSigImplNotCompatibleCompileTimeRequirementsDiffer() = (339, GetStringFunc("typrelSigImplNotCompatibleCompileTimeRequirementsDiffer",",,,") )
    /// The signature and implementation are not compatible because the declaration of the type parameter '%s' requires a constraint of the form %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:172)
    static member typrelSigImplNotCompatibleConstraintsDiffer(a0 : System.String, a1 : System.String) = (340, GetStringFunc("typrelSigImplNotCompatibleConstraintsDiffer",",,,%s,,,%s,,,") a0 a1)
    /// The signature and implementation are not compatible because the type parameter '%s' has a constraint of the form %s but the implementation does not. Either remove this constraint from the signature or add it to the implementation.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:173)
    static member typrelSigImplNotCompatibleConstraintsDifferRemove(a0 : System.String, a1 : System.String) = (341, GetStringFunc("typrelSigImplNotCompatibleConstraintsDifferRemove",",,,%s,,,%s,,,") a0 a1)
    /// The type '%s' implements 'System.IComparable'. Consider also adding an explicit override for 'Object.Equals'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:174)
    static member typrelTypeImplementsIComparableShouldOverrideObjectEquals(a0 : System.String) = (342, GetStringFunc("typrelTypeImplementsIComparableShouldOverrideObjectEquals",",,,%s,,,") a0)
    /// The type '%s' implements 'System.IComparable' explicitly but provides no corresponding override for 'Object.Equals'. An implementation of 'Object.Equals' has been automatically provided, implemented via 'System.IComparable'. Consider implementing the override 'Object.Equals' explicitly
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:175)
    static member typrelTypeImplementsIComparableDefaultObjectEqualsProvided(a0 : System.String) = (343, GetStringFunc("typrelTypeImplementsIComparableDefaultObjectEqualsProvided",",,,%s,,,") a0)
    /// The struct, record or union type '%s' has an explicit implementation of 'Object.GetHashCode' or 'Object.Equals'. You must apply the 'CustomEquality' attribute to the type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:176)
    static member typrelExplicitImplementationOfGetHashCodeOrEquals(a0 : System.String) = (344, GetStringFunc("typrelExplicitImplementationOfGetHashCodeOrEquals",",,,%s,,,") a0)
    /// The struct, record or union type '%s' has an explicit implementation of 'Object.GetHashCode'. Consider implementing a matching override for 'Object.Equals(obj)'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:177)
    static member typrelExplicitImplementationOfGetHashCode(a0 : System.String) = (345, GetStringFunc("typrelExplicitImplementationOfGetHashCode",",,,%s,,,") a0)
    /// The struct, record or union type '%s' has an explicit implementation of 'Object.Equals'. Consider implementing a matching override for 'Object.GetHashCode()'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:178)
    static member typrelExplicitImplementationOfEquals(a0 : System.String) = (346, GetStringFunc("typrelExplicitImplementationOfEquals",",,,%s,,,") a0)
    /// The exception definitions are not compatible because a CLI exception mapping is being hidden by a signature. The exception mapping must be visible to other modules. The module contains the exception definition\n    %s    \nbut its signature specifies\n\t%s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:179)
    static member ExceptionDefsNotCompatibleHiddenBySignature(a0 : System.String, a1 : System.String) = (GetStringFunc("ExceptionDefsNotCompatibleHiddenBySignature",",,,%s,,,%s,,,") a0 a1)
    /// The exception definitions are not compatible because the CLI representations differ. The module contains the exception definition\n    %s    \nbut its signature specifies\n\t%s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:180)
    static member ExceptionDefsNotCompatibleDotNetRepresentationsDiffer(a0 : System.String, a1 : System.String) = (GetStringFunc("ExceptionDefsNotCompatibleDotNetRepresentationsDiffer",",,,%s,,,%s,,,") a0 a1)
    /// The exception definitions are not compatible because the exception abbreviation is being hidden by the signature. The abbreviation must be visible to other CLI languages. Consider making the abbreviation visible in the signature. The module contains the exception definition\n    %s    \nbut its signature specifies\n\t%s.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:181)
    static member ExceptionDefsNotCompatibleAbbreviationHiddenBySignature(a0 : System.String, a1 : System.String) = (GetStringFunc("ExceptionDefsNotCompatibleAbbreviationHiddenBySignature",",,,%s,,,%s,,,") a0 a1)
    /// The exception definitions are not compatible because the exception abbreviations in the signature and implementation differ. The module contains the exception definition\n    %s    \nbut its signature specifies\n\t%s.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:182)
    static member ExceptionDefsNotCompatibleSignaturesDiffer(a0 : System.String, a1 : System.String) = (GetStringFunc("ExceptionDefsNotCompatibleSignaturesDiffer",",,,%s,,,%s,,,") a0 a1)
    /// The exception definitions are not compatible because the exception declarations differ. The module contains the exception definition\n    %s    \nbut its signature specifies\n\t%s.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:183)
    static member ExceptionDefsNotCompatibleExceptionDeclarationsDiffer(a0 : System.String, a1 : System.String) = (GetStringFunc("ExceptionDefsNotCompatibleExceptionDeclarationsDiffer",",,,%s,,,%s,,,") a0 a1)
    /// The exception definitions are not compatible because the field '%s' was required by the signature but was not specified by the implementation. The module contains the exception definition\n    %s    \nbut its signature specifies\n\t%s.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:184)
    static member ExceptionDefsNotCompatibleFieldInSigButNotImpl(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("ExceptionDefsNotCompatibleFieldInSigButNotImpl",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// The exception definitions are not compatible because the field '%s' was present in the implementation but not in the signature. The module contains the exception definition\n    %s    \nbut its signature specifies\n\t%s.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:185)
    static member ExceptionDefsNotCompatibleFieldInImplButNotSig(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("ExceptionDefsNotCompatibleFieldInImplButNotSig",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// The exception definitions are not compatible because the order of the fields is different in the signature and implementation. The module contains the exception definition\n    %s    \nbut its signature specifies\n\t%s.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:186)
    static member ExceptionDefsNotCompatibleFieldOrderDiffers(a0 : System.String, a1 : System.String) = (GetStringFunc("ExceptionDefsNotCompatibleFieldOrderDiffers",",,,%s,,,%s,,,") a0 a1)
    /// The namespace or module attributes differ between signature and implementation
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:187)
    static member typrelModuleNamespaceAttributesDifferInSigAndImpl() = (355, GetStringFunc("typrelModuleNamespaceAttributesDifferInSigAndImpl",",,,") )
    /// This method is over-constrained in its type parameters
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:188)
    static member typrelMethodIsOverconstrained() = (356, GetStringFunc("typrelMethodIsOverconstrained",",,,") )
    /// No implementations of '%s' had the correct number of arguments and type parameters. The required signature is '%s'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:189)
    static member typrelOverloadNotFound(a0 : System.String, a1 : System.String) = (357, GetStringFunc("typrelOverloadNotFound",",,,%s,,,%s,,,") a0 a1)
    /// The override for '%s' was ambiguous
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:190)
    static member typrelOverrideWasAmbiguous(a0 : System.String) = (358, GetStringFunc("typrelOverrideWasAmbiguous",",,,%s,,,") a0)
    /// More than one override implements '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:191)
    static member typrelMoreThenOneOverride(a0 : System.String) = (359, GetStringFunc("typrelMoreThenOneOverride",",,,%s,,,") a0)
    /// The method '%s' is sealed and cannot be overridden
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:192)
    static member typrelMethodIsSealed(a0 : System.String) = (360, GetStringFunc("typrelMethodIsSealed",",,,%s,,,") a0)
    /// The override '%s' implements more than one abstract slot, e.g. '%s' and '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:193)
    static member typrelOverrideImplementsMoreThenOneSlot(a0 : System.String, a1 : System.String, a2 : System.String) = (361, GetStringFunc("typrelOverrideImplementsMoreThenOneSlot",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// Duplicate or redundant interface
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:194)
    static member typrelDuplicateInterface() = (362, GetStringFunc("typrelDuplicateInterface",",,,") )
    /// The interface '%s' is included in multiple explicitly implemented interface types. Add an explicit implementation of this interface.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:195)
    static member typrelNeedExplicitImplementation(a0 : System.String) = (363, GetStringFunc("typrelNeedExplicitImplementation",",,,%s,,,") a0)
    /// A named argument has been assigned more than one value
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:196)
    static member typrelNamedArgumentHasBeenAssignedMoreThenOnce() = (364, GetStringFunc("typrelNamedArgumentHasBeenAssignedMoreThenOnce",",,,") )
    /// No implementation was given for '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:197)
    static member typrelNoImplementationGiven(a0 : System.String) = (365, GetStringFunc("typrelNoImplementationGiven",",,,%s,,,") a0)
    /// No implementation was given for '%s'. Note that all interface members must be implemented and listed under an appropriate 'interface' declaration, e.g. 'interface ... with member ...'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:198)
    static member typrelNoImplementationGivenWithSuggestion(a0 : System.String) = (366, GetStringFunc("typrelNoImplementationGivenWithSuggestion",",,,%s,,,") a0)
    /// The member '%s' does not have the correct number of arguments. The required signature is '%s'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:199)
    static member typrelMemberDoesNotHaveCorrectNumberOfArguments(a0 : System.String, a1 : System.String) = (367, GetStringFunc("typrelMemberDoesNotHaveCorrectNumberOfArguments",",,,%s,,,%s,,,") a0 a1)
    /// The member '%s' does not have the correct number of method type parameters. The required signature is '%s'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:200)
    static member typrelMemberDoesNotHaveCorrectNumberOfTypeParameters(a0 : System.String, a1 : System.String) = (368, GetStringFunc("typrelMemberDoesNotHaveCorrectNumberOfTypeParameters",",,,%s,,,%s,,,") a0 a1)
    /// The member '%s' does not have the correct kinds of generic parameters. The required signature is '%s'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:201)
    static member typrelMemberDoesNotHaveCorrectKindsOfGenericParameters(a0 : System.String, a1 : System.String) = (369, GetStringFunc("typrelMemberDoesNotHaveCorrectKindsOfGenericParameters",",,,%s,,,%s,,,") a0 a1)
    /// The member '%s' cannot be used to implement '%s'. The required signature is '%s'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:202)
    static member typrelMemberCannotImplement(a0 : System.String, a1 : System.String, a2 : System.String) = (370, GetStringFunc("typrelMemberCannotImplement",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// Error while parsing embedded IL
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:206)
    static member astParseEmbeddedILError() = (371, GetStringFunc("astParseEmbeddedILError",",,,") )
    /// Error while parsing embedded IL type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:207)
    static member astParseEmbeddedILTypeError() = (372, GetStringFunc("astParseEmbeddedILTypeError",",,,") )
    /// This indexer notation has been removed from the F# language
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:208)
    static member astDeprecatedIndexerNotation() = (GetStringFunc("astDeprecatedIndexerNotation",",,,") )
    /// Invalid expression on left of assignment
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:209)
    static member astInvalidExprLeftHandOfAssignment() = (374, GetStringFunc("astInvalidExprLeftHandOfAssignment",",,,") )
    /// The 'ReferenceEquality' attribute cannot be used on structs. Consider using the 'StructuralEquality' attribute instead, or implement an override for 'System.Object.Equals(obj)'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:213)
    static member augNoRefEqualsOnStruct() = (376, GetStringFunc("augNoRefEqualsOnStruct",",,,") )
    /// This type uses an invalid mix of the attributes 'NoEquality', 'ReferenceEquality', 'StructuralEquality', 'NoComparison' and 'StructuralComparison' attributes
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:214)
    static member augInvalidAttrs() = (377, GetStringFunc("augInvalidAttrs",",,,") )
    /// The 'NoEquality' attribute must be used in conjunction with the 'NoComparison' attribute
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:215)
    static member augNoEqualityNeedsNoComparison() = (378, GetStringFunc("augNoEqualityNeedsNoComparison",",,,") )
    /// The 'StructuralComparison' attribute must be used in conjunction with the 'StructuralEquality' attribute
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:216)
    static member augStructCompNeedsStructEquality() = (379, GetStringFunc("augStructCompNeedsStructEquality",",,,") )
    /// The 'StructuralEquality' attribute must be used in conjunction with the 'NoComparison' or 'StructuralComparison' attributes
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:217)
    static member augStructEqNeedsNoCompOrStructComp() = (380, GetStringFunc("augStructEqNeedsNoCompOrStructComp",",,,") )
    /// A type cannot have both the 'ReferenceEquality' and 'StructuralEquality' or 'StructuralComparison' attributes
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:218)
    static member augTypeCantHaveRefEqAndStructAttrs() = (381, GetStringFunc("augTypeCantHaveRefEqAndStructAttrs",",,,") )
    /// Only record, union, exception and struct types may be augmented with the 'ReferenceEquality', 'StructuralEquality' and 'StructuralComparison' attributes
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:219)
    static member augOnlyCertainTypesCanHaveAttrs() = (382, GetStringFunc("augOnlyCertainTypesCanHaveAttrs",",,,") )
    /// A type with attribute 'ReferenceEquality' cannot have an explicit implementation of 'Object.Equals(obj)', 'System.IEquatable<_>' or 'System.Collections.IStructuralEquatable'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:220)
    static member augRefEqCantHaveObjEquals() = (383, GetStringFunc("augRefEqCantHaveObjEquals",",,,") )
    /// A type with attribute 'CustomEquality' must have an explicit implementation of at least one of 'Object.Equals(obj)', 'System.IEquatable<_>' or 'System.Collections.IStructuralEquatable'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:221)
    static member augCustomEqNeedsObjEquals() = (384, GetStringFunc("augCustomEqNeedsObjEquals",",,,") )
    /// A type with attribute 'CustomComparison' must have an explicit implementation of at least one of 'System.IComparable' or 'System.Collections.IStructuralComparable'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:222)
    static member augCustomCompareNeedsIComp() = (385, GetStringFunc("augCustomCompareNeedsIComp",",,,") )
    /// A type with attribute 'NoEquality' should not usually have an explicit implementation of 'Object.Equals(obj)'. Disable this warning if this is intentional for interoperability purposes
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:223)
    static member augNoEqNeedsNoObjEquals() = (386, GetStringFunc("augNoEqNeedsNoObjEquals",",,,") )
    /// A type with attribute 'NoComparison' should not usually have an explicit implementation of 'System.IComparable', 'System.IComparable<_>' or 'System.Collections.IStructuralComparable'. Disable this warning if this is intentional for interoperability purposes
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:224)
    static member augNoCompCantImpIComp() = (386, GetStringFunc("augNoCompCantImpIComp",",,,") )
    /// The 'CustomEquality' attribute must be used in conjunction with the 'NoComparison' or 'CustomComparison' attributes
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:225)
    static member augCustomEqNeedsNoCompOrCustomComp() = (387, GetStringFunc("augCustomEqNeedsNoCompOrCustomComp",",,,") )
    /// Positional specifiers are not permitted in format strings
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:229)
    static member forPositionalSpecifiersNotPermitted() = (GetStringFunc("forPositionalSpecifiersNotPermitted",",,,") )
    /// Missing format specifier
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:230)
    static member forMissingFormatSpecifier() = (GetStringFunc("forMissingFormatSpecifier",",,,") )
    /// '%s' flag set twice
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:231)
    static member forFlagSetTwice(a0 : System.String) = (GetStringFunc("forFlagSetTwice",",,,%s,,,") a0)
    /// Prefix flag (' ' or '+') set twice
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:232)
    static member forPrefixFlagSpacePlusSetTwice() = (GetStringFunc("forPrefixFlagSpacePlusSetTwice",",,,") )
    /// The # formatting modifier is invalid in F#
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:233)
    static member forHashSpecifierIsInvalid() = (GetStringFunc("forHashSpecifierIsInvalid",",,,") )
    /// Bad precision in format specifier
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:234)
    static member forBadPrecision() = (GetStringFunc("forBadPrecision",",,,") )
    /// Bad width in format specifier
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:235)
    static member forBadWidth() = (GetStringFunc("forBadWidth",",,,") )
    /// '%s' format does not support '0' flag
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:236)
    static member forDoesNotSupportZeroFlag(a0 : System.String) = (GetStringFunc("forDoesNotSupportZeroFlag",",,,%s,,,") a0)
    /// Precision missing after the '.'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:237)
    static member forPrecisionMissingAfterDot() = (GetStringFunc("forPrecisionMissingAfterDot",",,,") )
    /// '%s' format does not support precision
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:238)
    static member forFormatDoesntSupportPrecision(a0 : System.String) = (GetStringFunc("forFormatDoesntSupportPrecision",",,,%s,,,") a0)
    /// Bad format specifier (after l or L): Expected ld,li,lo,lu,lx or lX. In F# code you can use %%d, %%x, %%o or %%u instead, which are overloaded to work with all basic integer types.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:239)
    static member forBadFormatSpecifier() = (GetStringFunc("forBadFormatSpecifier",",,,") )
    /// The 'l' or 'L' in this format specifier is unnecessary. In F# code you can use %%d, %%x, %%o or %%u instead, which are overloaded to work with all basic integer types.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:240)
    static member forLIsUnnecessary() = (GetStringFunc("forLIsUnnecessary",",,,") )
    /// The 'h' or 'H' in this format specifier is unnecessary. You can use %%d, %%x, %%o or %%u instead, which are overloaded to work with all basic integer types..
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:241)
    static member forHIsUnnecessary() = (GetStringFunc("forHIsUnnecessary",",,,") )
    /// '%s' does not support prefix '%s' flag
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:242)
    static member forDoesNotSupportPrefixFlag(a0 : System.String, a1 : System.String) = (GetStringFunc("forDoesNotSupportPrefixFlag",",,,%s,,,%s,,,") a0 a1)
    /// Bad format specifier: '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:243)
    static member forBadFormatSpecifierGeneral(a0 : System.String) = (GetStringFunc("forBadFormatSpecifierGeneral",",,,%s,,,") a0)
    /// System.Environment.Exit did not exit
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:247)
    static member elSysEnvExitDidntExit() = (GetStringFunc("elSysEnvExitDidntExit",",,,") )
    /// The treatment of this operator is now handled directly by the F# compiler and its meaning cannot be redefined
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:248)
    static member elDeprecatedOperator() = (GetStringFunc("elDeprecatedOperator",",,,") )
    /// A protected member is called or 'base' is being used. This is only allowed in the direct implementation of members since they could escape their object scope.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:252)
    static member chkProtectedOrBaseCalled() = (405, GetStringFunc("chkProtectedOrBaseCalled",",,,") )
    /// The byref-typed variable '%s' is used in an invalid way. Byrefs cannot be captured by closures or passed to inner functions.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:253)
    static member chkByrefUsedInInvalidWay(a0 : System.String) = (406, GetStringFunc("chkByrefUsedInInvalidWay",",,,%s,,,") a0)
    /// The mutable variable '%s' is used in an invalid way. Mutable variables cannot be captured by closures. Consider eliminating this use of mutation or using a heap-allocated mutable reference cell via 'ref' and '!'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:254)
    static member chkMutableUsedInInvalidWay(a0 : System.String) = (407, GetStringFunc("chkMutableUsedInInvalidWay",",,,%s,,,") a0)
    /// The 'base' keyword is used in an invalid way. Base calls cannot be used in closures. Consider using a private member to make base calls.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:255)
    static member chkBaseUsedInInvalidWay() = (408, GetStringFunc("chkBaseUsedInInvalidWay",",,,") )
    /// The variable '%s' is used in an invalid way
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:256)
    static member chkVariableUsedInInvalidWay(a0 : System.String) = (GetStringFunc("chkVariableUsedInInvalidWay",",,,%s,,,") a0)
    /// The type '%s' is less accessible than the value, member or type '%s' it is used in
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:257)
    static member chkTypeLessAccessibleThanType(a0 : System.String, a1 : System.String) = (410, GetStringFunc("chkTypeLessAccessibleThanType",",,,%s,,,%s,,,") a0 a1)
    /// 'System.Void' can only be used as 'typeof<System.Void>' in F#
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:258)
    static member chkSystemVoidOnlyInTypeof() = (411, GetStringFunc("chkSystemVoidOnlyInTypeof",",,,") )
    /// A type instantiation involves a byref type. This is not permitted by the rules of Common IL.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:259)
    static member chkErrorUseOfByref() = (412, GetStringFunc("chkErrorUseOfByref",",,,") )
    /// Calls to 'reraise' may only occur directly in a handler of a try-with
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:260)
    static member chkErrorContainsCallToRethrow() = (413, GetStringFunc("chkErrorContainsCallToRethrow",",,,") )
    /// Expression-splicing operators may only be used within quotations
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:261)
    static member chkSplicingOnlyInQuotations() = (414, GetStringFunc("chkSplicingOnlyInQuotations",",,,") )
    /// First-class uses of the expression-splicing operator are not permitted
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:262)
    static member chkNoFirstClassSplicing() = (415, GetStringFunc("chkNoFirstClassSplicing",",,,") )
    /// First-class uses of the address-of operators are not permitted
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:263)
    static member chkNoFirstClassAddressOf() = (416, GetStringFunc("chkNoFirstClassAddressOf",",,,") )
    /// First-class uses of the 'reraise' function is not permitted
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:264)
    static member chkNoFirstClassRethrow() = (417, GetStringFunc("chkNoFirstClassRethrow",",,,") )
    /// The byref typed value '%s' cannot be used at this point
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:265)
    static member chkNoByrefAtThisPoint(a0 : System.String) = (418, GetStringFunc("chkNoByrefAtThisPoint",",,,%s,,,") a0)
    /// 'base' values may only be used to make direct calls to the base implementations of overridden members
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:266)
    static member chkLimitationsOfBaseKeyword() = (419, GetStringFunc("chkLimitationsOfBaseKeyword",",,,") )
    /// Object constructors cannot directly use try/with and try/finally prior to the initialization of the object. This includes constructs such as 'for x in ...' that may elaborate to uses of these constructs. This is a limitation imposed by Common IL.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:267)
    static member chkObjCtorsCantUseExceptionHandling() = (420, GetStringFunc("chkObjCtorsCantUseExceptionHandling",",,,") )
    /// The address of the variable '%s' cannot be used at this point
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:268)
    static member chkNoAddressOfAtThisPoint(a0 : System.String) = (421, GetStringFunc("chkNoAddressOfAtThisPoint",",,,%s,,,") a0)
    /// The address of the static field '%s' cannot be used at this point
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:269)
    static member chkNoAddressStaticFieldAtThisPoint(a0 : System.String) = (422, GetStringFunc("chkNoAddressStaticFieldAtThisPoint",",,,%s,,,") a0)
    /// The address of the field '%s' cannot be used at this point
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:270)
    static member chkNoAddressFieldAtThisPoint(a0 : System.String) = (423, GetStringFunc("chkNoAddressFieldAtThisPoint",",,,%s,,,") a0)
    /// The address of an array element cannot be used at this point
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:271)
    static member chkNoAddressOfArrayElementAtThisPoint() = (424, GetStringFunc("chkNoAddressOfArrayElementAtThisPoint",",,,") )
    /// The type of a first-class function cannot contain byrefs
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:272)
    static member chkFirstClassFuncNoByref() = (425, GetStringFunc("chkFirstClassFuncNoByref",",,,") )
    /// A method return type would contain byrefs which is not permitted
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:273)
    static member chkReturnTypeNoByref() = (426, GetStringFunc("chkReturnTypeNoByref",",,,") )
    /// Invalid custom attribute value (not a constant or literal)
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:274)
    static member chkInvalidCustAttrVal() = (428, GetStringFunc("chkInvalidCustAttrVal",",,,") )
    /// The attribute type '%s' has 'AllowMultiple=false'. Multiple instances of this attribute cannot be attached to a single language element.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:275)
    static member chkAttrHasAllowMultiFalse(a0 : System.String) = (429, GetStringFunc("chkAttrHasAllowMultiFalse",",,,%s,,,") a0)
    /// The member '%s' is used in an invalid way. A use of '%s' has been inferred prior to its definition at or near '%s'. This is an invalid forward reference.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:276)
    static member chkMemberUsedInInvalidWay(a0 : System.String, a1 : System.String, a2 : System.String) = (430, GetStringFunc("chkMemberUsedInInvalidWay",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// A byref typed value would be stored here. Top-level let-bound byref values are not permitted.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:277)
    static member chkNoByrefAsTopValue() = (431, GetStringFunc("chkNoByrefAsTopValue",",,,") )
    /// [<ReflectedDefinition>] terms cannot contain uses of the prefix splice operator '%%'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:278)
    static member chkReflectedDefCantSplice() = (432, GetStringFunc("chkReflectedDefCantSplice",",,,") )
    /// A function labeled with the 'EntryPointAttribute' attribute must be the last declaration in the last file in the compilation sequence, and can only be used when compiling to a .exe
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:279)
    static member chkEntryPointUsage() = (433, GetStringFunc("chkEntryPointUsage",",,,") )
    /// compiled form of the union case
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:280)
    static member chkUnionCaseCompiledForm() = (GetStringFunc("chkUnionCaseCompiledForm",",,,") )
    /// default augmentation of the union case
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:281)
    static member chkUnionCaseDefaultAugmentation() = (GetStringFunc("chkUnionCaseDefaultAugmentation",",,,") )
    /// Name clash. The property '%s' has the same name as a method in this type.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:282)
    static member chkPropertySameNameMethod(a0 : System.String) = (434, GetStringFunc("chkPropertySameNameMethod",",,,%s,,,") a0)
    /// The property '%s' has a getter and a setter that do not match. If one is abstract then the other must be as well.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:283)
    static member chkGetterSetterDoNotMatchAbstract(a0 : System.String) = (435, GetStringFunc("chkGetterSetterDoNotMatchAbstract",",,,%s,,,") a0)
    /// The property '%s' has the same name as another property in this type, but one takes indexer arguments and the other does not. You may be missing an indexer argument to one of your properties.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:284)
    static member chkPropertySameNameIndexer(a0 : System.String) = (436, GetStringFunc("chkPropertySameNameIndexer",",,,%s,,,") a0)
    /// A type would store a byref typed value. This is not permitted by Common IL.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:285)
    static member chkCantStoreByrefValue() = (437, GetStringFunc("chkCantStoreByrefValue",",,,") )
    /// Duplicate method. The method '%s' has the same name and signature as another method in this type.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:287)
    static member chkDuplicateMethod(a0 : System.String) = (438, GetStringFunc("chkDuplicateMethod",",,,%s,,,") a0)
    /// Duplicate method. The method '%s' has the same name and signature as another method in this type once tuples, functions, units of measure and/or provided types are erased.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:288)
    static member chkDuplicateMethodWithSuffix(a0 : System.String) = (438, GetStringFunc("chkDuplicateMethodWithSuffix",",,,%s,,,") a0)
    /// The method '%s' has curried arguments but has the same name as another method in this type. Methods with curried arguments cannot be overloaded. Consider using a method taking tupled arguments.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:289)
    static member chkDuplicateMethodCurried(a0 : System.String) = (439, GetStringFunc("chkDuplicateMethodCurried",",,,%s,,,") a0)
    /// Methods with curried arguments cannot declare 'out', 'ParamArray', 'optional' or 'byref' arguments
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:290)
    static member chkCurriedMethodsCantHaveOutParams() = (440, GetStringFunc("chkCurriedMethodsCantHaveOutParams",",,,") )
    /// Duplicate property. The property '%s' has the same name and signature as another property in this type.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:291)
    static member chkDuplicateProperty(a0 : System.String) = (441, GetStringFunc("chkDuplicateProperty",",,,%s,,,") a0)
    /// Duplicate property. The property '%s' has the same name and signature as another property in this type once tuples, functions, units of measure and/or provided types are erased.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:292)
    static member chkDuplicatePropertyWithSuffix(a0 : System.String) = (441, GetStringFunc("chkDuplicatePropertyWithSuffix",",,,%s,,,") a0)
    /// Duplicate method. The abstract method '%s' has the same name and signature as an abstract method in an inherited type.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:293)
    static member chkDuplicateMethodInheritedType(a0 : System.String) = (442, GetStringFunc("chkDuplicateMethodInheritedType",",,,%s,,,") a0)
    /// Duplicate method. The abstract method '%s' has the same name and signature as an abstract method in an inherited type once tuples, functions, units of measure and/or provided types are erased.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:294)
    static member chkDuplicateMethodInheritedTypeWithSuffix(a0 : System.String) = (442, GetStringFunc("chkDuplicateMethodInheritedTypeWithSuffix",",,,%s,,,") a0)
    /// This type implements or inherits the same interface at different generic instantiations '%s' and '%s'. This is not permitted in this version of F#.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:295)
    static member chkMultipleGenericInterfaceInstantiations(a0 : System.String, a1 : System.String) = (443, GetStringFunc("chkMultipleGenericInterfaceInstantiations",",,,%s,,,%s,,,") a0 a1)
    /// The type of a field using the 'DefaultValue' attribute must admit default initialization, i.e. have 'null' as a proper value or be a struct type whose fields all admit default initialization. You can use 'DefaultValue(false)' to disable this check
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:296)
    static member chkValueWithDefaultValueMustHaveDefaultValue() = (444, GetStringFunc("chkValueWithDefaultValueMustHaveDefaultValue",",,,") )
    /// The type abbreviation contains byrefs. This is not permitted by F#.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:297)
    static member chkNoByrefInTypeAbbrev() = (445, GetStringFunc("chkNoByrefInTypeAbbrev",",,,") )
    /// The variable '%s' is bound in a quotation but is used as part of a spliced expression. This is not permitted since it may escape its scope.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:301)
    static member crefBoundVarUsedInSplice(a0 : System.String) = (446, GetStringFunc("crefBoundVarUsedInSplice",",,,%s,,,") a0)
    /// Quotations cannot contain uses of generic expressions
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:302)
    static member crefQuotationsCantContainGenericExprs() = (447, GetStringFunc("crefQuotationsCantContainGenericExprs",",,,") )
    /// Quotations cannot contain function definitions that are inferred or declared to be generic. Consider adding some type constraints to make this a valid quoted expression.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:303)
    static member crefQuotationsCantContainGenericFunctions() = (448, GetStringFunc("crefQuotationsCantContainGenericFunctions",",,,") )
    /// Quotations cannot contain object expressions
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:304)
    static member crefQuotationsCantContainObjExprs() = (449, GetStringFunc("crefQuotationsCantContainObjExprs",",,,") )
    /// Quotations cannot contain expressions that take the address of a field
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:305)
    static member crefQuotationsCantContainAddressOf() = (450, GetStringFunc("crefQuotationsCantContainAddressOf",",,,") )
    /// Quotations cannot contain expressions that fetch static fields
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:306)
    static member crefQuotationsCantContainStaticFieldRef() = (451, GetStringFunc("crefQuotationsCantContainStaticFieldRef",",,,") )
    /// Quotations cannot contain inline assembly code or pattern matching on arrays
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:307)
    static member crefQuotationsCantContainInlineIL() = (452, GetStringFunc("crefQuotationsCantContainInlineIL",",,,") )
    /// Quotations cannot contain descending for loops
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:308)
    static member crefQuotationsCantContainDescendingForLoops() = (453, GetStringFunc("crefQuotationsCantContainDescendingForLoops",",,,") )
    /// Quotations cannot contain expressions that fetch union case indexes
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:309)
    static member crefQuotationsCantFetchUnionIndexes() = (454, GetStringFunc("crefQuotationsCantFetchUnionIndexes",",,,") )
    /// Quotations cannot contain expressions that set union case fields
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:310)
    static member crefQuotationsCantSetUnionFields() = (455, GetStringFunc("crefQuotationsCantSetUnionFields",",,,") )
    /// Quotations cannot contain expressions that set fields in exception values
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:311)
    static member crefQuotationsCantSetExceptionFields() = (456, GetStringFunc("crefQuotationsCantSetExceptionFields",",,,") )
    /// Quotations cannot contain expressions that require byref pointers
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:312)
    static member crefQuotationsCantRequireByref() = (457, GetStringFunc("crefQuotationsCantRequireByref",",,,") )
    /// Quotations cannot contain expressions that make member constraint calls, or uses of operators that implicitly resolve to a member constraint call
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:313)
    static member crefQuotationsCantCallTraitMembers() = (458, GetStringFunc("crefQuotationsCantCallTraitMembers",",,,") )
    /// Quotations cannot contain this kind of constant
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:314)
    static member crefQuotationsCantContainThisConstant() = (459, GetStringFunc("crefQuotationsCantContainThisConstant",",,,") )
    /// Quotations cannot contain this kind of pattern match
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:315)
    static member crefQuotationsCantContainThisPatternMatch() = (460, GetStringFunc("crefQuotationsCantContainThisPatternMatch",",,,") )
    /// Quotations cannot contain array pattern matching
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:316)
    static member crefQuotationsCantContainArrayPatternMatching() = (461, GetStringFunc("crefQuotationsCantContainArrayPatternMatching",",,,") )
    /// Quotations cannot contain this kind of type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:317)
    static member crefQuotationsCantContainThisType() = (462, GetStringFunc("crefQuotationsCantContainThisType",",,,") )
    /// The declared type parameter '%s' cannot be used here since the type parameter cannot be resolved at compile time
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:321)
    static member csTypeCannotBeResolvedAtCompileTime(a0 : System.String) = (GetStringFunc("csTypeCannotBeResolvedAtCompileTime",",,,%s,,,") a0)
    /// This code is less generic than indicated by its annotations. A unit-of-measure specified using '_' has been determined to be '1', i.e. dimensionless. Consider making the code generic, or removing the use of '_'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:322)
    static member csCodeLessGeneric() = (464, GetStringFunc("csCodeLessGeneric",",,,") )
    /// Type inference problem too complicated (maximum iteration depth reached). Consider adding further type annotations.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:323)
    static member csTypeInferenceMaxDepth() = (465, GetStringFunc("csTypeInferenceMaxDepth",",,,") )
    /// Expected arguments to an instance member
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:324)
    static member csExpectedArguments() = (GetStringFunc("csExpectedArguments",",,,") )
    /// This indexer expects %d arguments but is here given %d
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:325)
    static member csIndexArgumentMismatch(a0 : System.Int32, a1 : System.Int32) = (GetStringFunc("csIndexArgumentMismatch",",,,%d,,,%d,,,") a0 a1)
    /// Expecting a type supporting the operator '%s' but given a function type. You may be missing an argument to a function.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:326)
    static member csExpectTypeWithOperatorButGivenFunction(a0 : System.String) = (GetStringFunc("csExpectTypeWithOperatorButGivenFunction",",,,%s,,,") a0)
    /// Expecting a type supporting the operator '%s' but given a tuple type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:327)
    static member csExpectTypeWithOperatorButGivenTuple(a0 : System.String) = (GetStringFunc("csExpectTypeWithOperatorButGivenTuple",",,,%s,,,") a0)
    /// None of the types '%s' support the operator '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:328)
    static member csTypesDoNotSupportOperator(a0 : System.String, a1 : System.String) = (GetStringFunc("csTypesDoNotSupportOperator",",,,%s,,,%s,,,") a0 a1)
    /// The type '%s' does not support the operator '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:329)
    static member csTypeDoesNotSupportOperator(a0 : System.String, a1 : System.String) = (GetStringFunc("csTypeDoesNotSupportOperator",",,,%s,,,%s,,,") a0 a1)
    /// None of the types '%s' support the operator '%s'. Consider opening the module 'Microsoft.FSharp.Linq.NullableOperators'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:330)
    static member csTypesDoNotSupportOperatorNullable(a0 : System.String, a1 : System.String) = (GetStringFunc("csTypesDoNotSupportOperatorNullable",",,,%s,,,%s,,,") a0 a1)
    /// The type '%s' does not support the operator '%s'. Consider opening the module 'Microsoft.FSharp.Linq.NullableOperators'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:331)
    static member csTypeDoesNotSupportOperatorNullable(a0 : System.String, a1 : System.String) = (GetStringFunc("csTypeDoesNotSupportOperatorNullable",",,,%s,,,%s,,,") a0 a1)
    /// The type '%s' does not support a conversion to the type '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:332)
    static member csTypeDoesNotSupportConversion(a0 : System.String, a1 : System.String) = (GetStringFunc("csTypeDoesNotSupportConversion",",,,%s,,,%s,,,") a0 a1)
    /// The type '%s' has a method '%s' (full name '%s'), but the method is static
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:333)
    static member csMethodFoundButIsStatic(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("csMethodFoundButIsStatic",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// The type '%s' has a method '%s' (full name '%s'), but the method is not static
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:334)
    static member csMethodFoundButIsNotStatic(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("csMethodFoundButIsNotStatic",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// The constraints 'struct' and 'not struct' are inconsistent
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:335)
    static member csStructConstraintInconsistent() = (472, GetStringFunc("csStructConstraintInconsistent",",,,") )
    /// The type '%s' does not have 'null' as a proper value
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:336)
    static member csTypeDoesNotHaveNull(a0 : System.String) = (GetStringFunc("csTypeDoesNotHaveNull",",,,%s,,,") a0)
    /// The type '%s' does not have 'null' as a proper value. To create a null value for a Nullable type use 'System.Nullable()'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:337)
    static member csNullableTypeDoesNotHaveNull(a0 : System.String) = (GetStringFunc("csNullableTypeDoesNotHaveNull",",,,%s,,,") a0)
    /// The type '%s' does not support the 'comparison' constraint because it has the 'NoComparison' attribute
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:338)
    static member csTypeDoesNotSupportComparison1(a0 : System.String) = (GetStringFunc("csTypeDoesNotSupportComparison1",",,,%s,,,") a0)
    /// The type '%s' does not support the 'comparison' constraint. For example, it does not support the 'System.IComparable' interface
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:339)
    static member csTypeDoesNotSupportComparison2(a0 : System.String) = (GetStringFunc("csTypeDoesNotSupportComparison2",",,,%s,,,") a0)
    /// The type '%s' does not support the 'comparison' constraint because it is a record, union or struct with one or more structural element types which do not support the 'comparison' constraint. Either avoid the use of comparison with this type, or add the 'StructuralComparison' attribute to the type to determine which field type does not support comparison
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:340)
    static member csTypeDoesNotSupportComparison3(a0 : System.String) = (GetStringFunc("csTypeDoesNotSupportComparison3",",,,%s,,,") a0)
    /// The type '%s' does not support the 'equality' constraint because it has the 'NoEquality' attribute
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:341)
    static member csTypeDoesNotSupportEquality1(a0 : System.String) = (GetStringFunc("csTypeDoesNotSupportEquality1",",,,%s,,,") a0)
    /// The type '%s' does not support the 'equality' constraint because it is a function type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:342)
    static member csTypeDoesNotSupportEquality2(a0 : System.String) = (GetStringFunc("csTypeDoesNotSupportEquality2",",,,%s,,,") a0)
    /// The type '%s' does not support the 'equality' constraint because it is a record, union or struct with one or more structural element types which do not support the 'equality' constraint. Either avoid the use of equality with this type, or add the 'StructuralEquality' attribute to the type to determine which field type does not support equality
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:343)
    static member csTypeDoesNotSupportEquality3(a0 : System.String) = (GetStringFunc("csTypeDoesNotSupportEquality3",",,,%s,,,") a0)
    /// The type '%s' is not a CLI enum type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:344)
    static member csTypeIsNotEnumType(a0 : System.String) = (GetStringFunc("csTypeIsNotEnumType",",,,%s,,,") a0)
    /// The type '%s' has a non-standard delegate type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:345)
    static member csTypeHasNonStandardDelegateType(a0 : System.String) = (GetStringFunc("csTypeHasNonStandardDelegateType",",,,%s,,,") a0)
    /// The type '%s' is not a CLI delegate type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:346)
    static member csTypeIsNotDelegateType(a0 : System.String) = (GetStringFunc("csTypeIsNotDelegateType",",,,%s,,,") a0)
    /// This type parameter cannot be instantiated to 'Nullable'. This is a restriction imposed in order to ensure the meaning of 'null' in some CLI languages is not confusing when used in conjunction with 'Nullable' values.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:347)
    static member csTypeParameterCannotBeNullable() = (GetStringFunc("csTypeParameterCannotBeNullable",",,,") )
    /// A generic construct requires that the type '%s' is a CLI or F# struct type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:348)
    static member csGenericConstructRequiresStructType(a0 : System.String) = (GetStringFunc("csGenericConstructRequiresStructType",",,,%s,,,") a0)
    /// A generic construct requires that the type '%s' is an unmanaged type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:349)
    static member csGenericConstructRequiresUnmanagedType(a0 : System.String) = (GetStringFunc("csGenericConstructRequiresUnmanagedType",",,,%s,,,") a0)
    /// The type '%s' is not compatible with any of the types %s, arising from the use of a printf-style format string
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:350)
    static member csTypeNotCompatibleBecauseOfPrintf(a0 : System.String, a1 : System.String) = (GetStringFunc("csTypeNotCompatibleBecauseOfPrintf",",,,%s,,,%s,,,") a0 a1)
    /// A generic construct requires that the type '%s' have reference semantics, but it does not, i.e. it is a struct
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:351)
    static member csGenericConstructRequiresReferenceSemantics(a0 : System.String) = (GetStringFunc("csGenericConstructRequiresReferenceSemantics",",,,%s,,,") a0)
    /// A generic construct requires that the type '%s' be non-abstract
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:352)
    static member csGenericConstructRequiresNonAbstract(a0 : System.String) = (GetStringFunc("csGenericConstructRequiresNonAbstract",",,,%s,,,") a0)
    /// A generic construct requires that the type '%s' have a public default constructor
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:353)
    static member csGenericConstructRequiresPublicDefaultConstructor(a0 : System.String) = (GetStringFunc("csGenericConstructRequiresPublicDefaultConstructor",",,,%s,,,") a0)
    /// Type instantiation length mismatch
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:354)
    static member csTypeInstantiationLengthMismatch() = (483, GetStringFunc("csTypeInstantiationLengthMismatch",",,,") )
    /// Optional arguments not permitted here
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:355)
    static member csOptionalArgumentNotPermittedHere() = (484, GetStringFunc("csOptionalArgumentNotPermittedHere",",,,") )
    /// %s is not a static member
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:356)
    static member csMemberIsNotStatic(a0 : System.String) = (485, GetStringFunc("csMemberIsNotStatic",",,,%s,,,") a0)
    /// %s is not an instance member
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:357)
    static member csMemberIsNotInstance(a0 : System.String) = (486, GetStringFunc("csMemberIsNotInstance",",,,%s,,,") a0)
    /// Argument length mismatch
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:358)
    static member csArgumentLengthMismatch() = (487, GetStringFunc("csArgumentLengthMismatch",",,,") )
    /// The argument types don't match
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:359)
    static member csArgumentTypesDoNotMatch() = (488, GetStringFunc("csArgumentTypesDoNotMatch",",,,") )
    /// This method expects a CLI 'params' parameter in this position. 'params' is a way of passing a variable number of arguments to a method in languages such as C#. Consider passing an array for this argument
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:360)
    static member csMethodExpectsParams() = (489, GetStringFunc("csMethodExpectsParams",",,,") )
    /// The member or object constructor '%s' is not %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:361)
    static member csMemberIsNotAccessible(a0 : System.String, a1 : System.String) = (490, GetStringFunc("csMemberIsNotAccessible",",,,%s,,,%s,,,") a0 a1)
    /// The member or object constructor '%s' is not %s. Private members may only be accessed from within the declaring type. Protected members may only be accessed from an extending type and cannot be accessed from inner lambda expressions.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:362)
    static member csMemberIsNotAccessible2(a0 : System.String, a1 : System.String) = (491, GetStringFunc("csMemberIsNotAccessible2",",,,%s,,,%s,,,") a0 a1)
    /// %s is not a static method
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:363)
    static member csMethodIsNotAStaticMethod(a0 : System.String) = (492, GetStringFunc("csMethodIsNotAStaticMethod",",,,%s,,,") a0)
    /// %s is not an instance method
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:364)
    static member csMethodIsNotAnInstanceMethod(a0 : System.String) = (493, GetStringFunc("csMethodIsNotAnInstanceMethod",",,,%s,,,") a0)
    /// The member or object constructor '%s' has no argument or settable return property '%s'. %s.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:365)
    static member csMemberHasNoArgumentOrReturnProperty(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("csMemberHasNoArgumentOrReturnProperty",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// The required signature is %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:366)
    static member csRequiredSignatureIs(a0 : System.String) = (495, GetStringFunc("csRequiredSignatureIs",",,,%s,,,") a0)
    /// The member or object constructor '%s' requires %d argument(s). The required signature is '%s'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:367)
    static member csMemberSignatureMismatch(a0 : System.String, a1 : System.Int32, a2 : System.String) = (496, GetStringFunc("csMemberSignatureMismatch",",,,%s,,,%d,,,%s,,,") a0 a1 a2)
    /// The member or object constructor '%s' requires %d additional argument(s). The required signature is '%s'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:368)
    static member csMemberSignatureMismatch2(a0 : System.String, a1 : System.Int32, a2 : System.String) = (497, GetStringFunc("csMemberSignatureMismatch2",",,,%s,,,%d,,,%s,,,") a0 a1 a2)
    /// The member or object constructor '%s' requires %d argument(s). The required signature is '%s'. Some names for missing arguments are %s.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:369)
    static member csMemberSignatureMismatch3(a0 : System.String, a1 : System.Int32, a2 : System.String, a3 : System.String) = (498, GetStringFunc("csMemberSignatureMismatch3",",,,%s,,,%d,,,%s,,,%s,,,") a0 a1 a2 a3)
    /// The member or object constructor '%s' requires %d additional argument(s). The required signature is '%s'. Some names for missing arguments are %s.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:370)
    static member csMemberSignatureMismatch4(a0 : System.String, a1 : System.Int32, a2 : System.String, a3 : System.String) = (499, GetStringFunc("csMemberSignatureMismatch4",",,,%s,,,%d,,,%s,,,%s,,,") a0 a1 a2 a3)
    /// The member or object constructor '%s' requires %d argument(s) but is here given %d unnamed and %d named argument(s). The required signature is '%s'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:371)
    static member csMemberSignatureMismatchArityNamed(a0 : System.String, a1 : System.Int32, a2 : System.Int32, a3 : System.Int32, a4 : System.String) = (500, GetStringFunc("csMemberSignatureMismatchArityNamed",",,,%s,,,%d,,,%d,,,%d,,,%s,,,") a0 a1 a2 a3 a4)
    /// The member or object constructor '%s' takes %d argument(s) but is here given %d. The required signature is '%s'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:372)
    static member csMemberSignatureMismatchArity(a0 : System.String, a1 : System.Int32, a2 : System.Int32, a3 : System.String) = (501, GetStringFunc("csMemberSignatureMismatchArity",",,,%s,,,%d,,,%d,,,%s,,,") a0 a1 a2 a3)
    /// The member or object constructor '%s' takes %d type argument(s) but is here given %d. The required signature is '%s'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:373)
    static member csMemberSignatureMismatchArityType(a0 : System.String, a1 : System.Int32, a2 : System.Int32, a3 : System.String) = (502, GetStringFunc("csMemberSignatureMismatchArityType",",,,%s,,,%d,,,%d,,,%s,,,") a0 a1 a2 a3)
    /// The member or object constructor '%s' taking %d arguments are not accessible from this code location. All accessible versions of method '%s' take %d arguments.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:374)
    static member csMemberNotAccessible(a0 : System.String, a1 : System.Int32, a2 : System.String, a3 : System.Int32) = (503, GetStringFunc("csMemberNotAccessible",",,,%s,,,%d,,,%s,,,%d,,,") a0 a1 a2 a3)
    /// Incorrect generic instantiation. No %s member named '%s' takes %d generic arguments.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:375)
    static member csIncorrectGenericInstantiation(a0 : System.String, a1 : System.String, a2 : System.Int32) = (504, GetStringFunc("csIncorrectGenericInstantiation",",,,%s,,,%s,,,%d,,,") a0 a1 a2)
    /// The member or object constructor '%s' does not take %d argument(s). An overload was found taking %d arguments.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:376)
    static member csMemberOverloadArityMismatch(a0 : System.String, a1 : System.Int32, a2 : System.Int32) = (505, GetStringFunc("csMemberOverloadArityMismatch",",,,%s,,,%d,,,%d,,,") a0 a1 a2)
    /// No %s member or object constructor named '%s' takes %d arguments
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:377)
    static member csNoMemberTakesTheseArguments(a0 : System.String, a1 : System.String, a2 : System.Int32) = (506, GetStringFunc("csNoMemberTakesTheseArguments",",,,%s,,,%s,,,%d,,,") a0 a1 a2)
    /// No %s member or object constructor named '%s' takes %d arguments. Note the call to this member also provides %d named arguments.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:378)
    static member csNoMemberTakesTheseArguments2(a0 : System.String, a1 : System.String, a2 : System.Int32, a3 : System.Int32) = (507, GetStringFunc("csNoMemberTakesTheseArguments2",",,,%s,,,%s,,,%d,,,%d,,,") a0 a1 a2 a3)
    /// No %s member or object constructor named '%s' takes %d arguments. The named argument '%s' doesn't correspond to any argument or settable return property for any overload.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:379)
    static member csNoMemberTakesTheseArguments3(a0 : System.String, a1 : System.String, a2 : System.Int32, a3 : System.String) = (508, GetStringFunc("csNoMemberTakesTheseArguments3",",,,%s,,,%s,,,%d,,,%s,,,") a0 a1 a2 a3)
    /// Method or object constructor '%s' not found
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:380)
    static member csMethodNotFound(a0 : System.String) = (509, GetStringFunc("csMethodNotFound",",,,%s,,,") a0)
    /// No overloads match for method '%s'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:381)
    static member csNoOverloadsFound(a0 : System.String) = (GetStringFunc("csNoOverloadsFound",",,,%s,,,") a0)
    /// A unique overload for method '%s' could not be determined based on type information prior to this program point. A type annotation may be needed.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:382)
    static member csMethodIsOverloaded(a0 : System.String) = (GetStringFunc("csMethodIsOverloaded",",,,%s,,,") a0)
    /// Candidates: %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:383)
    static member csCandidates(a0 : System.String) = (GetStringFunc("csCandidates",",,,%s,,,") a0)
    /// The available overloads are shown below (or in the Error List window).
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:384)
    static member csSeeAvailableOverloads() = (GetStringFunc("csSeeAvailableOverloads",",,,") )
    /// Accessibility modifiers are not permitted on 'do' bindings
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:388)
    static member parsDoCannotHaveVisibilityDeclarations() = (512, GetStringFunc("parsDoCannotHaveVisibilityDeclarations",",,,") )
    /// End of file in #if section begun at or after here
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:389)
    static member parsEofInHashIf() = (513, GetStringFunc("parsEofInHashIf",",,,") )
    /// End of file in string begun at or before here
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:390)
    static member parsEofInString() = (514, GetStringFunc("parsEofInString",",,,") )
    /// End of file in verbatim string begun at or before here
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:391)
    static member parsEofInVerbatimString() = (515, GetStringFunc("parsEofInVerbatimString",",,,") )
    /// End of file in comment begun at or before here
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:392)
    static member parsEofInComment() = (516, GetStringFunc("parsEofInComment",",,,") )
    /// End of file in string embedded in comment begun at or before here
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:393)
    static member parsEofInStringInComment() = (517, GetStringFunc("parsEofInStringInComment",",,,") )
    /// End of file in verbatim string embedded in comment begun at or before here
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:394)
    static member parsEofInVerbatimStringInComment() = (518, GetStringFunc("parsEofInVerbatimStringInComment",",,,") )
    /// End of file in IF-OCAML section begun at or before here
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:395)
    static member parsEofInIfOcaml() = (519, GetStringFunc("parsEofInIfOcaml",",,,") )
    /// End of file in directive begun at or before here
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:396)
    static member parsEofInDirective() = (520, GetStringFunc("parsEofInDirective",",,,") )
    /// No #endif found for #if or #else
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:397)
    static member parsNoHashEndIfFound() = (521, GetStringFunc("parsNoHashEndIfFound",",,,") )
    /// Attributes have been ignored in this construct
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:398)
    static member parsAttributesIgnored() = (522, GetStringFunc("parsAttributesIgnored",",,,") )
    /// 'use' bindings are not permitted in primary constructors
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:399)
    static member parsUseBindingsIllegalInImplicitClassConstructors() = (523, GetStringFunc("parsUseBindingsIllegalInImplicitClassConstructors",",,,") )
    /// 'use' bindings are not permitted in modules and are treated as 'let' bindings
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:400)
    static member parsUseBindingsIllegalInModules() = (524, GetStringFunc("parsUseBindingsIllegalInModules",",,,") )
    /// An integer for loop must use a simple identifier
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:401)
    static member parsIntegerForLoopRequiresSimpleIdentifier() = (525, GetStringFunc("parsIntegerForLoopRequiresSimpleIdentifier",",,,") )
    /// At most one 'with' augmentation is permitted
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:402)
    static member parsOnlyOneWithAugmentationAllowed() = (526, GetStringFunc("parsOnlyOneWithAugmentationAllowed",",,,") )
    /// A semicolon is not expected at this point
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:403)
    static member parsUnexpectedSemicolon() = (527, GetStringFunc("parsUnexpectedSemicolon",",,,") )
    /// Unexpected end of input
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:404)
    static member parsUnexpectedEndOfFile() = (528, GetStringFunc("parsUnexpectedEndOfFile",",,,") )
    /// Accessibility modifiers are not permitted here
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:405)
    static member parsUnexpectedVisibilityDeclaration() = (529, GetStringFunc("parsUnexpectedVisibilityDeclaration",",,,") )
    /// Only '#' compiler directives may occur prior to the first 'namespace' declaration
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:406)
    static member parsOnlyHashDirectivesAllowed() = (530, GetStringFunc("parsOnlyHashDirectivesAllowed",",,,") )
    /// Accessibility modifiers should come immediately prior to the identifier naming a construct
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:407)
    static member parsVisibilityDeclarationsShouldComePriorToIdentifier() = (531, GetStringFunc("parsVisibilityDeclarationsShouldComePriorToIdentifier",",,,") )
    /// Files should begin with either a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule', but not both. To define a module within a namespace use 'module SomeModule = ...'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:408)
    static member parsNamespaceOrModuleNotBoth() = (532, GetStringFunc("parsNamespaceOrModuleNotBoth",",,,") )
    /// A module abbreviation must be a simple name, not a path
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:409)
    static member parsModuleAbbreviationMustBeSimpleName() = (534, GetStringFunc("parsModuleAbbreviationMustBeSimpleName",",,,") )
    /// Ignoring attributes on module abbreviation
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:410)
    static member parsIgnoreAttributesOnModuleAbbreviation() = (535, GetStringFunc("parsIgnoreAttributesOnModuleAbbreviation",",,,") )
    /// Ignoring accessibility attribute on module abbreviation. Module abbreviations are always private.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:411)
    static member parsIgnoreAttributesOnModuleAbbreviationAlwaysPrivate() = (536, GetStringFunc("parsIgnoreAttributesOnModuleAbbreviationAlwaysPrivate",",,,") )
    /// Ignoring visibility attribute on module abbreviation. Module abbreviations are always private.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:412)
    static member parsIgnoreVisibilityOnModuleAbbreviationAlwaysPrivate() = (537, GetStringFunc("parsIgnoreVisibilityOnModuleAbbreviationAlwaysPrivate",",,,") )
    /// Unclosed block
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:413)
    static member parsUnClosedBlockInHashLight() = (538, GetStringFunc("parsUnClosedBlockInHashLight",",,,") )
    /// Unmatched 'begin' or 'struct'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:414)
    static member parsUnmatchedBeginOrStruct() = (539, GetStringFunc("parsUnmatchedBeginOrStruct",",,,") )
    /// A module name must be a simple name, not a path
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:415)
    static member parsModuleDefnMustBeSimpleName() = (541, GetStringFunc("parsModuleDefnMustBeSimpleName",",,,") )
    /// Unexpected empty type moduleDefn list
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:416)
    static member parsUnexpectedEmptyModuleDefn() = (542, GetStringFunc("parsUnexpectedEmptyModuleDefn",",,,") )
    /// Attributes should be placed before 'val'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:417)
    static member parsAttributesMustComeBeforeVal() = (GetStringFunc("parsAttributesMustComeBeforeVal",",,,") )
    /// Attributes are not permitted on interface implementations
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:418)
    static member parsAttributesAreNotPermittedOnInterfaceImplementations() = (543, GetStringFunc("parsAttributesAreNotPermittedOnInterfaceImplementations",",,,") )
    /// Syntax error
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:419)
    static member parsSyntaxError() = (544, GetStringFunc("parsSyntaxError",",,,") )
    /// Augmentations are not permitted on delegate type moduleDefns
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:420)
    static member parsAugmentationsIllegalOnDelegateType() = (545, GetStringFunc("parsAugmentationsIllegalOnDelegateType",",,,") )
    /// Unmatched 'class', 'interface' or 'struct'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:421)
    static member parsUnmatchedClassInterfaceOrStruct() = (546, GetStringFunc("parsUnmatchedClassInterfaceOrStruct",",,,") )
    /// A type definition requires one or more members or other declarations. If you intend to define an empty class, struct or interface, then use 'type ... = class end', 'interface end' or 'struct end'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:422)
    static member parsEmptyTypeDefinition() = (547, GetStringFunc("parsEmptyTypeDefinition",",,,") )
    /// Unmatched 'with' or badly formatted 'with' block
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:423)
    static member parsUnmatchedWith() = (550, GetStringFunc("parsUnmatchedWith",",,,") )
    /// 'get', 'set' or 'get,set' required
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:424)
    static member parsGetOrSetRequired() = (551, GetStringFunc("parsGetOrSetRequired",",,,") )
    /// Only class types may take value arguments
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:425)
    static member parsOnlyClassCanTakeValueArguments() = (552, GetStringFunc("parsOnlyClassCanTakeValueArguments",",,,") )
    /// Unmatched 'begin'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:426)
    static member parsUnmatchedBegin() = (553, GetStringFunc("parsUnmatchedBegin",",,,") )
    /// Invalid declaration syntax
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:427)
    static member parsInvalidDeclarationSyntax() = (554, GetStringFunc("parsInvalidDeclarationSyntax",",,,") )
    /// 'get' and/or 'set' required
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:428)
    static member parsGetAndOrSetRequired() = (555, GetStringFunc("parsGetAndOrSetRequired",",,,") )
    /// Type annotations on property getters and setters must be given after the 'get()' or 'set(v)', e.g. 'with get() : string = ...'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:429)
    static member parsTypeAnnotationsOnGetSet() = (556, GetStringFunc("parsTypeAnnotationsOnGetSet",",,,") )
    /// A getter property is expected to be a function, e.g. 'get() = ...' or 'get(index) = ...'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:430)
    static member parsGetterMustHaveAtLeastOneArgument() = (557, GetStringFunc("parsGetterMustHaveAtLeastOneArgument",",,,") )
    /// Multiple accessibilities given for property getter or setter
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:431)
    static member parsMultipleAccessibilitiesForGetSet() = (558, GetStringFunc("parsMultipleAccessibilitiesForGetSet",",,,") )
    /// Property setters must be defined using 'set value = ', 'set idx value = ' or 'set (idx1,...,idxN) value = ... '
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:432)
    static member parsSetSyntax() = (559, GetStringFunc("parsSetSyntax",",,,") )
    /// Interfaces always have the same visibility as the enclosing type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:433)
    static member parsInterfacesHaveSameVisibilityAsEnclosingType() = (560, GetStringFunc("parsInterfacesHaveSameVisibilityAsEnclosingType",",,,") )
    /// Accessibility modifiers are not allowed on this member. Abstract slots always have the same visibility as the enclosing type.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:434)
    static member parsAccessibilityModsIllegalForAbstract() = (561, GetStringFunc("parsAccessibilityModsIllegalForAbstract",",,,") )
    /// Attributes are not permitted on 'inherit' declarations
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:435)
    static member parsAttributesIllegalOnInherit() = (562, GetStringFunc("parsAttributesIllegalOnInherit",",,,") )
    /// Accessibility modifiers are not permitted on an 'inherits' declaration
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:436)
    static member parsVisibilityIllegalOnInherit() = (563, GetStringFunc("parsVisibilityIllegalOnInherit",",,,") )
    /// 'inherit' declarations cannot have 'as' bindings. To access members of the base class when overriding a method, the syntax 'base.SomeMember' may be used; 'base' is a keyword. Remove this 'as' binding.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:437)
    static member parsInheritDeclarationsCannotHaveAsBindings() = (564, GetStringFunc("parsInheritDeclarationsCannotHaveAsBindings",",,,") )
    /// Attributes are not allowed here
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:438)
    static member parsAttributesIllegalHere() = (565, GetStringFunc("parsAttributesIllegalHere",",,,") )
    /// Accessibility modifiers are not permitted in this position for type abbreviations
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:439)
    static member parsTypeAbbreviationsCannotHaveVisibilityDeclarations() = (566, GetStringFunc("parsTypeAbbreviationsCannotHaveVisibilityDeclarations",",,,") )
    /// Accessibility modifiers are not permitted in this position for enum types
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:440)
    static member parsEnumTypesCannotHaveVisibilityDeclarations() = (567, GetStringFunc("parsEnumTypesCannotHaveVisibilityDeclarations",",,,") )
    /// All enum fields must be given values
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:441)
    static member parsAllEnumFieldsRequireValues() = (568, GetStringFunc("parsAllEnumFieldsRequireValues",",,,") )
    /// Accessibility modifiers are not permitted on inline assembly code types
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:442)
    static member parsInlineAssemblyCannotHaveVisibilityDeclarations() = (569, GetStringFunc("parsInlineAssemblyCannotHaveVisibilityDeclarations",",,,") )
    /// Unexpected identifier: '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:443)
    static member parsUnexpectedIdentifier(a0 : System.String) = (571, GetStringFunc("parsUnexpectedIdentifier",",,,%s,,,") a0)
    /// Accessibility modifiers are not permitted on union cases. Use 'type U = internal ...' or 'type U = private ...' to give an accessibility to the whole representation.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:444)
    static member parsUnionCasesCannotHaveVisibilityDeclarations() = (572, GetStringFunc("parsUnionCasesCannotHaveVisibilityDeclarations",",,,") )
    /// Accessibility modifiers are not permitted on enumeration fields
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:445)
    static member parsEnumFieldsCannotHaveVisibilityDeclarations() = (573, GetStringFunc("parsEnumFieldsCannotHaveVisibilityDeclarations",",,,") )
    /// Consider using a separate record type instead
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:446)
    static member parsConsiderUsingSeparateRecordType() = (GetStringFunc("parsConsiderUsingSeparateRecordType",",,,") )
    /// Accessibility modifiers are not permitted on record fields. Use 'type R = internal ...' or 'type R = private ...' to give an accessibility to the whole representation.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:447)
    static member parsRecordFieldsCannotHaveVisibilityDeclarations() = (575, GetStringFunc("parsRecordFieldsCannotHaveVisibilityDeclarations",",,,") )
    /// The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:448)
    static member parsLetAndForNonRecBindings() = (576, GetStringFunc("parsLetAndForNonRecBindings",",,,") )
    /// Unmatched '('
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:449)
    static member parsUnmatchedParen() = (583, GetStringFunc("parsUnmatchedParen",",,,") )
    /// Successive patterns should be separated by spaces or tupled
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:450)
    static member parsSuccessivePatternsShouldBeSpacedOrTupled() = (584, GetStringFunc("parsSuccessivePatternsShouldBeSpacedOrTupled",",,,") )
    /// No matching 'in' found for this 'let'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:451)
    static member parsNoMatchingInForLet() = (586, GetStringFunc("parsNoMatchingInForLet",",,,") )
    /// Error in the return expression for this 'let'. Possible incorrect indentation.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:452)
    static member parsErrorInReturnForLetIncorrectIndentation() = (587, GetStringFunc("parsErrorInReturnForLetIncorrectIndentation",",,,") )
    /// Block following this '%s' is unfinished. Expect an expression.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:453)
    static member parsExpectedStatementAfterLet(a0 : System.String) = (588, GetStringFunc("parsExpectedStatementAfterLet",",,,%s,,,") a0)
    /// Incomplete conditional. Expected 'if <expr> then <expr>' or 'if <expr> then <expr> else <expr>'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:454)
    static member parsIncompleteIf() = (589, GetStringFunc("parsIncompleteIf",",,,") )
    /// 'assert' may not be used as a first class value. Use 'assert <expr>' instead.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:455)
    static member parsAssertIsNotFirstClassValue() = (590, GetStringFunc("parsAssertIsNotFirstClassValue",",,,") )
    /// Identifier expected
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:456)
    static member parsIdentifierExpected() = (594, GetStringFunc("parsIdentifierExpected",",,,") )
    /// 'in' or '=' expected
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:457)
    static member parsInOrEqualExpected() = (595, GetStringFunc("parsInOrEqualExpected",",,,") )
    /// The use of '->' in sequence and computation expressions is limited to the form 'for pat in expr -> expr'. Use the syntax 'for ... in ... do ... yield...' to generate elements in more complex sequence expressions.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:458)
    static member parsArrowUseIsLimited() = (596, GetStringFunc("parsArrowUseIsLimited",",,,") )
    /// Successive arguments should be separated by spaces or tupled, and arguments involving function or method applications should be parenthesized
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:459)
    static member parsSuccessiveArgsShouldBeSpacedOrTupled() = (597, GetStringFunc("parsSuccessiveArgsShouldBeSpacedOrTupled",",,,") )
    /// Unmatched '['
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:460)
    static member parsUnmatchedBracket() = (598, GetStringFunc("parsUnmatchedBracket",",,,") )
    /// Missing qualification after '.'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:461)
    static member parsMissingQualificationAfterDot() = (599, GetStringFunc("parsMissingQualificationAfterDot",",,,") )
    /// In F# code you may use 'expr.[expr]'. A type annotation may be required to indicate the first expression is an array
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:462)
    static member parsParenFormIsForML() = (GetStringFunc("parsParenFormIsForML",",,,") )
    /// Mismatched quotation, beginning with '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:463)
    static member parsMismatchedQuote(a0 : System.String) = (601, GetStringFunc("parsMismatchedQuote",",,,%s,,,") a0)
    /// Unmatched '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:464)
    static member parsUnmatched(a0 : System.String) = (602, GetStringFunc("parsUnmatched",",,,%s,,,") a0)
    /// Unmatched '[|'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:465)
    static member parsUnmatchedBracketBar() = (603, GetStringFunc("parsUnmatchedBracketBar",",,,") )
    /// Unmatched '{'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:466)
    static member parsUnmatchedBrace() = (604, GetStringFunc("parsUnmatchedBrace",",,,") )
    /// Field bindings must have the form 'id = expr;'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:467)
    static member parsFieldBinding() = (609, GetStringFunc("parsFieldBinding",",,,") )
    /// This member is not permitted in an object implementation
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:468)
    static member parsMemberIllegalInObjectImplementation() = (610, GetStringFunc("parsMemberIllegalInObjectImplementation",",,,") )
    /// Missing function body
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:469)
    static member parsMissingFunctionBody() = (611, GetStringFunc("parsMissingFunctionBody",",,,") )
    /// Syntax error in labelled type argument
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:470)
    static member parsSyntaxErrorInLabeledType() = (613, GetStringFunc("parsSyntaxErrorInLabeledType",",,,") )
    /// Unexpected infix operator in type expression
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:471)
    static member parsUnexpectedInfixOperator() = (615, GetStringFunc("parsUnexpectedInfixOperator",",,,") )
    /// The syntax '(typ,...,typ) ident' is not used in F# code. Consider using 'ident<typ,...,typ>' instead
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:472)
    static member parsMultiArgumentGenericTypeFormDeprecated() = (GetStringFunc("parsMultiArgumentGenericTypeFormDeprecated",",,,") )
    /// Invalid literal in type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:473)
    static member parsInvalidLiteralInType() = (618, GetStringFunc("parsInvalidLiteralInType",",,,") )
    /// Unexpected infix operator in unit-of-measure expression. Legal operators are '*', '/' and '^'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:474)
    static member parsUnexpectedOperatorForUnitOfMeasure() = (619, GetStringFunc("parsUnexpectedOperatorForUnitOfMeasure",",,,") )
    /// Unexpected integer literal in unit-of-measure expression
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:475)
    static member parsUnexpectedIntegerLiteralForUnitOfMeasure() = (620, GetStringFunc("parsUnexpectedIntegerLiteralForUnitOfMeasure",",,,") )
    /// Syntax error: unexpected type parameter specification
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:476)
    static member parsUnexpectedTypeParameter() = (621, GetStringFunc("parsUnexpectedTypeParameter",",,,") )
    /// Mismatched quotation operator name, beginning with '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:477)
    static member parsMismatchedQuotationName(a0 : System.String) = (622, GetStringFunc("parsMismatchedQuotationName",",,,%s,,,") a0)
    /// Active pattern case identifiers must begin with an uppercase letter
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:478)
    static member parsActivePatternCaseMustBeginWithUpperCase() = (623, GetStringFunc("parsActivePatternCaseMustBeginWithUpperCase",",,,") )
    /// No '=' symbol should follow a 'namespace' declaration
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:479)
    static member parsNoEqualShouldFollowNamespace() = (GetStringFunc("parsNoEqualShouldFollowNamespace",",,,") )
    /// The syntax 'module ... = struct .. end' is not used in F# code. Consider using 'module ... = begin .. end'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:480)
    static member parsSyntaxModuleStructEndDeprecated() = (GetStringFunc("parsSyntaxModuleStructEndDeprecated",",,,") )
    /// The syntax 'module ... : sig .. end' is not used in F# code. Consider using 'module ... = begin .. end'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:481)
    static member parsSyntaxModuleSigEndDeprecated() = (GetStringFunc("parsSyntaxModuleSigEndDeprecated",",,,") )
    /// A static field was used where an instance field is expected
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:485)
    static member tcStaticFieldUsedWhenInstanceFieldExpected() = (627, GetStringFunc("tcStaticFieldUsedWhenInstanceFieldExpected",",,,") )
    /// Method '%s' is not accessible from this code location
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:486)
    static member tcMethodNotAccessible(a0 : System.String) = (629, GetStringFunc("tcMethodNotAccessible",",,,%s,,,") a0)
    /// Implicit product of measures following /
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:488)
    static member tcImplicitMeasureFollowingSlash() = (632, GetStringFunc("tcImplicitMeasureFollowingSlash",",,,") )
    /// Unexpected SynMeasure.Anon
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:489)
    static member tcUnexpectedMeasureAnon() = (633, GetStringFunc("tcUnexpectedMeasureAnon",",,,") )
    /// Non-zero constants cannot have generic units. For generic zero, write 0.0<_>.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:490)
    static member tcNonZeroConstantCannotHaveGenericUnit() = (634, GetStringFunc("tcNonZeroConstantCannotHaveGenericUnit",",,,") )
    /// In sequence expressions, results are generated using 'yield'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:491)
    static member tcSeqResultsUseYield() = (635, GetStringFunc("tcSeqResultsUseYield",",,,") )
    /// Unexpected big rational constant
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:492)
    static member tcUnexpectedBigRationalConstant() = (GetStringFunc("tcUnexpectedBigRationalConstant",",,,") )
    /// Units-of-measure supported only on float, float32, decimal and signed integer types
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:493)
    static member tcInvalidTypeForUnitsOfMeasure() = (636, GetStringFunc("tcInvalidTypeForUnitsOfMeasure",",,,") )
    /// Unexpected Const_uint16array
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:494)
    static member tcUnexpectedConstUint16Array() = (GetStringFunc("tcUnexpectedConstUint16Array",",,,") )
    /// Unexpected Const_bytearray
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:495)
    static member tcUnexpectedConstByteArray() = (GetStringFunc("tcUnexpectedConstByteArray",",,,") )
    /// A parameter with attributes must also be given a name, e.g. '[<Attribute>] Name : Type'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:496)
    static member tcParameterRequiresName() = (640, GetStringFunc("tcParameterRequiresName",",,,") )
    /// Return values cannot have names
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:497)
    static member tcReturnValuesCannotHaveNames() = (641, GetStringFunc("tcReturnValuesCannotHaveNames",",,,") )
    /// MemberKind.PropertyGetSet only expected in parse trees
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:498)
    static member tcMemberKindPropertyGetSetNotExpected() = (GetStringFunc("tcMemberKindPropertyGetSetNotExpected",",,,") )
    /// Namespaces cannot contain values. Consider using a module to hold your value declarations.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:499)
    static member tcNamespaceCannotContainValues() = (201, GetStringFunc("tcNamespaceCannotContainValues",",,,") )
    /// Namespaces cannot contain extension members except in the same file and namespace where the type is defined. Consider using a module to hold declarations of extension members.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:500)
    static member tcNamespaceCannotContainExtensionMembers() = (644, GetStringFunc("tcNamespaceCannotContainExtensionMembers",",,,") )
    /// Multiple visibility attributes have been specified for this identifier
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:501)
    static member tcMultipleVisibilityAttributes() = (645, GetStringFunc("tcMultipleVisibilityAttributes",",,,") )
    /// Multiple visibility attributes have been specified for this identifier. 'let' bindings in classes are always private, as are any 'let' bindings inside expressions.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:502)
    static member tcMultipleVisibilityAttributesWithLet() = (646, GetStringFunc("tcMultipleVisibilityAttributesWithLet",",,,") )
    /// Unrecognized accessibility specification
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:503)
    static member tcUnrecognizedAccessibilitySpec() = (GetStringFunc("tcUnrecognizedAccessibilitySpec",",,,") )
    /// The name '(%s)' should not be used as a member name. To define comparison semantics for a type, implement the 'System.IComparable' interface. If defining a static member for use from other CLI languages then use the name '%s' instead.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:504)
    static member tcInvalidMethodNameForRelationalOperator(a0 : System.String, a1 : System.String) = (GetStringFunc("tcInvalidMethodNameForRelationalOperator",",,,%s,,,%s,,,") a0 a1)
    /// The name '(%s)' should not be used as a member name. To define equality semantics for a type, override the 'Object.Equals' member. If defining a static member for use from other CLI languages then use the name '%s' instead.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:505)
    static member tcInvalidMethodNameForEquality(a0 : System.String, a1 : System.String) = (GetStringFunc("tcInvalidMethodNameForEquality",",,,%s,,,%s,,,") a0 a1)
    /// The name '(%s)' should not be used as a member name. If defining a static member for use from other CLI languages then use the name '%s' instead.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:506)
    static member tcInvalidMemberName(a0 : System.String, a1 : System.String) = (GetStringFunc("tcInvalidMemberName",",,,%s,,,%s,,,") a0 a1)
    /// The name '(%s)' should not be used as a member name because it is given a standard definition in the F# library over fixed types
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:507)
    static member tcInvalidMemberNameFixedTypes(a0 : System.String) = (GetStringFunc("tcInvalidMemberNameFixedTypes",",,,%s,,,") a0)
    /// The '%s' operator should not normally be redefined. To define overloaded comparison semantics for a particular type, implement the 'System.IComparable' interface in the definition of that type.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:508)
    static member tcInvalidOperatorDefinitionRelational(a0 : System.String) = (GetStringFunc("tcInvalidOperatorDefinitionRelational",",,,%s,,,") a0)
    /// The '%s' operator should not normally be redefined. To define equality semantics for a type, override the 'Object.Equals' member in the definition of that type.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:509)
    static member tcInvalidOperatorDefinitionEquality(a0 : System.String) = (GetStringFunc("tcInvalidOperatorDefinitionEquality",",,,%s,,,") a0)
    /// The '%s' operator should not normally be redefined. Consider using a different operator name
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:510)
    static member tcInvalidOperatorDefinition(a0 : System.String) = (GetStringFunc("tcInvalidOperatorDefinition",",,,%s,,,") a0)
    /// The '%s' operator cannot be redefined. Consider using a different operator name
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:511)
    static member tcInvalidIndexOperatorDefinition(a0 : System.String) = (GetStringFunc("tcInvalidIndexOperatorDefinition",",,,%s,,,") a0)
    /// Expected module or namespace parent %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:512)
    static member tcExpectModuleOrNamespaceParent(a0 : System.String) = (GetStringFunc("tcExpectModuleOrNamespaceParent",",,,%s,,,") a0)
    /// The struct, record or union type '%s' implements the interface 'System.IComparable' explicitly. You must apply the 'CustomComparison' attribute to the type.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:513)
    static member tcImplementsIComparableExplicitly(a0 : System.String) = (647, GetStringFunc("tcImplementsIComparableExplicitly",",,,%s,,,") a0)
    /// The struct, record or union type '%s' implements the interface 'System.IComparable<_>' explicitly. You must apply the 'CustomComparison' attribute to the type, and should also provide a consistent implementation of the non-generic interface System.IComparable.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:514)
    static member tcImplementsGenericIComparableExplicitly(a0 : System.String) = (648, GetStringFunc("tcImplementsGenericIComparableExplicitly",",,,%s,,,") a0)
    /// The struct, record or union type '%s' implements the interface 'System.IStructuralComparable' explicitly. Apply the 'CustomComparison' attribute to the type.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:515)
    static member tcImplementsIStructuralComparableExplicitly(a0 : System.String) = (649, GetStringFunc("tcImplementsIStructuralComparableExplicitly",",,,%s,,,") a0)
    /// This record contains fields from inconsistent types
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:516)
    static member tcRecordFieldInconsistentTypes() = (656, GetStringFunc("tcRecordFieldInconsistentTypes",",,,") )
    /// DLLImport stubs cannot be inlined
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:517)
    static member tcDllImportStubsCannotBeInlined() = (657, GetStringFunc("tcDllImportStubsCannotBeInlined",",,,") )
    /// Structs may only bind a 'this' parameter at member declarations
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:518)
    static member tcStructsCanOnlyBindThisAtMemberDeclaration() = (658, GetStringFunc("tcStructsCanOnlyBindThisAtMemberDeclaration",",,,") )
    /// Unexpected expression at recursive inference point
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:519)
    static member tcUnexpectedExprAtRecInfPoint() = (659, GetStringFunc("tcUnexpectedExprAtRecInfPoint",",,,") )
    /// This code is less generic than required by its annotations because the explicit type variable '%s' could not be generalized. It was constrained to be '%s'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:520)
    static member tcLessGenericBecauseOfAnnotation(a0 : System.String, a1 : System.String) = (660, GetStringFunc("tcLessGenericBecauseOfAnnotation",",,,%s,,,%s,,,") a0 a1)
    /// One or more of the explicit class or function type variables for this binding could not be generalized, because they were constrained to other types
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:521)
    static member tcConstrainedTypeVariableCannotBeGeneralized() = (661, GetStringFunc("tcConstrainedTypeVariableCannotBeGeneralized",",,,") )
    /// A generic type parameter has been used in a way that constrains it to always be '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:522)
    static member tcGenericParameterHasBeenConstrained(a0 : System.String) = (662, GetStringFunc("tcGenericParameterHasBeenConstrained",",,,%s,,,") a0)
    /// This type parameter has been used in a way that constrains it to always be '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:523)
    static member tcTypeParameterHasBeenConstrained(a0 : System.String) = (663, GetStringFunc("tcTypeParameterHasBeenConstrained",",,,%s,,,") a0)
    /// The type parameters inferred for this value are not stable under the erasure of type abbreviations. This is due to the use of type abbreviations which drop or reorder type parameters, e.g. \n\ttype taggedInt<'a> = int or\n\ttype swap<'a,'b> = 'b * 'a.\nConsider declaring the type parameters for this value explicitly, e.g.\n\tlet f<'a,'b> ((x,y) : swap<'b,'a>) : swap<'a,'b> = (y,x).
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:524)
    static member tcTypeParametersInferredAreNotStable() = (664, GetStringFunc("tcTypeParametersInferredAreNotStable",",,,") )
    /// Explicit type parameters may only be used on module or member bindings
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:525)
    static member tcExplicitTypeParameterInvalid() = (665, GetStringFunc("tcExplicitTypeParameterInvalid",",,,") )
    /// You must explicitly declare either all or no type parameters when overriding a generic abstract method
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:526)
    static member tcOverridingMethodRequiresAllOrNoTypeParameters() = (666, GetStringFunc("tcOverridingMethodRequiresAllOrNoTypeParameters",",,,") )
    /// The field labels and expected type of this record expression or pattern do not uniquely determine a corresponding record type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:527)
    static member tcFieldsDoNotDetermineUniqueRecordType() = (667, GetStringFunc("tcFieldsDoNotDetermineUniqueRecordType",",,,") )
    /// The field '%s' appears twice in this record expression or pattern
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:528)
    static member tcFieldAppearsTwiceInRecord(a0 : System.String) = (668, GetStringFunc("tcFieldAppearsTwiceInRecord",",,,%s,,,") a0)
    /// Unknown union case
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:529)
    static member tcUnknownUnion() = (669, GetStringFunc("tcUnknownUnion",",,,") )
    /// This code is not sufficiently generic. The type variable %s could not be generalized because it would escape its scope.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:530)
    static member tcNotSufficientlyGenericBecauseOfScope(a0 : System.String) = (670, GetStringFunc("tcNotSufficientlyGenericBecauseOfScope",",,,%s,,,") a0)
    /// A property cannot have explicit type parameters. Consider using a method instead.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:531)
    static member tcPropertyRequiresExplicitTypeParameters() = (671, GetStringFunc("tcPropertyRequiresExplicitTypeParameters",",,,") )
    /// A constructor cannot have explicit type parameters. Consider using a static construction method instead.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:532)
    static member tcConstructorCannotHaveTypeParameters() = (672, GetStringFunc("tcConstructorCannotHaveTypeParameters",",,,") )
    /// This instance member needs a parameter to represent the object being invoked. Make the member static or use the notation 'member x.Member(args) = ...'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:533)
    static member tcInstanceMemberRequiresTarget() = (673, GetStringFunc("tcInstanceMemberRequiresTarget",",,,") )
    /// Unexpected source-level property specification in syntax tree
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:534)
    static member tcUnexpectedPropertyInSyntaxTree() = (674, GetStringFunc("tcUnexpectedPropertyInSyntaxTree",",,,") )
    /// A static initializer requires an argument
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:535)
    static member tcStaticInitializerRequiresArgument() = (675, GetStringFunc("tcStaticInitializerRequiresArgument",",,,") )
    /// An object constructor requires an argument
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:536)
    static member tcObjectConstructorRequiresArgument() = (676, GetStringFunc("tcObjectConstructorRequiresArgument",",,,") )
    /// This static member should not have a 'this' parameter. Consider using the notation 'member Member(args) = ...'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:537)
    static member tcStaticMemberShouldNotHaveThis() = (677, GetStringFunc("tcStaticMemberShouldNotHaveThis",",,,") )
    /// An explicit static initializer should use the syntax 'static new(args) = expr'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:538)
    static member tcExplicitStaticInitializerSyntax() = (678, GetStringFunc("tcExplicitStaticInitializerSyntax",",,,") )
    /// An explicit object constructor should use the syntax 'new(args) = expr'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:539)
    static member tcExplicitObjectConstructorSyntax() = (679, GetStringFunc("tcExplicitObjectConstructorSyntax",",,,") )
    /// Unexpected source-level property specification
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:540)
    static member tcUnexpectedPropertySpec() = (680, GetStringFunc("tcUnexpectedPropertySpec",",,,") )
    /// This form of object expression is not used in F#. Use 'member this.MemberName ... = ...' to define member implementations in object expressions.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:541)
    static member tcObjectExpressionFormDeprecated() = (GetStringFunc("tcObjectExpressionFormDeprecated",",,,") )
    /// Invalid declaration
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:542)
    static member tcInvalidDeclaration() = (682, GetStringFunc("tcInvalidDeclaration",",,,") )
    /// Attributes are not allowed within patterns
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:543)
    static member tcAttributesInvalidInPatterns() = (683, GetStringFunc("tcAttributesInvalidInPatterns",",,,") )
    /// The generic function '%s' must be given explicit type argument(s)
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:544)
    static member tcFunctionRequiresExplicitTypeArguments(a0 : System.String) = (685, GetStringFunc("tcFunctionRequiresExplicitTypeArguments",",,,%s,,,") a0)
    /// The method or function '%s' should not be given explicit type argument(s) because it does not declare its type parameters explicitly
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:545)
    static member tcDoesNotAllowExplicitTypeArguments(a0 : System.String) = (686, GetStringFunc("tcDoesNotAllowExplicitTypeArguments",",,,%s,,,") a0)
    /// This value, type or method expects %d type parameter(s) but was given %d
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:546)
    static member tcTypeParameterArityMismatch(a0 : System.Int32, a1 : System.Int32) = (687, GetStringFunc("tcTypeParameterArityMismatch",",,,%d,,,%d,,,") a0 a1)
    /// The default, zero-initializing constructor of a struct type may only be used if all the fields of the struct type admit default initialization
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:547)
    static member tcDefaultStructConstructorCall() = (688, GetStringFunc("tcDefaultStructConstructorCall",",,,") )
    /// Couldn't find Dispose on IDisposable, or it was overloaded
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:548)
    static member tcCouldNotFindIDisposable() = (GetStringFunc("tcCouldNotFindIDisposable",",,,") )
    /// This value is not a literal and cannot be used in a pattern
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:549)
    static member tcNonLiteralCannotBeUsedInPattern() = (689, GetStringFunc("tcNonLiteralCannotBeUsedInPattern",",,,") )
    /// This field is readonly
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:550)
    static member tcFieldIsReadonly() = (690, GetStringFunc("tcFieldIsReadonly",",,,") )
    /// Named arguments must appear after all other arguments
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:551)
    static member tcNameArgumentsMustAppearLast() = (691, GetStringFunc("tcNameArgumentsMustAppearLast",",,,") )
    /// This function value is being used to construct a delegate type whose signature includes a byref argument. You must use an explicit lambda expression taking %d arguments.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:552)
    static member tcFunctionRequiresExplicitLambda(a0 : System.Int32) = (692, GetStringFunc("tcFunctionRequiresExplicitLambda",",,,%d,,,") a0)
    /// The type '%s' is not a type whose values can be enumerated with this syntax, i.e. is not compatible with either seq<_>, IEnumerable<_> or IEnumerable and does not have a GetEnumerator method
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:553)
    static member tcTypeCannotBeEnumerated(a0 : System.String) = (693, GetStringFunc("tcTypeCannotBeEnumerated",",,,%s,,,") a0)
    /// This expression has a method called GetEnumerator, but its return type is a value type. Methods returning struct enumerators cannot be used in this expression form.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:554)
    static member tcBadReturnTypeForGetEnumerator() = (694, GetStringFunc("tcBadReturnTypeForGetEnumerator",",,,") )
    /// This recursive binding uses an invalid mixture of recursive forms
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:555)
    static member tcInvalidMixtureOfRecursiveForms() = (695, GetStringFunc("tcInvalidMixtureOfRecursiveForms",",,,") )
    /// This is not a valid object construction expression. Explicit object constructors must either call an alternate constructor or initialize all fields of the object and specify a call to a super class constructor.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:556)
    static member tcInvalidObjectConstructionExpression() = (696, GetStringFunc("tcInvalidObjectConstructionExpression",",,,") )
    /// Invalid constraint
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:557)
    static member tcInvalidConstraint() = (697, GetStringFunc("tcInvalidConstraint",",,,") )
    /// Invalid constraint: the type used for the constraint is sealed, which means the constraint could only be satisfied by at most one solution
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:558)
    static member tcInvalidConstraintTypeSealed() = (698, GetStringFunc("tcInvalidConstraintTypeSealed",",,,") )
    /// An 'enum' constraint must be of the form 'enum<type>'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:559)
    static member tcInvalidEnumConstraint() = (699, GetStringFunc("tcInvalidEnumConstraint",",,,") )
    /// 'new' constraints must take one argument of type 'unit' and return the constructed type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:560)
    static member tcInvalidNewConstraint() = (700, GetStringFunc("tcInvalidNewConstraint",",,,") )
    /// This property has an invalid type. Properties taking multiple indexer arguments should have types of the form 'ty1 * ty2 -> ty3'. Properties returning functions should have types of the form '(ty1 -> ty2)'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:561)
    static member tcInvalidPropertyType() = (701, GetStringFunc("tcInvalidPropertyType",",,,") )
    /// Expected unit-of-measure parameter, not type parameter. Explicit unit-of-measure parameters must be marked with the [<Measure>] attribute.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:562)
    static member tcExpectedUnitOfMeasureMarkWithAttribute() = (702, GetStringFunc("tcExpectedUnitOfMeasureMarkWithAttribute",",,,") )
    /// Expected type parameter, not unit-of-measure parameter
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:563)
    static member tcExpectedTypeParameter() = (703, GetStringFunc("tcExpectedTypeParameter",",,,") )
    /// Expected type, not unit-of-measure
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:564)
    static member tcExpectedTypeNotUnitOfMeasure() = (704, GetStringFunc("tcExpectedTypeNotUnitOfMeasure",",,,") )
    /// Expected unit-of-measure, not type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:565)
    static member tcExpectedUnitOfMeasureNotType() = (705, GetStringFunc("tcExpectedUnitOfMeasureNotType",",,,") )
    /// Units-of-measure cannot be used as prefix arguments to a type. Rewrite as postfix arguments in angle brackets.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:566)
    static member tcInvalidUnitsOfMeasurePrefix() = (706, GetStringFunc("tcInvalidUnitsOfMeasurePrefix",",,,") )
    /// Unit-of-measure cannot be used in type constructor application
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:567)
    static member tcUnitsOfMeasureInvalidInTypeConstructor() = (707, GetStringFunc("tcUnitsOfMeasureInvalidInTypeConstructor",",,,") )
    /// This control construct may only be used if the computation expression builder defines a '%s' method
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:568)
    static member tcRequireBuilderMethod(a0 : System.String) = (708, GetStringFunc("tcRequireBuilderMethod",",,,%s,,,") a0)
    /// This type has no nested types
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:569)
    static member tcTypeHasNoNestedTypes() = (709, GetStringFunc("tcTypeHasNoNestedTypes",",,,") )
    /// Unexpected %s in type expression
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:570)
    static member tcUnexpectedSymbolInTypeExpression(a0 : System.String) = (711, GetStringFunc("tcUnexpectedSymbolInTypeExpression",",,,%s,,,") a0)
    /// Type parameter cannot be used as type constructor
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:571)
    static member tcTypeParameterInvalidAsTypeConstructor() = (712, GetStringFunc("tcTypeParameterInvalidAsTypeConstructor",",,,") )
    /// Illegal syntax in type expression
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:572)
    static member tcIllegalSyntaxInTypeExpression() = (713, GetStringFunc("tcIllegalSyntaxInTypeExpression",",,,") )
    /// Anonymous unit-of-measure cannot be nested inside another unit-of-measure expression
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:573)
    static member tcAnonymousUnitsOfMeasureCannotBeNested() = (714, GetStringFunc("tcAnonymousUnitsOfMeasureCannotBeNested",",,,") )
    /// Anonymous type variables are not permitted in this declaration
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:574)
    static member tcAnonymousTypeInvalidInDeclaration() = (715, GetStringFunc("tcAnonymousTypeInvalidInDeclaration",",,,") )
    /// Unexpected / in type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:575)
    static member tcUnexpectedSlashInType() = (716, GetStringFunc("tcUnexpectedSlashInType",",,,") )
    /// Unexpected type arguments
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:576)
    static member tcUnexpectedTypeArguments() = (717, GetStringFunc("tcUnexpectedTypeArguments",",,,") )
    /// Optional arguments are only permitted on type members
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:577)
    static member tcOptionalArgsOnlyOnMembers() = (718, GetStringFunc("tcOptionalArgsOnlyOnMembers",",,,") )
    /// Name '%s' not bound in pattern context
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:578)
    static member tcNameNotBoundInPattern(a0 : System.String) = (719, GetStringFunc("tcNameNotBoundInPattern",",,,%s,,,") a0)
    /// Non-primitive numeric literal constants cannot be used in pattern matches because they can be mapped to multiple different types through the use of a NumericLiteral module. Consider using replacing with a variable, and use 'when <variable> = <constant>' at the end of the match clause.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:579)
    static member tcInvalidNonPrimitiveLiteralInPatternMatch() = (720, GetStringFunc("tcInvalidNonPrimitiveLiteralInPatternMatch",",,,") )
    /// Type arguments cannot be specified here
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:580)
    static member tcInvalidTypeArgumentUsage() = (721, GetStringFunc("tcInvalidTypeArgumentUsage",",,,") )
    /// Only active patterns returning exactly one result may accept arguments
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:581)
    static member tcRequireActivePatternWithOneResult() = (722, GetStringFunc("tcRequireActivePatternWithOneResult",",,,") )
    /// Invalid argument to parameterized pattern label
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:582)
    static member tcInvalidArgForParameterizedPattern() = (723, GetStringFunc("tcInvalidArgForParameterizedPattern",",,,") )
    /// Internal error. Invalid index into active pattern array
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:583)
    static member tcInvalidIndexIntoActivePatternArray() = (724, GetStringFunc("tcInvalidIndexIntoActivePatternArray",",,,") )
    /// This union case does not take arguments
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:584)
    static member tcUnionCaseDoesNotTakeArguments() = (725, GetStringFunc("tcUnionCaseDoesNotTakeArguments",",,,") )
    /// This union case takes one argument
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:585)
    static member tcUnionCaseRequiresOneArgument() = (726, GetStringFunc("tcUnionCaseRequiresOneArgument",",,,") )
    /// This union case expects %d arguments in tupled form
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:586)
    static member tcUnionCaseExpectsTupledArguments(a0 : System.Int32) = (727, GetStringFunc("tcUnionCaseExpectsTupledArguments",",,,%d,,,") a0)
    /// Field '%s' is not static
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:587)
    static member tcFieldIsNotStatic(a0 : System.String) = (728, GetStringFunc("tcFieldIsNotStatic",",,,%s,,,") a0)
    /// This field is not a literal and cannot be used in a pattern
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:588)
    static member tcFieldNotLiteralCannotBeUsedInPattern() = (729, GetStringFunc("tcFieldNotLiteralCannotBeUsedInPattern",",,,") )
    /// This is not a variable, constant, active recognizer or literal
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:589)
    static member tcRequireVarConstRecogOrLiteral() = (730, GetStringFunc("tcRequireVarConstRecogOrLiteral",",,,") )
    /// This is not a valid pattern
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:590)
    static member tcInvalidPattern() = (731, GetStringFunc("tcInvalidPattern",",,,") )
    /// Character range matches have been removed in F#. Consider using a 'when' pattern guard instead.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:591)
    static member tcUseWhenPatternGuard() = (GetStringFunc("tcUseWhenPatternGuard",",,,") )
    /// Illegal pattern
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:592)
    static member tcIllegalPattern() = (733, GetStringFunc("tcIllegalPattern",",,,") )
    /// Syntax error - unexpected '?' symbol
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:593)
    static member tcSyntaxErrorUnexpectedQMark() = (734, GetStringFunc("tcSyntaxErrorUnexpectedQMark",",,,") )
    /// Expected %d expressions, got %d
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:594)
    static member tcExpressionCountMisMatch(a0 : System.Int32, a1 : System.Int32) = (735, GetStringFunc("tcExpressionCountMisMatch",",,,%d,,,%d,,,") a0 a1)
    /// TcExprUndelayed: delayed
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:595)
    static member tcExprUndelayed() = (736, GetStringFunc("tcExprUndelayed",",,,") )
    /// This expression form may only be used in sequence and computation expressions
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:596)
    static member tcExpressionRequiresSequence() = (737, GetStringFunc("tcExpressionRequiresSequence",",,,") )
    /// Invalid object expression. Objects without overrides or interfaces should use the expression form 'new Type(args)' without braces.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:597)
    static member tcInvalidObjectExpressionSyntaxForm() = (738, GetStringFunc("tcInvalidObjectExpressionSyntaxForm",",,,") )
    /// Invalid object, sequence or record expression
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:598)
    static member tcInvalidObjectSequenceOrRecordExpression() = (739, GetStringFunc("tcInvalidObjectSequenceOrRecordExpression",",,,") )
    /// Invalid record, sequence or computation expression. Sequence expressions should be of the form 'seq { ... }'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:599)
    static member tcInvalidSequenceExpressionSyntaxForm() = (740, GetStringFunc("tcInvalidSequenceExpressionSyntaxForm",",,,") )
    /// This list or array expression includes an element of the form 'if ... then ... else'. Parenthesize this expression to indicate it is an individual element of the list or array, to disambiguate this from a list generated using a sequence expression
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:600)
    static member tcExpressionWithIfRequiresParenthesis() = (GetStringFunc("tcExpressionWithIfRequiresParenthesis",",,,") )
    /// Unable to parse format string '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:601)
    static member tcUnableToParseFormatString(a0 : System.String) = (741, GetStringFunc("tcUnableToParseFormatString",",,,%s,,,") a0)
    /// This list expression exceeds the maximum size for list literals. Use an array for larger literals and call Array.ToList.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:602)
    static member tcListLiteralMaxSize() = (742, GetStringFunc("tcListLiteralMaxSize",",,,") )
    /// The expression form 'expr then expr' may only be used as part of an explicit object constructor
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:603)
    static member tcExpressionFormRequiresObjectConstructor() = (743, GetStringFunc("tcExpressionFormRequiresObjectConstructor",",,,") )
    /// Named arguments cannot be given to member trait calls
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:604)
    static member tcNamedArgumentsCannotBeUsedInMemberTraits() = (744, GetStringFunc("tcNamedArgumentsCannotBeUsedInMemberTraits",",,,") )
    /// This is not a valid name for an enumeration case
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:605)
    static member tcNotValidEnumCaseName() = (745, GetStringFunc("tcNotValidEnumCaseName",",,,") )
    /// This field is not mutable
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:606)
    static member tcFieldIsNotMutable() = (746, GetStringFunc("tcFieldIsNotMutable",",,,") )
    /// This construct may only be used within list, array and sequence expressions, e.g. expressions of the form 'seq { ... }', '[ ... ]' or '[| ... |]'. These use the syntax 'for ... in ... do ... yield...' to generate elements
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:607)
    static member tcConstructRequiresListArrayOrSequence() = (747, GetStringFunc("tcConstructRequiresListArrayOrSequence",",,,") )
    /// This construct may only be used within computation expressions. To return a value from an ordinary function simply write the expression without 'return'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:608)
    static member tcConstructRequiresComputationExpressions() = (748, GetStringFunc("tcConstructRequiresComputationExpressions",",,,") )
    /// This construct may only be used within sequence or computation expressions
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:609)
    static member tcConstructRequiresSequenceOrComputations() = (749, GetStringFunc("tcConstructRequiresSequenceOrComputations",",,,") )
    /// This construct may only be used within computation expressions
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:610)
    static member tcConstructRequiresComputationExpression() = (750, GetStringFunc("tcConstructRequiresComputationExpression",",,,") )
    /// Invalid indexer expression
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:611)
    static member tcInvalidIndexerExpression() = (751, GetStringFunc("tcInvalidIndexerExpression",",,,") )
    /// The operator 'expr.[idx]' has been used on an object of indeterminate type based on information prior to this program point. Consider adding further type constraints
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:612)
    static member tcObjectOfIndeterminateTypeUsedRequireTypeConstraint() = (752, GetStringFunc("tcObjectOfIndeterminateTypeUsedRequireTypeConstraint",",,,") )
    /// Cannot inherit from a variable type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:613)
    static member tcCannotInheritFromVariableType() = (753, GetStringFunc("tcCannotInheritFromVariableType",",,,") )
    /// Calls to object constructors on type parameters cannot be given arguments
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:614)
    static member tcObjectConstructorsOnTypeParametersCannotTakeArguments() = (754, GetStringFunc("tcObjectConstructorsOnTypeParametersCannotTakeArguments",",,,") )
    /// The 'CompiledName' attribute cannot be used with this language element
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:615)
    static member tcCompiledNameAttributeMisused() = (755, GetStringFunc("tcCompiledNameAttributeMisused",",,,") )
    /// '%s' may only be used with named types
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:616)
    static member tcNamedTypeRequired(a0 : System.String) = (756, GetStringFunc("tcNamedTypeRequired",",,,%s,,,") a0)
    /// 'inherit' cannot be used on interface types. Consider implementing the interface by using 'interface ... with ... end' instead.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:617)
    static member tcInheritCannotBeUsedOnInterfaceType() = (757, GetStringFunc("tcInheritCannotBeUsedOnInterfaceType",",,,") )
    /// 'new' cannot be used on interface types. Consider using an object expression '{ new ... with ... }' instead.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:618)
    static member tcNewCannotBeUsedOnInterfaceType() = (758, GetStringFunc("tcNewCannotBeUsedOnInterfaceType",",,,") )
    /// Instances of this type cannot be created since it has been marked abstract or not all methods have been given implementations. Consider using an object expression '{ new ... with ... }' instead.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:619)
    static member tcAbstractTypeCannotBeInstantiated() = (759, GetStringFunc("tcAbstractTypeCannotBeInstantiated",",,,") )
    /// It is recommended that objects that support the IDisposable interface are created using 'new Type(args)' rather than 'Type(args)' to indicate that resources may be owned by the generated value
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:620)
    static member tcIDisposableTypeShouldUseNew() = (760, GetStringFunc("tcIDisposableTypeShouldUseNew",",,,") )
    /// '%s' may only be used to construct object types
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:621)
    static member tcSyntaxCanOnlyBeUsedToCreateObjectTypes(a0 : System.String) = (761, GetStringFunc("tcSyntaxCanOnlyBeUsedToCreateObjectTypes",",,,%s,,,") a0)
    /// Constructors for the type '%s' must directly or indirectly call its implicit object constructor. Use a call to the implicit object constructor instead of a record expression.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:622)
    static member tcConstructorRequiresCall(a0 : System.String) = (762, GetStringFunc("tcConstructorRequiresCall",",,,%s,,,") a0)
    /// The field '%s' has been given a value, but is not present in the type '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:623)
    static member tcUndefinedField(a0 : System.String, a1 : System.String) = (763, GetStringFunc("tcUndefinedField",",,,%s,,,%s,,,") a0 a1)
    /// No assignment given for field '%s' of type '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:624)
    static member tcFieldRequiresAssignment(a0 : System.String, a1 : System.String) = (764, GetStringFunc("tcFieldRequiresAssignment",",,,%s,,,%s,,,") a0 a1)
    /// Extraneous fields have been given values
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:625)
    static member tcExtraneousFieldsGivenValues() = (765, GetStringFunc("tcExtraneousFieldsGivenValues",",,,") )
    /// Only overrides of abstract and virtual members may be specified in object expressions
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:626)
    static member tcObjectExpressionsCanOnlyOverrideAbstractOrVirtual() = (766, GetStringFunc("tcObjectExpressionsCanOnlyOverrideAbstractOrVirtual",",,,") )
    /// The member '%s' does not correspond to any abstract or virtual method available to override or implement
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:627)
    static member tcNoAbstractOrVirtualMemberFound(a0 : System.String) = (767, GetStringFunc("tcNoAbstractOrVirtualMemberFound",",,,%s,,,") a0)
    /// The member '%s' does not accept the correct number of arguments, %d arguments are expected
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:628)
    static member tcArgumentArityMismatch(a0 : System.String, a1 : System.Int32) = (768, GetStringFunc("tcArgumentArityMismatch",",,,%s,,,%d,,,") a0 a1)
    /// The member '%s' does not accept the correct number of arguments. One overload accepts %d arguments.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:629)
    static member tcArgumentArityMismatchOneOverload(a0 : System.String, a1 : System.Int32) = (769, GetStringFunc("tcArgumentArityMismatchOneOverload",",,,%s,,,%d,,,") a0 a1)
    /// A simple method name is required here
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:630)
    static member tcSimpleMethodNameRequired() = (770, GetStringFunc("tcSimpleMethodNameRequired",",,,") )
    /// The types System.ValueType, System.Enum, System.Delegate, System.MulticastDelegate and System.Array cannot be used as super types in an object expression or class
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:631)
    static member tcPredefinedTypeCannotBeUsedAsSuperType() = (771, GetStringFunc("tcPredefinedTypeCannotBeUsedAsSuperType",",,,") )
    /// 'new' must be used with a named type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:632)
    static member tcNewMustBeUsedWithNamedType() = (772, GetStringFunc("tcNewMustBeUsedWithNamedType",",,,") )
    /// Cannot create an extension of a sealed type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:633)
    static member tcCannotCreateExtensionOfSealedType() = (773, GetStringFunc("tcCannotCreateExtensionOfSealedType",",,,") )
    /// No arguments may be given when constructing a record value
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:634)
    static member tcNoArgumentsForRecordValue() = (774, GetStringFunc("tcNoArgumentsForRecordValue",",,,") )
    /// Interface implementations cannot be given on construction expressions
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:635)
    static member tcNoInterfaceImplementationForConstructionExpression() = (775, GetStringFunc("tcNoInterfaceImplementationForConstructionExpression",",,,") )
    /// Object construction expressions may only be used to implement constructors in class types
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:636)
    static member tcObjectConstructionCanOnlyBeUsedInClassTypes() = (776, GetStringFunc("tcObjectConstructionCanOnlyBeUsedInClassTypes",",,,") )
    /// Only simple bindings of the form 'id = expr' can be used in construction expressions
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:637)
    static member tcOnlySimpleBindingsCanBeUsedInConstructionExpressions() = (777, GetStringFunc("tcOnlySimpleBindingsCanBeUsedInConstructionExpressions",",,,") )
    /// Objects must be initialized by an object construction expression that calls an inherited object constructor and assigns a value to each field
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:638)
    static member tcObjectsMustBeInitializedWithObjectExpression() = (778, GetStringFunc("tcObjectsMustBeInitializedWithObjectExpression",",,,") )
    /// Expected an interface type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:639)
    static member tcExpectedInterfaceType() = (779, GetStringFunc("tcExpectedInterfaceType",",,,") )
    /// Constructor expressions for interfaces do not take arguments
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:640)
    static member tcConstructorForInterfacesDoNotTakeArguments() = (780, GetStringFunc("tcConstructorForInterfacesDoNotTakeArguments",",,,") )
    /// This object constructor requires arguments
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:641)
    static member tcConstructorRequiresArguments() = (781, GetStringFunc("tcConstructorRequiresArguments",",,,") )
    /// 'new' may only be used with object constructors
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:642)
    static member tcNewRequiresObjectConstructor() = (782, GetStringFunc("tcNewRequiresObjectConstructor",",,,") )
    /// At least one override did not correctly implement its corresponding abstract member
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:643)
    static member tcAtLeastOneOverrideIsInvalid() = (783, GetStringFunc("tcAtLeastOneOverrideIsInvalid",",,,") )
    /// This numeric literal requires that a module '%s' defining functions FromZero, FromOne, FromInt32, FromInt64 and FromString be in scope
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:644)
    static member tcNumericLiteralRequiresModule(a0 : System.String) = (784, GetStringFunc("tcNumericLiteralRequiresModule",",,,%s,,,") a0)
    /// Invalid record construction
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:645)
    static member tcInvalidRecordConstruction() = (785, GetStringFunc("tcInvalidRecordConstruction",",,,") )
    /// The expression form { expr with ... } may only be used with record types. To build object types use { new Type(...) with ... }
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:646)
    static member tcExpressionFormRequiresRecordTypes() = (786, GetStringFunc("tcExpressionFormRequiresRecordTypes",",,,") )
    /// The inherited type is not an object model type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:647)
    static member tcInheritedTypeIsNotObjectModelType() = (787, GetStringFunc("tcInheritedTypeIsNotObjectModelType",",,,") )
    /// Object construction expressions (i.e. record expressions with inheritance specifications) may only be used to implement constructors in object model types. Use 'new ObjectType(args)' to construct instances of object model types outside of constructors
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:648)
    static member tcObjectConstructionExpressionCanOnlyImplementConstructorsInObjectModelTypes() = (788, GetStringFunc("tcObjectConstructionExpressionCanOnlyImplementConstructorsInObjectModelTypes",",,,") )
    /// '{ }' is not a valid expression. Records must include at least one field. Empty sequences are specified by using Seq.empty or an empty list '[]'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:649)
    static member tcEmptyRecordInvalid() = (789, GetStringFunc("tcEmptyRecordInvalid",",,,") )
    /// This type is not a record type. Values of class and struct types must be created using calls to object constructors.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:650)
    static member tcTypeIsNotARecordTypeNeedConstructor() = (790, GetStringFunc("tcTypeIsNotARecordTypeNeedConstructor",",,,") )
    /// This type is not a record type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:651)
    static member tcTypeIsNotARecordType() = (791, GetStringFunc("tcTypeIsNotARecordType",",,,") )
    /// This construct is ambiguous as part of a computation expression. Nested expressions may be written using 'let _ = (...)' and nested computations using 'let! res = builder { ... }'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:652)
    static member tcConstructIsAmbiguousInComputationExpression() = (792, GetStringFunc("tcConstructIsAmbiguousInComputationExpression",",,,") )
    /// This construct is ambiguous as part of a sequence expression. Nested expressions may be written using 'let _ = (...)' and nested sequences using 'yield! seq {... }'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:653)
    static member tcConstructIsAmbiguousInSequenceExpression() = (793, GetStringFunc("tcConstructIsAmbiguousInSequenceExpression",",,,") )
    /// 'do!' cannot be used within sequence expressions
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:654)
    static member tcDoBangIllegalInSequenceExpression() = (794, GetStringFunc("tcDoBangIllegalInSequenceExpression",",,,") )
    /// The use of 'let! x = coll' in sequence expressions is not permitted. Use 'for x in coll' instead.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:655)
    static member tcUseForInSequenceExpression() = (795, GetStringFunc("tcUseForInSequenceExpression",",,,") )
    /// 'try'/'with' cannot be used within sequence expressions
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:656)
    static member tcTryIllegalInSequenceExpression() = (796, GetStringFunc("tcTryIllegalInSequenceExpression",",,,") )
    /// In sequence expressions, multiple results are generated using 'yield!'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:657)
    static member tcUseYieldBangForMultipleResults() = (797, GetStringFunc("tcUseYieldBangForMultipleResults",",,,") )
    /// Invalid assignment
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:658)
    static member tcInvalidAssignment() = (799, GetStringFunc("tcInvalidAssignment",",,,") )
    /// Invalid use of a type name
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:659)
    static member tcInvalidUseOfTypeName() = (800, GetStringFunc("tcInvalidUseOfTypeName",",,,") )
    /// This type has no accessible object constructors
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:660)
    static member tcTypeHasNoAccessibleConstructor() = (801, GetStringFunc("tcTypeHasNoAccessibleConstructor",",,,") )
    /// Invalid use of a type name and/or object constructor. If necessary use 'new' and apply the constructor to its arguments, e.g. 'new Type(args)'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:661)
    static member tcInvalidUseOfTypeNameOrConstructor() = (802, GetStringFunc("tcInvalidUseOfTypeNameOrConstructor",",,,") )
    /// Invalid use of a type name and/or object constructor. If necessary use 'new' and apply the constructor to its arguments, e.g. 'new Type(args)'. The required signature is:\n\t%s.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:662)
    static member tcInvalidUseOfTypeNameOrConstructorWithOverloads(a0 : System.String) = (803, GetStringFunc("tcInvalidUseOfTypeNameOrConstructorWithOverloads",",,,%s,,,") a0)
    /// Invalid use of an interface type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:663)
    static member tcInvalidUseOfInterfaceType() = (804, GetStringFunc("tcInvalidUseOfInterfaceType",",,,") )
    /// Invalid use of a delegate constructor. Use the syntax 'new Type(args)' or just 'Type(args)'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:664)
    static member tcInvalidUseOfDelegate() = (805, GetStringFunc("tcInvalidUseOfDelegate",",,,") )
    /// Property '%s' is not static
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:665)
    static member tcPropertyIsNotStatic(a0 : System.String) = (806, GetStringFunc("tcPropertyIsNotStatic",",,,%s,,,") a0)
    /// Property '%s' is not readable
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:666)
    static member tcPropertyIsNotReadable(a0 : System.String) = (807, GetStringFunc("tcPropertyIsNotReadable",",,,%s,,,") a0)
    /// This lookup cannot be used here
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:667)
    static member tcLookupMayNotBeUsedHere() = (808, GetStringFunc("tcLookupMayNotBeUsedHere",",,,") )
    /// Property '%s' is static
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:668)
    static member tcPropertyIsStatic(a0 : System.String) = (809, GetStringFunc("tcPropertyIsStatic",",,,%s,,,") a0)
    /// Property '%s' cannot be set
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:669)
    static member tcPropertyCannotBeSet1(a0 : System.String) = (810, GetStringFunc("tcPropertyCannotBeSet1",",,,%s,,,") a0)
    /// Constructors must be applied to arguments and cannot be used as first-class values. If necessary use an anonymous function '(fun arg1 ... argN -> new Type(arg1,...,argN))'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:670)
    static member tcConstructorsCannotBeFirstClassValues() = (811, GetStringFunc("tcConstructorsCannotBeFirstClassValues",",,,") )
    /// The syntax 'expr.id' may only be used with record labels, properties and fields
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:671)
    static member tcSyntaxFormUsedOnlyWithRecordLabelsPropertiesAndFields() = (812, GetStringFunc("tcSyntaxFormUsedOnlyWithRecordLabelsPropertiesAndFields",",,,") )
    /// Event '%s' is static
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:672)
    static member tcEventIsStatic(a0 : System.String) = (813, GetStringFunc("tcEventIsStatic",",,,%s,,,") a0)
    /// Event '%s' is not static
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:673)
    static member tcEventIsNotStatic(a0 : System.String) = (814, GetStringFunc("tcEventIsNotStatic",",,,%s,,,") a0)
    /// The named argument '%s' did not match any argument or mutable property
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:674)
    static member tcNamedArgumentDidNotMatch(a0 : System.String) = (815, GetStringFunc("tcNamedArgumentDidNotMatch",",,,%s,,,") a0)
    /// One or more of the overloads of this method has curried arguments. Consider redesigning these members to take arguments in tupled form.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:675)
    static member tcOverloadsCannotHaveCurriedArguments() = (816, GetStringFunc("tcOverloadsCannotHaveCurriedArguments",",,,") )
    /// The unnamed arguments do not form a prefix of the arguments of the method called
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:676)
    static member tcUnnamedArgumentsDoNotFormPrefix() = (GetStringFunc("tcUnnamedArgumentsDoNotFormPrefix",",,,") )
    /// Static optimization conditionals are only for use within the F# library
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:677)
    static member tcStaticOptimizationConditionalsOnlyForFSharpLibrary() = (817, GetStringFunc("tcStaticOptimizationConditionalsOnlyForFSharpLibrary",",,,") )
    /// The corresponding formal argument is not optional
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:678)
    static member tcFormalArgumentIsNotOptional() = (818, GetStringFunc("tcFormalArgumentIsNotOptional",",,,") )
    /// Invalid optional assignment to a property or field
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:679)
    static member tcInvalidOptionalAssignmentToPropertyOrField() = (819, GetStringFunc("tcInvalidOptionalAssignmentToPropertyOrField",",,,") )
    /// A delegate constructor must be passed a single function value
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:680)
    static member tcDelegateConstructorMustBePassed() = (820, GetStringFunc("tcDelegateConstructorMustBePassed",",,,") )
    /// A binding cannot be marked both 'use' and 'rec'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:681)
    static member tcBindingCannotBeUseAndRec() = (821, GetStringFunc("tcBindingCannotBeUseAndRec",",,,") )
    /// The 'VolatileField' attribute may only be used on 'let' bindings in classes
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:682)
    static member tcVolatileOnlyOnClassLetBindings() = (823, GetStringFunc("tcVolatileOnlyOnClassLetBindings",",,,") )
    /// Attributes are not permitted on 'let' bindings in expressions
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:683)
    static member tcAttributesAreNotPermittedOnLetBindings() = (824, GetStringFunc("tcAttributesAreNotPermittedOnLetBindings",",,,") )
    /// The 'DefaultValue' attribute may only be used on 'val' declarations
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:684)
    static member tcDefaultValueAttributeRequiresVal() = (825, GetStringFunc("tcDefaultValueAttributeRequiresVal",",,,") )
    /// The 'ConditionalAttribute' attribute may only be used on members
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:685)
    static member tcConditionalAttributeRequiresMembers() = (826, GetStringFunc("tcConditionalAttributeRequiresMembers",",,,") )
    /// This is not a valid name for an active pattern
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:686)
    static member tcInvalidActivePatternName() = (827, GetStringFunc("tcInvalidActivePatternName",",,,") )
    /// The 'EntryPointAttribute' attribute may only be used on function definitions in modules
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:687)
    static member tcEntryPointAttributeRequiresFunctionInModule() = (828, GetStringFunc("tcEntryPointAttributeRequiresFunctionInModule",",,,") )
    /// Mutable values cannot be marked 'inline'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:688)
    static member tcMutableValuesCannotBeInline() = (829, GetStringFunc("tcMutableValuesCannotBeInline",",,,") )
    /// Mutable values cannot have generic parameters
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:689)
    static member tcMutableValuesMayNotHaveGenericParameters() = (830, GetStringFunc("tcMutableValuesMayNotHaveGenericParameters",",,,") )
    /// Mutable function values should be written 'let mutable f = (fun args -> ...)'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:690)
    static member tcMutableValuesSyntax() = (831, GetStringFunc("tcMutableValuesSyntax",",,,") )
    /// Only functions may be marked 'inline'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:691)
    static member tcOnlyFunctionsCanBeInline() = (832, GetStringFunc("tcOnlyFunctionsCanBeInline",",,,") )
    /// A literal value cannot be given the [<ThreadStatic>] or [<ContextStatic>] attributes
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:692)
    static member tcIllegalAttributesForLiteral() = (833, GetStringFunc("tcIllegalAttributesForLiteral",",,,") )
    /// A literal value cannot be marked 'mutable'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:693)
    static member tcLiteralCannotBeMutable() = (834, GetStringFunc("tcLiteralCannotBeMutable",",,,") )
    /// A literal value cannot be marked 'inline'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:694)
    static member tcLiteralCannotBeInline() = (835, GetStringFunc("tcLiteralCannotBeInline",",,,") )
    /// Literal values cannot have generic parameters
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:695)
    static member tcLiteralCannotHaveGenericParameters() = (836, GetStringFunc("tcLiteralCannotHaveGenericParameters",",,,") )
    /// This is not a valid constant expression
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:696)
    static member tcInvalidConstantExpression() = (837, GetStringFunc("tcInvalidConstantExpression",",,,") )
    /// This type is not accessible from this code location
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:697)
    static member tcTypeIsInaccessible() = (838, GetStringFunc("tcTypeIsInaccessible",",,,") )
    /// Unexpected condition in imported assembly: failed to decode AttributeUsage attribute
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:698)
    static member tcUnexpectedConditionInImportedAssembly() = (839, GetStringFunc("tcUnexpectedConditionInImportedAssembly",",,,") )
    /// Unrecognized attribute target. Valid attribute targets are 'assembly', 'module', 'type', 'method', 'property', 'return', 'param', 'field', 'event', 'constructor'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:699)
    static member tcUnrecognizedAttributeTarget() = (840, GetStringFunc("tcUnrecognizedAttributeTarget",",,,") )
    /// This attribute is not valid for use on this language element. Assembly attributes should be attached to a 'do ()' declaration, if necessary within an F# module.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:700)
    static member tcAttributeIsNotValidForLanguageElementUseDo() = (841, GetStringFunc("tcAttributeIsNotValidForLanguageElementUseDo",",,,") )
    /// This attribute is not valid for use on this language element
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:701)
    static member tcAttributeIsNotValidForLanguageElement() = (842, GetStringFunc("tcAttributeIsNotValidForLanguageElement",",,,") )
    /// Optional arguments cannot be used in custom attributes
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:702)
    static member tcOptionalArgumentsCannotBeUsedInCustomAttribute() = (843, GetStringFunc("tcOptionalArgumentsCannotBeUsedInCustomAttribute",",,,") )
    /// This property cannot be set
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:703)
    static member tcPropertyCannotBeSet0() = (844, GetStringFunc("tcPropertyCannotBeSet0",",,,") )
    /// This property or field was not found on this custom attribute type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:704)
    static member tcPropertyOrFieldNotFoundInAttribute() = (845, GetStringFunc("tcPropertyOrFieldNotFoundInAttribute",",,,") )
    /// A custom attribute must be a reference type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:705)
    static member tcCustomAttributeMustBeReferenceType() = (846, GetStringFunc("tcCustomAttributeMustBeReferenceType",",,,") )
    /// The number of args for a custom attribute does not match the expected number of args for the attribute constructor
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:706)
    static member tcCustomAttributeArgumentMismatch() = (847, GetStringFunc("tcCustomAttributeArgumentMismatch",",,,") )
    /// A custom attribute must invoke an object constructor
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:707)
    static member tcCustomAttributeMustInvokeConstructor() = (848, GetStringFunc("tcCustomAttributeMustInvokeConstructor",",,,") )
    /// Attribute expressions must be calls to object constructors
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:708)
    static member tcAttributeExpressionsMustBeConstructorCalls() = (849, GetStringFunc("tcAttributeExpressionsMustBeConstructorCalls",",,,") )
    /// This attribute cannot be used in this version of F#
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:709)
    static member tcUnsupportedAttribute() = (850, GetStringFunc("tcUnsupportedAttribute",",,,") )
    /// Invalid inline specification
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:710)
    static member tcInvalidInlineSpecification() = (851, GetStringFunc("tcInvalidInlineSpecification",",,,") )
    /// 'use' bindings must be of the form 'use <var> = <expr>'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:711)
    static member tcInvalidUseBinding() = (852, GetStringFunc("tcInvalidUseBinding",",,,") )
    /// Abstract members are not permitted in an augmentation - they must be defined as part of the type itself
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:712)
    static member tcAbstractMembersIllegalInAugmentation() = (853, GetStringFunc("tcAbstractMembersIllegalInAugmentation",",,,") )
    /// Method overrides and interface implementations are not permitted here
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:713)
    static member tcMethodOverridesIllegalHere() = (854, GetStringFunc("tcMethodOverridesIllegalHere",",,,") )
    /// No abstract or interface member was found that corresponds to this override
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:714)
    static member tcNoMemberFoundForOverride() = (855, GetStringFunc("tcNoMemberFoundForOverride",",,,") )
    /// This override takes a different number of arguments to the corresponding abstract member
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:715)
    static member tcOverrideArityMismatch() = (856, GetStringFunc("tcOverrideArityMismatch",",,,") )
    /// This method already has a default implementation
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:716)
    static member tcDefaultImplementationAlreadyExists() = (857, GetStringFunc("tcDefaultImplementationAlreadyExists",",,,") )
    /// The method implemented by this default is ambiguous
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:717)
    static member tcDefaultAmbiguous() = (858, GetStringFunc("tcDefaultAmbiguous",",,,") )
    /// No abstract property was found that corresponds to this override
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:718)
    static member tcNoPropertyFoundForOverride() = (859, GetStringFunc("tcNoPropertyFoundForOverride",",,,") )
    /// This property overrides or implements an abstract property but the abstract property doesn't have a corresponding %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:719)
    static member tcAbstractPropertyMissingGetOrSet(a0 : System.String) = (860, GetStringFunc("tcAbstractPropertyMissingGetOrSet",",,,%s,,,") a0)
    /// Invalid signature for set member
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:720)
    static member tcInvalidSignatureForSet() = (861, GetStringFunc("tcInvalidSignatureForSet",",,,") )
    /// This property already has a default implementation
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:721)
    static member tcPropertyAlreadyHasDefaultImplementation() = (862, GetStringFunc("tcPropertyAlreadyHasDefaultImplementation",",,,") )
    /// The property implemented by this default is ambiguous
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:722)
    static member tcPropertyImplementedIsAmbiguous() = (863, GetStringFunc("tcPropertyImplementedIsAmbiguous",",,,") )
    /// This new member hides the abstract member '%s'. Rename the member or use 'override' instead.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:723)
    static member tcNewMemberHidesAbstractMember(a0 : System.String) = (864, GetStringFunc("tcNewMemberHidesAbstractMember",",,,%s,,,") a0)
    /// This new member hides the abstract member '%s' once tuples, functions, units of measure and/or provided types are erased. Rename the member or use 'override' instead.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:724)
    static member tcNewMemberHidesAbstractMemberWithSuffix(a0 : System.String) = (864, GetStringFunc("tcNewMemberHidesAbstractMemberWithSuffix",",,,%s,,,") a0)
    /// Interfaces cannot contain definitions of static initializers
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:725)
    static member tcStaticInitializersIllegalInInterface() = (865, GetStringFunc("tcStaticInitializersIllegalInInterface",",,,") )
    /// Interfaces cannot contain definitions of object constructors
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:726)
    static member tcObjectConstructorsIllegalInInterface() = (866, GetStringFunc("tcObjectConstructorsIllegalInInterface",",,,") )
    /// Interfaces cannot contain definitions of member overrides
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:727)
    static member tcMemberOverridesIllegalInInterface() = (867, GetStringFunc("tcMemberOverridesIllegalInInterface",",,,") )
    /// Interfaces cannot contain definitions of concrete members. You may need to define a constructor on your type to indicate that the type is a class.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:728)
    static member tcConcreteMembersIllegalInInterface() = (868, GetStringFunc("tcConcreteMembersIllegalInInterface",",,,") )
    /// Constructors cannot be specified in exception augmentations
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:729)
    static member tcConstructorsDisallowedInExceptionAugmentation() = (869, GetStringFunc("tcConstructorsDisallowedInExceptionAugmentation",",,,") )
    /// Structs cannot have an object constructor with no arguments. This is a restriction imposed on all CLI languages as structs automatically support a default constructor.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:730)
    static member tcStructsCannotHaveConstructorWithNoArguments() = (870, GetStringFunc("tcStructsCannotHaveConstructorWithNoArguments",",,,") )
    /// Constructors cannot be defined for this type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:731)
    static member tcConstructorsIllegalForThisType() = (871, GetStringFunc("tcConstructorsIllegalForThisType",",,,") )
    /// Recursive bindings that include member specifications can only occur as a direct augmentation of a type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:732)
    static member tcRecursiveBindingsWithMembersMustBeDirectAugmentation() = (872, GetStringFunc("tcRecursiveBindingsWithMembersMustBeDirectAugmentation",",,,") )
    /// Only simple variable patterns can be bound in 'let rec' constructs
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:733)
    static member tcOnlySimplePatternsInLetRec() = (873, GetStringFunc("tcOnlySimplePatternsInLetRec",",,,") )
    /// Only record fields and simple 'let' bindings may be marked mutable
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:734)
    static member tcOnlyRecordFieldsAndSimpleLetCanBeMutable() = (874, GetStringFunc("tcOnlyRecordFieldsAndSimpleLetCanBeMutable",",,,") )
    /// This member is not sufficiently generic
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:735)
    static member tcMemberIsNotSufficientlyGeneric() = (875, GetStringFunc("tcMemberIsNotSufficientlyGeneric",",,,") )
    /// A declaration may only be the [<Literal>] attribute if a constant value is also given, e.g. 'val x : int = 1'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:736)
    static member tcLiteralAttributeRequiresConstantValue() = (876, GetStringFunc("tcLiteralAttributeRequiresConstantValue",",,,") )
    /// A declaration may only be given a value in a signature if the declaration has the [<Literal>] attribute
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:737)
    static member tcValueInSignatureRequiresLiteralAttribute() = (877, GetStringFunc("tcValueInSignatureRequiresLiteralAttribute",",,,") )
    /// Thread-static and context-static variables must be static and given the [<DefaultValue>] attribute to indicate that the value is initialized to the default value on each new thread
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:738)
    static member tcThreadStaticAndContextStaticMustBeStatic() = (878, GetStringFunc("tcThreadStaticAndContextStaticMustBeStatic",",,,") )
    /// Volatile fields must be marked 'mutable' and cannot be thread-static
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:739)
    static member tcVolatileFieldsMustBeMutable() = (879, GetStringFunc("tcVolatileFieldsMustBeMutable",",,,") )
    /// Uninitialized 'val' fields must be mutable and marked with the '[<DefaultValue>]' attribute. Consider using a 'let' binding instead of a 'val' field.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:740)
    static member tcUninitializedValFieldsMustBeMutable() = (880, GetStringFunc("tcUninitializedValFieldsMustBeMutable",",,,") )
    /// Static 'val' fields in types must be mutable, private and marked with the '[<DefaultValue>]' attribute. They are initialized to the 'null' or 'zero' value for their type. Consider also using a 'static let mutable' binding in a class type.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:741)
    static member tcStaticValFieldsMustBeMutableAndPrivate() = (881, GetStringFunc("tcStaticValFieldsMustBeMutableAndPrivate",",,,") )
    /// This field requires a name
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:742)
    static member tcFieldRequiresName() = (882, GetStringFunc("tcFieldRequiresName",",,,") )
    /// Invalid namespace, module, type or union case name
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:743)
    static member tcInvalidNamespaceModuleTypeUnionName() = (883, GetStringFunc("tcInvalidNamespaceModuleTypeUnionName",",,,") )
    /// Explicit type declarations for constructors must be of the form 'ty1 * ... * tyN -> resTy'. Parentheses may be required around 'resTy'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:744)
    static member tcIllegalFormForExplicitTypeDeclaration() = (884, GetStringFunc("tcIllegalFormForExplicitTypeDeclaration",",,,") )
    /// Return types of union cases must be identical to the type being defined, up to abbreviations
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:745)
    static member tcReturnTypesForUnionMustBeSameAsType() = (885, GetStringFunc("tcReturnTypesForUnionMustBeSameAsType",",,,") )
    /// This is not a valid value for an enumeration literal
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:746)
    static member tcInvalidEnumerationLiteral() = (886, GetStringFunc("tcInvalidEnumerationLiteral",",,,") )
    /// The type '%s' is not an interface type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:747)
    static member tcTypeIsNotInterfaceType1(a0 : System.String) = (887, GetStringFunc("tcTypeIsNotInterfaceType1",",,,%s,,,") a0)
    /// Duplicate specification of an interface
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:748)
    static member tcDuplicateSpecOfInterface() = (888, GetStringFunc("tcDuplicateSpecOfInterface",",,,") )
    /// A field/val declaration is not permitted here
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:749)
    static member tcFieldValIllegalHere() = (889, GetStringFunc("tcFieldValIllegalHere",",,,") )
    /// A inheritance declaration is not permitted here
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:750)
    static member tcInheritIllegalHere() = (890, GetStringFunc("tcInheritIllegalHere",",,,") )
    /// This declaration opens the module '%s', which is marked as 'RequireQualifiedAccess'. Adjust your code to use qualified references to the elements of the module instead, e.g. 'List.map' instead of 'map'. This change will ensure that your code is robust as new constructs are added to libraries.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:751)
    static member tcModuleRequiresQualifiedAccess(a0 : System.String) = (892, GetStringFunc("tcModuleRequiresQualifiedAccess",",,,%s,,,") a0)
    /// This declaration opens the namespace or module '%s' through a partially qualified path. Adjust this code to use the full path of the namespace. This change will make your code more robust as new constructs are added to the F# and CLI libraries.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:752)
    static member tcOpenUsedWithPartiallyQualifiedPath(a0 : System.String) = (893, GetStringFunc("tcOpenUsedWithPartiallyQualifiedPath",",,,%s,,,") a0)
    /// Local class bindings cannot be marked inline. Consider lifting the definition out of the class or else do not mark it as inline.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:753)
    static member tcLocalClassBindingsCannotBeInline() = (894, GetStringFunc("tcLocalClassBindingsCannotBeInline",",,,") )
    /// Type abbreviations cannot have members
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:754)
    static member tcTypeAbbreviationsMayNotHaveMembers() = (895, GetStringFunc("tcTypeAbbreviationsMayNotHaveMembers",",,,") )
    /// Enumerations cannot have members
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:755)
    static member tcEnumerationsMayNotHaveMembers() = (896, GetStringFunc("tcEnumerationsMayNotHaveMembers",",,,") )
    /// Measure declarations may have only static members
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:756)
    static member tcMeasureDeclarationsRequireStaticMembers() = (897, GetStringFunc("tcMeasureDeclarationsRequireStaticMembers",",,,") )
    /// Structs cannot contain 'do' bindings because the default constructor for structs would not execute these bindings
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:757)
    static member tcStructsMayNotContainDoBindings() = (GetStringFunc("tcStructsMayNotContainDoBindings",",,,") )
    /// Structs cannot contain value definitions because the default constructor for structs will not execute these bindings. Consider adding additional arguments to the primary constructor for the type.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:758)
    static member tcStructsMayNotContainLetBindings() = (901, GetStringFunc("tcStructsMayNotContainLetBindings",",,,") )
    /// Static value definitions may only be used in types with a primary constructor. Consider adding arguments to the type definition, e.g. 'type X(args) = ...'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:759)
    static member tcStaticLetBindingsRequireClassesWithImplicitConstructors() = (902, GetStringFunc("tcStaticLetBindingsRequireClassesWithImplicitConstructors",",,,") )
    /// Measure declarations may have only static members: constructors are not available
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:760)
    static member tcMeasureDeclarationsRequireStaticMembersNotConstructors() = (904, GetStringFunc("tcMeasureDeclarationsRequireStaticMembersNotConstructors",",,,") )
    /// A member and a local class binding both have the name '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:761)
    static member tcMemberAndLocalClassBindingHaveSameName(a0 : System.String) = (905, GetStringFunc("tcMemberAndLocalClassBindingHaveSameName",",,,%s,,,") a0)
    /// Type abbreviations cannot have interface declarations
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:762)
    static member tcTypeAbbreviationsCannotHaveInterfaceDeclaration() = (906, GetStringFunc("tcTypeAbbreviationsCannotHaveInterfaceDeclaration",",,,") )
    /// Enumerations cannot have interface declarations
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:763)
    static member tcEnumerationsCannotHaveInterfaceDeclaration() = (907, GetStringFunc("tcEnumerationsCannotHaveInterfaceDeclaration",",,,") )
    /// This type is not an interface type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:764)
    static member tcTypeIsNotInterfaceType0() = (908, GetStringFunc("tcTypeIsNotInterfaceType0",",,,") )
    /// All implemented interfaces should be declared on the initial declaration of the type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:765)
    static member tcAllImplementedInterfacesShouldBeDeclared() = (909, GetStringFunc("tcAllImplementedInterfacesShouldBeDeclared",",,,") )
    /// A default implementation of this interface has already been added because the explicit implementation of the interface was not specified at the definition of the type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:766)
    static member tcDefaultImplementationForInterfaceHasAlreadyBeenAdded() = (910, GetStringFunc("tcDefaultImplementationForInterfaceHasAlreadyBeenAdded",",,,") )
    /// This member is not permitted in an interface implementation
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:767)
    static member tcMemberNotPermittedInInterfaceImplementation() = (911, GetStringFunc("tcMemberNotPermittedInInterfaceImplementation",",,,") )
    /// This declaration element is not permitted in an augmentation
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:768)
    static member tcDeclarationElementNotPermittedInAugmentation() = (912, GetStringFunc("tcDeclarationElementNotPermittedInAugmentation",",,,") )
    /// Types cannot contain nested type definitions
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:769)
    static member tcTypesCannotContainNestedTypes() = (913, GetStringFunc("tcTypesCannotContainNestedTypes",",,,") )
    /// type, exception or module
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:770)
    static member tcTypeExceptionOrModule() = (GetStringFunc("tcTypeExceptionOrModule",",,,") )
    /// type or module
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:771)
    static member tcTypeOrModule() = (GetStringFunc("tcTypeOrModule",",,,") )
    /// The struct, record or union type '%s' implements the interface 'System.IStructuralEquatable' explicitly. Apply the 'CustomEquality' attribute to the type.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:772)
    static member tcImplementsIStructuralEquatableExplicitly(a0 : System.String) = (914, GetStringFunc("tcImplementsIStructuralEquatableExplicitly",",,,%s,,,") a0)
    /// The struct, record or union type '%s' implements the interface 'System.IEquatable<_>' explicitly. Apply the 'CustomEquality' attribute to the type and provide a consistent implementation of the non-generic override 'System.Object.Equals(obj)'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:773)
    static member tcImplementsIEquatableExplicitly(a0 : System.String) = (915, GetStringFunc("tcImplementsIEquatableExplicitly",",,,%s,,,") a0)
    /// Explicit type specifications cannot be used for exception constructors
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:774)
    static member tcExplicitTypeSpecificationCannotBeUsedForExceptionConstructors() = (916, GetStringFunc("tcExplicitTypeSpecificationCannotBeUsedForExceptionConstructors",",,,") )
    /// Exception abbreviations should not have argument lists
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:775)
    static member tcExceptionAbbreviationsShouldNotHaveArgumentList() = (917, GetStringFunc("tcExceptionAbbreviationsShouldNotHaveArgumentList",",,,") )
    /// Abbreviations for Common IL exceptions cannot take arguments
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:776)
    static member tcAbbreviationsFordotNetExceptionsCannotTakeArguments() = (918, GetStringFunc("tcAbbreviationsFordotNetExceptionsCannotTakeArguments",",,,") )
    /// Exception abbreviations must refer to existing exceptions or F# types deriving from System.Exception
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:777)
    static member tcExceptionAbbreviationsMustReferToValidExceptions() = (919, GetStringFunc("tcExceptionAbbreviationsMustReferToValidExceptions",",,,") )
    /// Abbreviations for Common IL exception types must have a matching object constructor
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:778)
    static member tcAbbreviationsFordotNetExceptionsMustHaveMatchingObjectConstructor() = (920, GetStringFunc("tcAbbreviationsFordotNetExceptionsMustHaveMatchingObjectConstructor",",,,") )
    /// Not an exception
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:779)
    static member tcNotAnException() = (921, GetStringFunc("tcNotAnException",",,,") )
    /// Invalid module name
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:781)
    static member tcInvalidModuleName() = (924, GetStringFunc("tcInvalidModuleName",",,,") )
    /// Invalid type extension
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:782)
    static member tcInvalidTypeExtension() = (925, GetStringFunc("tcInvalidTypeExtension",",,,") )
    /// The attributes of this type specify multiple kinds for the type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:783)
    static member tcAttributesOfTypeSpecifyMultipleKindsForType() = (926, GetStringFunc("tcAttributesOfTypeSpecifyMultipleKindsForType",",,,") )
    /// The kind of the type specified by its attributes does not match the kind implied by its definition
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:784)
    static member tcKindOfTypeSpecifiedDoesNotMatchDefinition() = (927, GetStringFunc("tcKindOfTypeSpecifiedDoesNotMatchDefinition",",,,") )
    /// Measure definitions cannot have type parameters
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:785)
    static member tcMeasureDefinitionsCannotHaveTypeParameters() = (928, GetStringFunc("tcMeasureDefinitionsCannotHaveTypeParameters",",,,") )
    /// This type requires a definition
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:786)
    static member tcTypeRequiresDefinition() = (929, GetStringFunc("tcTypeRequiresDefinition",",,,") )
    /// This type abbreviation has one or more declared type parameters that do not appear in the type being abbreviated. Type abbreviations must use all declared type parameters in the type being abbreviated. Consider removing one or more type parameters, or use a concrete type definition that wraps an underlying type, such as 'type C<'a> = C of ...'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:787)
    static member tcTypeAbbreviationHasTypeParametersMissingOnType() = (GetStringFunc("tcTypeAbbreviationHasTypeParametersMissingOnType",",,,") )
    /// Structs, interfaces, enums and delegates cannot inherit from other types
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:788)
    static member tcStructsInterfacesEnumsDelegatesMayNotInheritFromOtherTypes() = (931, GetStringFunc("tcStructsInterfacesEnumsDelegatesMayNotInheritFromOtherTypes",",,,") )
    /// Types cannot inherit from multiple concrete types
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:789)
    static member tcTypesCannotInheritFromMultipleConcreteTypes() = (932, GetStringFunc("tcTypesCannotInheritFromMultipleConcreteTypes",",,,") )
    /// Records, union, abbreviations and struct types cannot have the 'AllowNullLiteral' attribute
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:790)
    static member tcRecordsUnionsAbbreviationsStructsMayNotHaveAllowNullLiteralAttribute() = (934, GetStringFunc("tcRecordsUnionsAbbreviationsStructsMayNotHaveAllowNullLiteralAttribute",",,,") )
    /// Types with the 'AllowNullLiteral' attribute may only inherit from or implement types which also allow the use of the null literal
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:791)
    static member tcAllowNullTypesMayOnlyInheritFromAllowNullTypes() = (935, GetStringFunc("tcAllowNullTypesMayOnlyInheritFromAllowNullTypes",",,,") )
    /// Generic types cannot be given the 'StructLayout' attribute
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:792)
    static member tcGenericTypesCannotHaveStructLayout() = (936, GetStringFunc("tcGenericTypesCannotHaveStructLayout",",,,") )
    /// Only structs and classes without primary constructors may be given the 'StructLayout' attribute
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:793)
    static member tcOnlyStructsCanHaveStructLayout() = (937, GetStringFunc("tcOnlyStructsCanHaveStructLayout",",,,") )
    /// The representation of this type is hidden by the signature. It must be given an attribute such as [<Sealed>], [<Class>] or [<Interface>] to indicate the characteristics of the type.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:794)
    static member tcRepresentationOfTypeHiddenBySignature() = (938, GetStringFunc("tcRepresentationOfTypeHiddenBySignature",",,,") )
    /// Only classes may be given the 'AbstractClass' attribute
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:795)
    static member tcOnlyClassesCanHaveAbstract() = (939, GetStringFunc("tcOnlyClassesCanHaveAbstract",",,,") )
    /// Only types representing units-of-measure may be given the 'Measure' attribute
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:796)
    static member tcOnlyTypesRepresentingUnitsOfMeasureCanHaveMeasure() = (940, GetStringFunc("tcOnlyTypesRepresentingUnitsOfMeasureCanHaveMeasure",",,,") )
    /// Accessibility modifiers are not permitted on overrides or interface implementations
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:797)
    static member tcOverridesCannotHaveVisibilityDeclarations() = (941, GetStringFunc("tcOverridesCannotHaveVisibilityDeclarations",",,,") )
    /// Discriminated union types are always sealed
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:798)
    static member tcTypesAreAlwaysSealedDU() = (942, GetStringFunc("tcTypesAreAlwaysSealedDU",",,,") )
    /// Record types are always sealed
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:799)
    static member tcTypesAreAlwaysSealedRecord() = (942, GetStringFunc("tcTypesAreAlwaysSealedRecord",",,,") )
    /// Assembly code types are always sealed
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:800)
    static member tcTypesAreAlwaysSealedAssemblyCode() = (942, GetStringFunc("tcTypesAreAlwaysSealedAssemblyCode",",,,") )
    /// Struct types are always sealed
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:801)
    static member tcTypesAreAlwaysSealedStruct() = (942, GetStringFunc("tcTypesAreAlwaysSealedStruct",",,,") )
    /// Delegate types are always sealed
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:802)
    static member tcTypesAreAlwaysSealedDelegate() = (942, GetStringFunc("tcTypesAreAlwaysSealedDelegate",",,,") )
    /// Enum types are always sealed
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:803)
    static member tcTypesAreAlwaysSealedEnum() = (942, GetStringFunc("tcTypesAreAlwaysSealedEnum",",,,") )
    /// Interface types and delegate types cannot contain fields
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:804)
    static member tcInterfaceTypesAndDelegatesCannotContainFields() = (943, GetStringFunc("tcInterfaceTypesAndDelegatesCannotContainFields",",,,") )
    /// Abbreviated types cannot be given the 'Sealed' attribute
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:805)
    static member tcAbbreviatedTypesCannotBeSealed() = (944, GetStringFunc("tcAbbreviatedTypesCannotBeSealed",",,,") )
    /// Cannot inherit a sealed type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:806)
    static member tcCannotInheritFromSealedType() = (945, GetStringFunc("tcCannotInheritFromSealedType",",,,") )
    /// Cannot inherit from interface type. Use interface ... with instead.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:807)
    static member tcCannotInheritFromInterfaceType() = (946, GetStringFunc("tcCannotInheritFromInterfaceType",",,,") )
    /// Struct types cannot contain abstract members
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:808)
    static member tcStructTypesCannotContainAbstractMembers() = (947, GetStringFunc("tcStructTypesCannotContainAbstractMembers",",,,") )
    /// Interface types cannot be sealed
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:809)
    static member tcInterfaceTypesCannotBeSealed() = (948, GetStringFunc("tcInterfaceTypesCannotBeSealed",",,,") )
    /// Delegate specifications must be of the form 'typ -> typ'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:810)
    static member tcInvalidDelegateSpecification() = (949, GetStringFunc("tcInvalidDelegateSpecification",",,,") )
    /// Delegate specifications must not be curried types. Use 'typ * ... * typ -> typ' for multi-argument delegates, and 'typ -> (typ -> typ)' for delegates returning function values.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:811)
    static member tcDelegatesCannotBeCurried() = (950, GetStringFunc("tcDelegatesCannotBeCurried",",,,") )
    /// Literal enumerations must have type int, uint, int16, uint16, int64, uint64, byte, sbyte or char
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:812)
    static member tcInvalidTypeForLiteralEnumeration() = (951, GetStringFunc("tcInvalidTypeForLiteralEnumeration",",,,") )
    /// This type definition involves an immediate cyclic reference through an abbreviation
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:814)
    static member tcTypeDefinitionIsCyclic() = (953, GetStringFunc("tcTypeDefinitionIsCyclic",",,,") )
    /// This type definition involves an immediate cyclic reference through a struct field or inheritance relation
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:815)
    static member tcTypeDefinitionIsCyclicThroughInheritance() = (954, GetStringFunc("tcTypeDefinitionIsCyclicThroughInheritance",",,,") )
    /// The syntax 'type X with ...' is reserved for augmentations. Types whose representations are hidden but which have members are now declared in signatures using 'type X = ...'. You may also need to add the '[<Sealed>] attribute to the type definition in the signature
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:816)
    static member tcReservedSyntaxForAugmentation() = (GetStringFunc("tcReservedSyntaxForAugmentation",",,,") )
    /// Members that extend interface, delegate or enum types must be placed in a module separate to the definition of the type. This module must either have the AutoOpen attribute or be opened explicitly by client code to bring the extension members into scope.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:817)
    static member tcMembersThatExtendInterfaceMustBePlacedInSeparateModule() = (956, GetStringFunc("tcMembersThatExtendInterfaceMustBePlacedInSeparateModule",",,,") )
    /// The declared type parameters for this type extension do not match the declared type parameters on the original type '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:818)
    static member tcDeclaredTypeParametersForExtensionDoNotMatchOriginal(a0 : System.String) = (957, GetStringFunc("tcDeclaredTypeParametersForExtensionDoNotMatchOriginal",",,,%s,,,") a0)
    /// Type definitions may only have one 'inherit' specification and it must be the first declaration
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:819)
    static member tcTypeDefinitionsWithImplicitConstructionMustHaveOneInherit() = (959, GetStringFunc("tcTypeDefinitionsWithImplicitConstructionMustHaveOneInherit",",,,") )
    /// 'let' and 'do' bindings must come before member and interface definitions in type definitions
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:820)
    static member tcTypeDefinitionsWithImplicitConstructionMustHaveLocalBindingsBeforeMembers() = (960, GetStringFunc("tcTypeDefinitionsWithImplicitConstructionMustHaveLocalBindingsBeforeMembers",",,,") )
    /// This 'inherit' declaration specifies the inherited type but no arguments. Consider supplying arguments, e.g. 'inherit BaseType(args)'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:821)
    static member tcInheritDeclarationMissingArguments() = (961, GetStringFunc("tcInheritDeclarationMissingArguments",",,,") )
    /// This 'inherit' declaration has arguments, but is not in a type with a primary constructor. Consider adding arguments to your type definition, e.g. 'type X(args) = ...'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:822)
    static member tcInheritConstructionCallNotPartOfImplicitSequence() = (962, GetStringFunc("tcInheritConstructionCallNotPartOfImplicitSequence",",,,") )
    /// This definition may only be used in a type with a primary constructor. Consider adding arguments to your type definition, e.g. 'type X(args) = ...'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:823)
    static member tcLetAndDoRequiresImplicitConstructionSequence() = (963, GetStringFunc("tcLetAndDoRequiresImplicitConstructionSequence",",,,") )
    /// Type abbreviations cannot have augmentations
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:824)
    static member tcTypeAbbreviationsCannotHaveAugmentations() = (964, GetStringFunc("tcTypeAbbreviationsCannotHaveAugmentations",",,,") )
    /// The path '%s' is a namespace. A module abbreviation may not abbreviate a namespace.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:825)
    static member tcModuleAbbreviationForNamespace(a0 : System.String) = (965, GetStringFunc("tcModuleAbbreviationForNamespace",",,,%s,,,") a0)
    /// The type '%s' is used in an invalid way. A value prior to '%s' has an inferred type involving '%s', which is an invalid forward reference.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:826)
    static member tcTypeUsedInInvalidWay(a0 : System.String, a1 : System.String, a2 : System.String) = (966, GetStringFunc("tcTypeUsedInInvalidWay",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// The member '%s' is used in an invalid way. A use of '%s' has been inferred prior to the definition of '%s', which is an invalid forward reference.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:827)
    static member tcMemberUsedInInvalidWay(a0 : System.String, a1 : System.String, a2 : System.String) = (967, GetStringFunc("tcMemberUsedInInvalidWay",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// The attribute 'AutoOpen(\"%s\")' in the assembly '%s' did not refer to a valid module or namespace in that assembly and has been ignored
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:830)
    static member tcAttributeAutoOpenWasIgnored(a0 : System.String, a1 : System.String) = (970, GetStringFunc("tcAttributeAutoOpenWasIgnored",",,,%s,,,%s,,,") a0 a1)
    /// Undefined value '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:834)
    static member ilUndefinedValue(a0 : System.String) = (971, GetStringFunc("ilUndefinedValue",",,,%s,,,") a0)
    /// Label %s not found
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:835)
    static member ilLabelNotFound(a0 : System.String) = (972, GetStringFunc("ilLabelNotFound",",,,%s,,,") a0)
    /// Incorrect number of type arguments to local call
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:836)
    static member ilIncorrectNumberOfTypeArguments() = (973, GetStringFunc("ilIncorrectNumberOfTypeArguments",",,,") )
    /// Dynamic invocation of %s is not supported
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:837)
    static member ilDynamicInvocationNotSupported(a0 : System.String) = (GetStringFunc("ilDynamicInvocationNotSupported",",,,%s,,,") a0)
    /// Taking the address of a literal field is invalid
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:838)
    static member ilAddressOfLiteralFieldIsInvalid() = (975, GetStringFunc("ilAddressOfLiteralFieldIsInvalid",",,,") )
    /// This operation involves taking the address of a value '%s' represented using a local variable or other special representation. This is invalid.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:839)
    static member ilAddressOfValueHereIsInvalid(a0 : System.String) = (976, GetStringFunc("ilAddressOfValueHereIsInvalid",",,,%s,,,") a0)
    /// Values marked with 'LiteralAttribute' cannot be mutable
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:840)
    static member ilValuesWithLiteralAttributeCannotBeMutable() = (978, GetStringFunc("ilValuesWithLiteralAttributeCannotBeMutable",",,,") )
    /// Values marked with 'LiteralAttribute' must currently be simple integer, character, Boolean, string or floating point constants
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:841)
    static member ilValuesWithLiteralAttributeMustBeSimple() = (979, GetStringFunc("ilValuesWithLiteralAttributeMustBeSimple",",,,") )
    /// Custom marshallers cannot be specified in F# code. Consider using a C# helper function.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:842)
    static member ilCustomMarshallersCannotBeUsedInFSharp() = (980, GetStringFunc("ilCustomMarshallersCannotBeUsedInFSharp",",,,") )
    /// The MarshalAs attribute could not be decoded
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:843)
    static member ilMarshalAsAttributeCannotBeDecoded() = (981, GetStringFunc("ilMarshalAsAttributeCannotBeDecoded",",,,") )
    /// The signature for this external function contains type parameters. Constrain the argument and return types to indicate the types of the corresponding C function.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:844)
    static member ilSignatureForExternalFunctionContainsTypeParameters() = (982, GetStringFunc("ilSignatureForExternalFunctionContainsTypeParameters",",,,") )
    /// The DllImport attribute could not be decoded
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:845)
    static member ilDllImportAttributeCouldNotBeDecoded() = (983, GetStringFunc("ilDllImportAttributeCouldNotBeDecoded",",,,") )
    /// Literal fields cannot be set
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:846)
    static member ilLiteralFieldsCannotBeSet() = (984, GetStringFunc("ilLiteralFieldsCannotBeSet",",,,") )
    /// GenSetStorage: %s was represented as a static method but was not an appropriate lambda expression
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:847)
    static member ilStaticMethodIsNotLambda(a0 : System.String) = (985, GetStringFunc("ilStaticMethodIsNotLambda",",,,%s,,,") a0)
    /// Mutable variables cannot escape their method
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:848)
    static member ilMutableVariablesCannotEscapeMethod() = (986, GetStringFunc("ilMutableVariablesCannotEscapeMethod",",,,") )
    /// Compiler error: unexpected unrealized value
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:849)
    static member ilUnexpectedUnrealizedValue() = (987, GetStringFunc("ilUnexpectedUnrealizedValue",",,,") )
    /// Main module of program is empty: nothing will happen when it is run
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:850)
    static member ilMainModuleEmpty() = (988, GetStringFunc("ilMainModuleEmpty",",,,") )
    /// This type cannot be used for a literal field
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:851)
    static member ilTypeCannotBeUsedForLiteralField() = (989, GetStringFunc("ilTypeCannotBeUsedForLiteralField",",,,") )
    /// Unexpected GetSet annotation on a property
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:852)
    static member ilUnexpectedGetSetAnnotation() = (990, GetStringFunc("ilUnexpectedGetSetAnnotation",",,,") )
    /// The FieldOffset attribute could not be decoded
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:853)
    static member ilFieldOffsetAttributeCouldNotBeDecoded() = (991, GetStringFunc("ilFieldOffsetAttributeCouldNotBeDecoded",",,,") )
    /// The StructLayout attribute could not be decoded
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:854)
    static member ilStructLayoutAttributeCouldNotBeDecoded() = (992, GetStringFunc("ilStructLayoutAttributeCouldNotBeDecoded",",,,") )
    /// The DefaultAugmentation attribute could not be decoded
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:855)
    static member ilDefaultAugmentationAttributeCouldNotBeDecoded() = (993, GetStringFunc("ilDefaultAugmentationAttributeCouldNotBeDecoded",",,,") )
    /// Reflected definitions cannot contain uses of the prefix splice operator '%%'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:856)
    static member ilReflectedDefinitionsCannotUseSliceOperator() = (994, GetStringFunc("ilReflectedDefinitionsCannotUseSliceOperator",",,,") )
    /// Problem with codepage '%d': %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:860)
    static member optsProblemWithCodepage(a0 : System.Int32, a1 : System.String) = (1000, GetStringFunc("optsProblemWithCodepage",",,,%d,,,%s,,,") a0 a1)
    /// Freely distributed under the Apache 2.0 Open Source License
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:861)
    static member optsCopyright() = (GetStringFunc("optsCopyright",",,,") )
    /// Name of the output file (Short form: -o)
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:862)
    static member optsNameOfOutputFile() = (GetStringFunc("optsNameOfOutputFile",",,,") )
    /// Build a console executable
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:863)
    static member optsBuildConsole() = (GetStringFunc("optsBuildConsole",",,,") )
    /// Build a Windows executable
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:864)
    static member optsBuildWindows() = (GetStringFunc("optsBuildWindows",",,,") )
    /// Build a library (Short form: -a)
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:865)
    static member optsBuildLibrary() = (GetStringFunc("optsBuildLibrary",",,,") )
    /// Build a module that can be added to another assembly
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:866)
    static member optsBuildModule() = (GetStringFunc("optsBuildModule",",,,") )
    /// Delay-sign the assembly using only the public portion of the strong name key
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:867)
    static member optsDelaySign() = (GetStringFunc("optsDelaySign",",,,") )
    /// Write the xmldoc of the assembly to the given file
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:868)
    static member optsWriteXml() = (GetStringFunc("optsWriteXml",",,,") )
    /// Specify a strong name key file
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:869)
    static member optsStrongKeyFile() = (GetStringFunc("optsStrongKeyFile",",,,") )
    /// Specify a strong name key container
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:870)
    static member optsStrongKeyContainer() = (GetStringFunc("optsStrongKeyContainer",",,,") )
    /// Limit which platforms this code can run on: x86, Itanium, x64, anycpu32bitpreferred, or anycpu. The default is anycpu.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:871)
    static member optsPlatform() = (GetStringFunc("optsPlatform",",,,") )
    /// Only include optimization information essential for implementing inlined constructs. Inhibits cross-module inlining but improves binary compatibility.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:872)
    static member optsNoOpt() = (GetStringFunc("optsNoOpt",",,,") )
    /// Don't add a resource to the generated assembly containing F#-specific metadata
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:873)
    static member optsNoInterface() = (GetStringFunc("optsNoInterface",",,,") )
    /// Print the inferred interface of the assembly to a file
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:874)
    static member optsSig() = (GetStringFunc("optsSig",",,,") )
    /// Reference an assembly (Short form: -r)
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:875)
    static member optsReference() = (GetStringFunc("optsReference",",,,") )
    /// Specify a Win32 resource file (.res)
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:876)
    static member optsWin32res() = (GetStringFunc("optsWin32res",",,,") )
    /// Specify a Win32 manifest file
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:877)
    static member optsWin32manifest() = (GetStringFunc("optsWin32manifest",",,,") )
    /// Do not include the default Win32 manifest
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:878)
    static member optsNowin32manifest() = (GetStringFunc("optsNowin32manifest",",,,") )
    /// Embed the specified managed resource
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:879)
    static member optsResource() = (GetStringFunc("optsResource",",,,") )
    /// Link the specified resource to this assembly where the resinfo format is <file>[,<string name>[,public|private]]
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:880)
    static member optsLinkresource() = (GetStringFunc("optsLinkresource",",,,") )
    /// Emit debug information (Short form: -g)
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:881)
    static member optsDebugPM() = (GetStringFunc("optsDebugPM",",,,") )
    /// Specify debugging type: full, pdbonly. ('full' is the default and enables attaching a debugger to a running program).
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:882)
    static member optsDebug() = (GetStringFunc("optsDebug",",,,") )
    /// Enable optimizations (Short form: -O)
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:883)
    static member optsOptimize() = (GetStringFunc("optsOptimize",",,,") )
    /// Enable or disable tailcalls
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:884)
    static member optsTailcalls() = (GetStringFunc("optsTailcalls",",,,") )
    /// Enable or disable cross-module optimizations
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:885)
    static member optsCrossoptimize() = (GetStringFunc("optsCrossoptimize",",,,") )
    /// Report all warnings as errors
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:886)
    static member optsWarnaserrorPM() = (GetStringFunc("optsWarnaserrorPM",",,,") )
    /// Report specific warnings as errors
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:887)
    static member optsWarnaserror() = (GetStringFunc("optsWarnaserror",",,,") )
    /// Set a warning level (0-5)
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:888)
    static member optsWarn() = (GetStringFunc("optsWarn",",,,") )
    /// Disable specific warning messages
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:889)
    static member optsNowarn() = (GetStringFunc("optsNowarn",",,,") )
    /// Enable specific warnings that may be off by default
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:890)
    static member optsWarnOn() = (GetStringFunc("optsWarnOn",",,,") )
    /// Generate overflow checks
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:891)
    static member optsChecked() = (GetStringFunc("optsChecked",",,,") )
    /// Define conditional compilation symbols (Short form: -d)
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:892)
    static member optsDefine() = (GetStringFunc("optsDefine",",,,") )
    /// Ignore ML compatibility warnings
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:893)
    static member optsMlcompatibility() = (GetStringFunc("optsMlcompatibility",",,,") )
    /// Suppress compiler copyright message
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:894)
    static member optsNologo() = (GetStringFunc("optsNologo",",,,") )
    /// Display this usage message (Short form: -?)
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:895)
    static member optsHelp() = (GetStringFunc("optsHelp",",,,") )
    /// Specify the codepage used to read source files
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:896)
    static member optsCodepage() = (GetStringFunc("optsCodepage",",,,") )
    /// Output messages in UTF-8 encoding
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:897)
    static member optsUtf8output() = (GetStringFunc("optsUtf8output",",,,") )
    /// Output messages with fully qualified paths
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:898)
    static member optsFullpaths() = (GetStringFunc("optsFullpaths",",,,") )
    /// Specify a directory for the include path which is used to resolve source files and assemblies (Short form: -I)
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:899)
    static member optsLib() = (GetStringFunc("optsLib",",,,") )
    /// Base address for the library to be built
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:900)
    static member optsBaseaddress() = (GetStringFunc("optsBaseaddress",",,,") )
    /// Do not reference the default CLI assemblies by default
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:901)
    static member optsNoframework() = (GetStringFunc("optsNoframework",",,,") )
    /// Statically link the F# library and all referenced DLLs that depend on it into the assembly being generated
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:902)
    static member optsStandalone() = (GetStringFunc("optsStandalone",",,,") )
    /// Statically link the given assembly and all referenced DLLs that depend on this assembly. Use an assembly name e.g. mylib, not a DLL name.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:903)
    static member optsStaticlink() = (GetStringFunc("optsStaticlink",",,,") )
    /// Use a resident background compilation service to improve compiler startup times.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:904)
    static member optsResident() = (GetStringFunc("optsResident",",,,") )
    /// Name the output debug file
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:905)
    static member optsPdb() = (GetStringFunc("optsPdb",",,,") )
    /// Resolve assembly references using directory-based rules rather than MSBuild resolution
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:906)
    static member optsSimpleresolution() = (GetStringFunc("optsSimpleresolution",",,,") )
    /// Unrecognized target '%s', expected 'exe', 'winexe', 'library' or 'module'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:907)
    static member optsUnrecognizedTarget(a0 : System.String) = (1048, GetStringFunc("optsUnrecognizedTarget",",,,%s,,,") a0)
    /// Unrecognized debug type '%s', expected 'pdbonly' or 'full'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:908)
    static member optsUnrecognizedDebugType(a0 : System.String) = (1049, GetStringFunc("optsUnrecognizedDebugType",",,,%s,,,") a0)
    /// Invalid warning level '%d'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:909)
    static member optsInvalidWarningLevel(a0 : System.Int32) = (1050, GetStringFunc("optsInvalidWarningLevel",",,,%d,,,") a0)
    /// Short form of '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:910)
    static member optsShortFormOf(a0 : System.String) = (GetStringFunc("optsShortFormOf",",,,%s,,,") a0)
    /// The command-line option '--cliroot' has been deprecated. Use an explicit reference to a specific copy of mscorlib.dll instead.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:911)
    static member optsClirootDeprecatedMsg() = (GetStringFunc("optsClirootDeprecatedMsg",",,,") )
    /// Use to override where the compiler looks for mscorlib.dll and framework components
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:912)
    static member optsClirootDescription() = (GetStringFunc("optsClirootDescription",",,,") )
    /// - OUTPUT FILES -
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:913)
    static member optsHelpBannerOutputFiles() = (GetStringFunc("optsHelpBannerOutputFiles",",,,") )
    /// - INPUT FILES -
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:914)
    static member optsHelpBannerInputFiles() = (GetStringFunc("optsHelpBannerInputFiles",",,,") )
    /// - RESOURCES -
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:915)
    static member optsHelpBannerResources() = (GetStringFunc("optsHelpBannerResources",",,,") )
    /// - CODE GENERATION -
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:916)
    static member optsHelpBannerCodeGen() = (GetStringFunc("optsHelpBannerCodeGen",",,,") )
    /// - ADVANCED -
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:917)
    static member optsHelpBannerAdvanced() = (GetStringFunc("optsHelpBannerAdvanced",",,,") )
    /// - MISCELLANEOUS -
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:918)
    static member optsHelpBannerMisc() = (GetStringFunc("optsHelpBannerMisc",",,,") )
    /// - LANGUAGE -
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:919)
    static member optsHelpBannerLanguage() = (GetStringFunc("optsHelpBannerLanguage",",,,") )
    /// - ERRORS AND WARNINGS -
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:920)
    static member optsHelpBannerErrsAndWarns() = (GetStringFunc("optsHelpBannerErrsAndWarns",",,,") )
    /// Unknown --test argument: '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:921)
    static member optsUnknownArgumentToTheTestSwitch(a0 : System.String) = (1063, GetStringFunc("optsUnknownArgumentToTheTestSwitch",",,,%s,,,") a0)
    /// Unrecognized platform '%s', valid values are 'x86', 'x64', 'Itanium', 'anycpu32bitpreferred', and 'anycpu'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:922)
    static member optsUnknownPlatform(a0 : System.String) = (1064, GetStringFunc("optsUnknownPlatform",",,,%s,,,") a0)
    /// The command-line option '%s' is for internal use only
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:923)
    static member optsInternalNoDescription(a0 : System.String) = (GetStringFunc("optsInternalNoDescription",",,,%s,,,") a0)
    /// The command-line option '%s' has been deprecated
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:924)
    static member optsDCLONoDescription(a0 : System.String) = (GetStringFunc("optsDCLONoDescription",",,,%s,,,") a0)
    /// The command-line option '%s' has been deprecated. Use '%s' instead.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:925)
    static member optsDCLODeprecatedSuggestAlternative(a0 : System.String, a1 : System.String) = (GetStringFunc("optsDCLODeprecatedSuggestAlternative",",,,%s,,,%s,,,") a0 a1)
    /// The command-line option '%s' has been deprecated. HTML document generation is now part of the F# Power Pack, via the tool FsHtmlDoc.exe.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:926)
    static member optsDCLOHtmlDoc(a0 : System.String) = (GetStringFunc("optsDCLOHtmlDoc",",,,%s,,,") a0)
    /// Output warning and error messages in color
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:927)
    static member optsConsoleColors() = (GetStringFunc("optsConsoleColors",",,,") )
    /// Enable high-entropy ASLR
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:928)
    static member optsUseHighEntropyVA() = (GetStringFunc("optsUseHighEntropyVA",",,,") )
    /// Specify subsystem version of this assembly
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:929)
    static member optsSubSystemVersion() = (GetStringFunc("optsSubSystemVersion",",,,") )
    /// Invalid version '%s' for '--subsystemversion'. The version must be 4.00 or greater.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:930)
    static member optsInvalidSubSystemVersion(a0 : System.String) = (1051, GetStringFunc("optsInvalidSubSystemVersion",",,,%s,,,") a0)
    /// Full name
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:934)
    static member typeInfoFullName() = (GetStringFunc("typeInfoFullName",",,,") )
    /// type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:935)
    static member typeInfoType() = (GetStringFunc("typeInfoType",",,,") )
    /// inherits
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:936)
    static member typeInfoInherits() = (GetStringFunc("typeInfoInherits",",,,") )
    /// implements
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:937)
    static member typeInfoImplements() = (GetStringFunc("typeInfoImplements",",,,") )
    /// and %d other overloads
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:938)
    static member typeInfoOtherOverloads(a0 : System.Int32) = (GetStringFunc("typeInfoOtherOverloads",",,,%d,,,") a0)
    /// union case
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:939)
    static member typeInfoUnionCase() = (GetStringFunc("typeInfoUnionCase",",,,") )
    /// active pattern result
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:940)
    static member typeInfoActivePatternResult() = (GetStringFunc("typeInfoActivePatternResult",",,,") )
    /// active recognizer
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:941)
    static member typeInfoActiveRecognizer() = (GetStringFunc("typeInfoActiveRecognizer",",,,") )
    /// field
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:942)
    static member typeInfoField() = (GetStringFunc("typeInfoField",",,,") )
    /// event
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:943)
    static member typeInfoEvent() = (GetStringFunc("typeInfoEvent",",,,") )
    /// property
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:944)
    static member typeInfoProperty() = (GetStringFunc("typeInfoProperty",",,,") )
    /// custom operation
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:945)
    static member typeInfoCustomOperation() = (GetStringFunc("typeInfoCustomOperation",",,,") )
    /// argument
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:946)
    static member typeInfoArgument() = (GetStringFunc("typeInfoArgument",",,,") )
    /// patvar
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:947)
    static member typeInfoPatternVariable() = (GetStringFunc("typeInfoPatternVariable",",,,") )
    /// namespace
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:948)
    static member typeInfoNamespace() = (GetStringFunc("typeInfoNamespace",",,,") )
    /// module
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:949)
    static member typeInfoModule() = (GetStringFunc("typeInfoModule",",,,") )
    /// namespace/module
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:950)
    static member typeInfoNamespaceOrModule() = (GetStringFunc("typeInfoNamespaceOrModule",",,,") )
    /// from %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:951)
    static member typeInfoFromFirst(a0 : System.String) = (GetStringFunc("typeInfoFromFirst",",,,%s,,,") a0)
    /// also from %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:952)
    static member typeInfoFromNext(a0 : System.String) = (GetStringFunc("typeInfoFromNext",",,,%s,,,") a0)
    /// generated property
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:953)
    static member typeInfoGeneratedProperty() = (GetStringFunc("typeInfoGeneratedProperty",",,,") )
    /// generated type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:954)
    static member typeInfoGeneratedType() = (GetStringFunc("typeInfoGeneratedType",",,,") )
    /// Found by AssemblyFolders registry key
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:955)
    static member assemblyResolutionFoundByAssemblyFoldersKey() = (GetStringFunc("assemblyResolutionFoundByAssemblyFoldersKey",",,,") )
    /// Found by AssemblyFoldersEx registry key
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:956)
    static member assemblyResolutionFoundByAssemblyFoldersExKey() = (GetStringFunc("assemblyResolutionFoundByAssemblyFoldersExKey",",,,") )
    /// .NET Framework
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:957)
    static member assemblyResolutionNetFramework() = (GetStringFunc("assemblyResolutionNetFramework",",,,") )
    /// Global Assembly Cache
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:958)
    static member assemblyResolutionGAC() = (GetStringFunc("assemblyResolutionGAC",",,,") )
    /// Recursive class hierarchy in type '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:962)
    static member recursiveClassHierarchy(a0 : System.String) = (1089, GetStringFunc("recursiveClassHierarchy",",,,%s,,,") a0)
    /// Invalid recursive reference to an abstract slot
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:963)
    static member InvalidRecursiveReferenceToAbstractSlot() = (1090, GetStringFunc("InvalidRecursiveReferenceToAbstractSlot",",,,") )
    /// The event '%s' has a non-standard type. If this event is declared in another CLI language, you may need to access this event using the explicit %s and %s methods for the event. If this event is declared in F#, make the type of the event an instantiation of either 'IDelegateEvent<_>' or 'IEvent<_,_>'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:964)
    static member eventHasNonStandardType(a0 : System.String, a1 : System.String, a2 : System.String) = (1091, GetStringFunc("eventHasNonStandardType",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// The type '%s' is not accessible from this code location
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:965)
    static member typeIsNotAccessible(a0 : System.String) = (1092, GetStringFunc("typeIsNotAccessible",",,,%s,,,") a0)
    /// The union cases or fields of the type '%s' are not accessible from this code location
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:966)
    static member unionCasesAreNotAccessible(a0 : System.String) = (1093, GetStringFunc("unionCasesAreNotAccessible",",,,%s,,,") a0)
    /// The value '%s' is not accessible from this code location
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:967)
    static member valueIsNotAccessible(a0 : System.String) = (1094, GetStringFunc("valueIsNotAccessible",",,,%s,,,") a0)
    /// The union case '%s' is not accessible from this code location
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:968)
    static member unionCaseIsNotAccessible(a0 : System.String) = (1095, GetStringFunc("unionCaseIsNotAccessible",",,,%s,,,") a0)
    /// The record, struct or class field '%s' is not accessible from this code location
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:969)
    static member fieldIsNotAccessible(a0 : System.String) = (1096, GetStringFunc("fieldIsNotAccessible",",,,%s,,,") a0)
    /// The struct or class field '%s' is not accessible from this code location
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:970)
    static member structOrClassFieldIsNotAccessible(a0 : System.String) = (1097, GetStringFunc("structOrClassFieldIsNotAccessible",",,,%s,,,") a0)
    /// This construct is experimental
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:971)
    static member experimentalConstruct() = (GetStringFunc("experimentalConstruct",",,,") )
    /// No Invoke methods found for delegate type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:972)
    static member noInvokeMethodsFound() = (1099, GetStringFunc("noInvokeMethodsFound",",,,") )
    /// More than one Invoke method found for delegate type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:973)
    static member moreThanOneInvokeMethodFound() = (GetStringFunc("moreThanOneInvokeMethodFound",",,,") )
    /// Delegates are not allowed to have curried signatures
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:974)
    static member delegatesNotAllowedToHaveCurriedSignatures() = (1101, GetStringFunc("delegatesNotAllowedToHaveCurriedSignatures",",,,") )
    /// Unexpected Expr.TyChoose
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:978)
    static member tlrUnexpectedTExpr() = (1102, GetStringFunc("tlrUnexpectedTExpr",",,,") )
    /// Note: Lambda-lifting optimizations have not been applied because of the use of this local constrained generic function as a first class value. Adding type constraints may resolve this condition.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:979)
    static member tlrLambdaLiftingOptimizationsNotApplied() = (1103, GetStringFunc("tlrLambdaLiftingOptimizationsNotApplied",",,,") )
    /// Identifiers containing '@' are reserved for use in F# code generation
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:983)
    static member lexhlpIdentifiersContainingAtSymbolReserved() = (1104, GetStringFunc("lexhlpIdentifiersContainingAtSymbolReserved",",,,") )
    /// The identifier '%s' is reserved for future use by F#
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:984)
    static member lexhlpIdentifierReserved(a0 : System.String) = (GetStringFunc("lexhlpIdentifierReserved",",,,%s,,,") a0)
    /// Missing variable '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:988)
    static member patcMissingVariable(a0 : System.String) = (1106, GetStringFunc("patcMissingVariable",",,,%s,,,") a0)
    /// Partial active patterns may only generate one result
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:989)
    static member patcPartialActivePatternsGenerateOneResult() = (1107, GetStringFunc("patcPartialActivePatternsGenerateOneResult",",,,") )
    /// The type '%s' is required here and is unavailable. You must add a reference to assembly '%s'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:993)
    static member impTypeRequiredUnavailable(a0 : System.String, a1 : System.String) = (1108, GetStringFunc("impTypeRequiredUnavailable",",,,%s,,,%s,,,") a0 a1)
    /// A reference to the type '%s' in assembly '%s' was found, but the type could not be found in that assembly
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:994)
    static member impReferencedTypeCouldNotBeFoundInAssembly(a0 : System.String, a1 : System.String) = (1109, GetStringFunc("impReferencedTypeCouldNotBeFoundInAssembly",",,,%s,,,%s,,,") a0 a1)
    /// Internal error or badly formed metadata: not enough type parameters were in scope while importing
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:995)
    static member impNotEnoughTypeParamsInScopeWhileImporting() = (1110, GetStringFunc("impNotEnoughTypeParamsInScopeWhileImporting",",,,") )
    /// A reference to the DLL %s is required by assembly %s. The imported type %s is located in the first assembly and could not be resolved.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:996)
    static member impReferenceToDllRequiredByAssembly(a0 : System.String, a1 : System.String, a2 : System.String) = (1111, GetStringFunc("impReferenceToDllRequiredByAssembly",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// An imported assembly uses the type '%s' but that type is not public
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:997)
    static member impImportedAssemblyUsesNotPublicType(a0 : System.String) = (1112, GetStringFunc("impImportedAssemblyUsesNotPublicType",",,,%s,,,") a0)
    /// The value '%s' was marked inline but its implementation makes use of an internal or private function which is not sufficiently accessible
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1001)
    static member optValueMarkedInlineButIncomplete(a0 : System.String) = (1113, GetStringFunc("optValueMarkedInlineButIncomplete",",,,%s,,,") a0)
    /// The value '%s' was marked inline but was not bound in the optimization environment
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1002)
    static member optValueMarkedInlineButWasNotBoundInTheOptEnv(a0 : System.String) = (1114, GetStringFunc("optValueMarkedInlineButWasNotBoundInTheOptEnv",",,,%s,,,") a0)
    /// Local value %s not found during optimization
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1003)
    static member optLocalValueNotFoundDuringOptimization(a0 : System.String) = (1115, GetStringFunc("optLocalValueNotFoundDuringOptimization",",,,%s,,,") a0)
    /// A value marked as 'inline' has an unexpected value
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1004)
    static member optValueMarkedInlineHasUnexpectedValue() = (1116, GetStringFunc("optValueMarkedInlineHasUnexpectedValue",",,,") )
    /// A value marked as 'inline' could not be inlined
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1005)
    static member optValueMarkedInlineCouldNotBeInlined() = (1117, GetStringFunc("optValueMarkedInlineCouldNotBeInlined",",,,") )
    /// Failed to inline the value '%s' marked 'inline', perhaps because a recursive value was marked 'inline'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1006)
    static member optFailedToInlineValue(a0 : System.String) = (1118, GetStringFunc("optFailedToInlineValue",",,,%s,,,") a0)
    /// Recursive ValValue %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1007)
    static member optRecursiveValValue(a0 : System.String) = (1119, GetStringFunc("optRecursiveValValue",",,,%s,,,") a0)
    /// The indentation of this 'in' token is incorrect with respect to the corresponding 'let'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1011)
    static member lexfltIncorrentIndentationOfIn() = (GetStringFunc("lexfltIncorrentIndentationOfIn",",,,") )
    /// Possible incorrect indentation: this token is offside of context started at position %s. Try indenting this token further or using standard formatting conventions.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1012)
    static member lexfltTokenIsOffsideOfContextStartedEarlier(a0 : System.String) = (GetStringFunc("lexfltTokenIsOffsideOfContextStartedEarlier",",,,%s,,,") a0)
    /// The '|' tokens separating rules of this pattern match are misaligned by one column. Consider realigning your code or using further indentation.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1013)
    static member lexfltSeparatorTokensOfPatternMatchMisaligned() = (GetStringFunc("lexfltSeparatorTokensOfPatternMatchMisaligned",",,,") )
    /// Invalid module/expression/type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1017)
    static member nrInvalidModuleExprType() = (1123, GetStringFunc("nrInvalidModuleExprType",",,,") )
    /// Multiple types exist called '%s', taking different numbers of generic parameters. Provide a type instantiation to disambiguate the type resolution, e.g. '%s'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1018)
    static member nrTypeInstantiationNeededToDisambiguateTypesWithSameName(a0 : System.String, a1 : System.String) = (1124, GetStringFunc("nrTypeInstantiationNeededToDisambiguateTypesWithSameName",",,,%s,,,%s,,,") a0 a1)
    /// The instantiation of the generic type '%s' is missing and can't be inferred from the arguments or return type of this member. Consider providing a type instantiation when accessing this type, e.g. '%s'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1019)
    static member nrTypeInstantiationIsMissingAndCouldNotBeInferred(a0 : System.String, a1 : System.String) = (1125, GetStringFunc("nrTypeInstantiationIsMissingAndCouldNotBeInferred",",,,%s,,,%s,,,") a0 a1)
    /// 'global' may only be used as the first name in a qualified path
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1020)
    static member nrGlobalUsedOnlyAsFirstName() = (1126, GetStringFunc("nrGlobalUsedOnlyAsFirstName",",,,") )
    /// This is not a constructor or literal, or a constructor is being used incorrectly
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1021)
    static member nrIsNotConstructorOrLiteral() = (1127, GetStringFunc("nrIsNotConstructorOrLiteral",",,,") )
    /// Unexpected empty long identifier
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1022)
    static member nrUnexpectedEmptyLongId() = (1128, GetStringFunc("nrUnexpectedEmptyLongId",",,,") )
    /// The type '%s' does not contain a field '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1023)
    static member nrTypeDoesNotContainSuchField(a0 : System.String, a1 : System.String) = (1129, GetStringFunc("nrTypeDoesNotContainSuchField",",,,%s,,,%s,,,") a0 a1)
    /// Invalid field label
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1024)
    static member nrInvalidFieldLabel() = (1130, GetStringFunc("nrInvalidFieldLabel",",,,") )
    /// Invalid expression '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1025)
    static member nrInvalidExpression(a0 : System.String) = (1132, GetStringFunc("nrInvalidExpression",",,,%s,,,") a0)
    /// No constructors are available for the type '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1026)
    static member nrNoConstructorsAvailableForType(a0 : System.String) = (1133, GetStringFunc("nrNoConstructorsAvailableForType",",,,%s,,,") a0)
    /// Unexpected error creating debug information file '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1030)
    static member ilwriteErrorCreatingPdb(a0 : System.String) = (1135, GetStringFunc("ilwriteErrorCreatingPdb",",,,%s,,,") a0)
    /// This number is outside the allowable range for this integer type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1034)
    static member lexOutsideIntegerRange() = (1138, GetStringFunc("lexOutsideIntegerRange",",,,") )
    /// '%s' is not permitted as a character in operator names and is reserved for future use
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1035)
    static member lexCharNotAllowedInOperatorNames(a0 : System.String) = (GetStringFunc("lexCharNotAllowedInOperatorNames",",,,%s,,,") a0)
    /// Unexpected character '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1036)
    static member lexUnexpectedChar(a0 : System.String) = (GetStringFunc("lexUnexpectedChar",",,,%s,,,") a0)
    /// This byte array literal contains characters that do not encode as a single byte
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1037)
    static member lexByteArrayCannotEncode() = (1140, GetStringFunc("lexByteArrayCannotEncode",",,,") )
    /// Identifiers followed by '%s' are reserved for future use
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1038)
    static member lexIdentEndInMarkReserved(a0 : System.String) = (1141, GetStringFunc("lexIdentEndInMarkReserved",",,,%s,,,") a0)
    /// This number is outside the allowable range for 8-bit signed integers
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1039)
    static member lexOutsideEightBitSigned() = (1142, GetStringFunc("lexOutsideEightBitSigned",",,,") )
    /// This number is outside the allowable range for hexadecimal 8-bit signed integers
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1040)
    static member lexOutsideEightBitSignedHex() = (1143, GetStringFunc("lexOutsideEightBitSignedHex",",,,") )
    /// This number is outside the allowable range for 8-bit unsigned integers
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1041)
    static member lexOutsideEightBitUnsigned() = (1144, GetStringFunc("lexOutsideEightBitUnsigned",",,,") )
    /// This number is outside the allowable range for 16-bit signed integers
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1042)
    static member lexOutsideSixteenBitSigned() = (1145, GetStringFunc("lexOutsideSixteenBitSigned",",,,") )
    /// This number is outside the allowable range for 16-bit unsigned integers
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1043)
    static member lexOutsideSixteenBitUnsigned() = (1146, GetStringFunc("lexOutsideSixteenBitUnsigned",",,,") )
    /// This number is outside the allowable range for 32-bit signed integers
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1044)
    static member lexOutsideThirtyTwoBitSigned() = (1147, GetStringFunc("lexOutsideThirtyTwoBitSigned",",,,") )
    /// This number is outside the allowable range for 32-bit unsigned integers
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1045)
    static member lexOutsideThirtyTwoBitUnsigned() = (1148, GetStringFunc("lexOutsideThirtyTwoBitUnsigned",",,,") )
    /// This number is outside the allowable range for 64-bit signed integers
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1046)
    static member lexOutsideSixtyFourBitSigned() = (1149, GetStringFunc("lexOutsideSixtyFourBitSigned",",,,") )
    /// This number is outside the allowable range for 64-bit unsigned integers
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1047)
    static member lexOutsideSixtyFourBitUnsigned() = (1150, GetStringFunc("lexOutsideSixtyFourBitUnsigned",",,,") )
    /// This number is outside the allowable range for signed native integers
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1048)
    static member lexOutsideNativeSigned() = (1151, GetStringFunc("lexOutsideNativeSigned",",,,") )
    /// This number is outside the allowable range for unsigned native integers
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1049)
    static member lexOutsideNativeUnsigned() = (1152, GetStringFunc("lexOutsideNativeUnsigned",",,,") )
    /// Invalid floating point number
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1050)
    static member lexInvalidFloat() = (1153, GetStringFunc("lexInvalidFloat",",,,") )
    /// This number is outside the allowable range for decimal literals
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1051)
    static member lexOusideDecimal() = (1154, GetStringFunc("lexOusideDecimal",",,,") )
    /// This number is outside the allowable range for 32-bit floats
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1052)
    static member lexOusideThirtyTwoBitFloat() = (1155, GetStringFunc("lexOusideThirtyTwoBitFloat",",,,") )
    /// This is not a valid numeric literal. Sample formats include 4, 0x4, 0b0100, 4L, 4UL, 4u, 4s, 4us, 4y, 4uy, 4.0, 4.0f, 4I.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1053)
    static member lexInvalidNumericLiteral() = (1156, GetStringFunc("lexInvalidNumericLiteral",",,,") )
    /// This is not a valid byte literal
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1054)
    static member lexInvalidByteLiteral() = (1157, GetStringFunc("lexInvalidByteLiteral",",,,") )
    /// This is not a valid character literal
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1055)
    static member lexInvalidCharLiteral() = (1158, GetStringFunc("lexInvalidCharLiteral",",,,") )
    /// This Unicode encoding is only valid in string literals
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1056)
    static member lexThisUnicodeOnlyInStringLiterals() = (1159, GetStringFunc("lexThisUnicodeOnlyInStringLiterals",",,,") )
    /// This token is reserved for future use
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1057)
    static member lexTokenReserved() = (1160, GetStringFunc("lexTokenReserved",",,,") )
    /// TABs are not allowed in F# code unless the #indent \"off\" option is used
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1058)
    static member lexTabsNotAllowed() = (1161, GetStringFunc("lexTabsNotAllowed",",,,") )
    /// Invalid line number: '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1059)
    static member lexInvalidLineNumber(a0 : System.String) = (1162, GetStringFunc("lexInvalidLineNumber",",,,%s,,,") a0)
    /// #if directive must appear as the first non-whitespace character on a line
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1060)
    static member lexHashIfMustBeFirst() = (1163, GetStringFunc("lexHashIfMustBeFirst",",,,") )
    /// #else has no matching #if
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1061)
    static member lexHashElseNoMatchingIf() = (GetStringFunc("lexHashElseNoMatchingIf",",,,") )
    /// #endif required for #else
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1062)
    static member lexHashEndifRequiredForElse() = (GetStringFunc("lexHashEndifRequiredForElse",",,,") )
    /// #else directive must appear as the first non-whitespace character on a line
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1063)
    static member lexHashElseMustBeFirst() = (1166, GetStringFunc("lexHashElseMustBeFirst",",,,") )
    /// #endif has no matching #if
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1064)
    static member lexHashEndingNoMatchingIf() = (GetStringFunc("lexHashEndingNoMatchingIf",",,,") )
    /// #endif directive must appear as the first non-whitespace character on a line
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1065)
    static member lexHashEndifMustBeFirst() = (1168, GetStringFunc("lexHashEndifMustBeFirst",",,,") )
    /// #if directive should be immediately followed by an identifier
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1066)
    static member lexHashIfMustHaveIdent() = (1169, GetStringFunc("lexHashIfMustHaveIdent",",,,") )
    /// Syntax error. Wrong nested #endif, unexpected tokens before it.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1067)
    static member lexWrongNestedHashEndif() = (1170, GetStringFunc("lexWrongNestedHashEndif",",,,") )
    /// Expected single line comment or end of line
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1068)
    static member lexExpectedSingleLineComment() = (1171, GetStringFunc("lexExpectedSingleLineComment",",,,") )
    /// Infix operator member '%s' has no arguments. Expected a tuple of 2 arguments, e.g. static member (+) (x,y) = ...
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1069)
    static member memberOperatorDefinitionWithNoArguments(a0 : System.String) = (1172, GetStringFunc("memberOperatorDefinitionWithNoArguments",",,,%s,,,") a0)
    /// Infix operator member '%s' has %d initial argument(s). Expected a tuple of 2 arguments, e.g. static member (+) (x,y) = ...
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1070)
    static member memberOperatorDefinitionWithNonPairArgument(a0 : System.String, a1 : System.Int32) = (1173, GetStringFunc("memberOperatorDefinitionWithNonPairArgument",",,,%s,,,%d,,,") a0 a1)
    /// Infix operator member '%s' has extra curried arguments. Expected a tuple of 2 arguments, e.g. static member (+) (x,y) = ...
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1071)
    static member memberOperatorDefinitionWithCurriedArguments(a0 : System.String) = (1174, GetStringFunc("memberOperatorDefinitionWithCurriedArguments",",,,%s,,,") a0)
    /// All record, union and struct types in FSharp.Core.dll must be explicitly labelled with 'StructuralComparison' or 'NoComparison'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1072)
    static member tcFSharpCoreRequiresExplicit() = (1175, GetStringFunc("tcFSharpCoreRequiresExplicit",",,,") )
    /// The struct, record or union type '%s' has the 'StructuralComparison' attribute but the type parameter '%s' does not satisfy the 'comparison' constraint. Consider adding the 'comparison' constraint to the type parameter
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1073)
    static member tcStructuralComparisonNotSatisfied1(a0 : System.String, a1 : System.String) = (1176, GetStringFunc("tcStructuralComparisonNotSatisfied1",",,,%s,,,%s,,,") a0 a1)
    /// The struct, record or union type '%s' has the 'StructuralComparison' attribute but the component type '%s' does not satisfy the 'comparison' constraint
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1074)
    static member tcStructuralComparisonNotSatisfied2(a0 : System.String, a1 : System.String) = (1177, GetStringFunc("tcStructuralComparisonNotSatisfied2",",,,%s,,,%s,,,") a0 a1)
    /// The struct, record or union type '%s' is not structurally comparable because the type parameter %s does not satisfy the 'comparison' constraint. Consider adding the 'NoComparison' attribute to the type '%s' to clarify that the type is not comparable
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1075)
    static member tcNoComparisonNeeded1(a0 : System.String, a1 : System.String, a2 : System.String) = (1178, GetStringFunc("tcNoComparisonNeeded1",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// The struct, record or union type '%s' is not structurally comparable because the type '%s' does not satisfy the 'comparison' constraint. Consider adding the 'NoComparison' attribute to the type '%s' to clarify that the type is not comparable
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1076)
    static member tcNoComparisonNeeded2(a0 : System.String, a1 : System.String, a2 : System.String) = (1178, GetStringFunc("tcNoComparisonNeeded2",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// The struct, record or union type '%s' does not support structural equality because the type parameter %s does not satisfy the 'equality' constraint. Consider adding the 'NoEquality' attribute to the type '%s' to clarify that the type does not support structural equality
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1077)
    static member tcNoEqualityNeeded1(a0 : System.String, a1 : System.String, a2 : System.String) = (1178, GetStringFunc("tcNoEqualityNeeded1",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// The struct, record or union type '%s' does not support structural equality because the type '%s' does not satisfy the 'equality' constraint. Consider adding the 'NoEquality' attribute to the type '%s' to clarify that the type does not support structural equality
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1078)
    static member tcNoEqualityNeeded2(a0 : System.String, a1 : System.String, a2 : System.String) = (1178, GetStringFunc("tcNoEqualityNeeded2",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// The struct, record or union type '%s' has the 'StructuralEquality' attribute but the type parameter '%s' does not satisfy the 'equality' constraint. Consider adding the 'equality' constraint to the type parameter
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1079)
    static member tcStructuralEqualityNotSatisfied1(a0 : System.String, a1 : System.String) = (1179, GetStringFunc("tcStructuralEqualityNotSatisfied1",",,,%s,,,%s,,,") a0 a1)
    /// The struct, record or union type '%s' has the 'StructuralEquality' attribute but the component type '%s' does not satisfy the 'equality' constraint
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1080)
    static member tcStructuralEqualityNotSatisfied2(a0 : System.String, a1 : System.String) = (1180, GetStringFunc("tcStructuralEqualityNotSatisfied2",",,,%s,,,%s,,,") a0 a1)
    /// Each argument of the primary constructor for a struct must be given a type, for example 'type S(x1:int, x2: int) = ...'. These arguments determine the fields of the struct.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1081)
    static member tcStructsMustDeclareTypesOfImplicitCtorArgsExplicitly() = (1181, GetStringFunc("tcStructsMustDeclareTypesOfImplicitCtorArgsExplicitly",",,,") )
    /// The value '%s' is unused
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1082)
    static member chkUnusedValue(a0 : System.String) = (1182, GetStringFunc("chkUnusedValue",",,,%s,,,") a0)
    /// The recursive object reference '%s' is unused. The presence of a recursive object reference adds runtime initialization checks to members in this and derived types. Consider removing this recursive object reference.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1083)
    static member chkUnusedThisVariable(a0 : System.String) = (1183, GetStringFunc("chkUnusedThisVariable",",,,%s,,,") a0)
    /// A getter property may have at most one argument group
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1084)
    static member parsGetterAtMostOneArgument() = (1184, GetStringFunc("parsGetterAtMostOneArgument",",,,") )
    /// A setter property may have at most two argument groups
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1085)
    static member parsSetterAtMostTwoArguments() = (1185, GetStringFunc("parsSetterAtMostTwoArguments",",,,") )
    /// Invalid property getter or setter
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1086)
    static member parsInvalidProperty() = (1186, GetStringFunc("parsInvalidProperty",",,,") )
    /// An indexer property must be given at least one argument
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1087)
    static member parsIndexerPropertyRequiresAtLeastOneArgument() = (1187, GetStringFunc("parsIndexerPropertyRequiresAtLeastOneArgument",",,,") )
    /// This operation accesses a mutable top-level value defined in another assembly in an unsupported way. The value cannot be accessed through its address. Consider copying the expression to a mutable local, e.g. 'let mutable x = ...', and if necessary assigning the value back after the completion of the operation
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1088)
    static member tastInvalidAddressOfMutableAcrossAssemblyBoundary() = (1188, GetStringFunc("tastInvalidAddressOfMutableAcrossAssemblyBoundary",",,,") )
    /// Type parameters must be placed directly adjacent to the type name, e.g. \"type C<'T>\", not     type \"C   <'T>\"
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1089)
    static member parsNonAdjacentTypars() = (1189, GetStringFunc("parsNonAdjacentTypars",",,,") )
    /// Type arguments must be placed directly adjacent to the type name, e.g. \"C<'T>\", not \"C  <'T>\"
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1090)
    static member parsNonAdjacentTyargs() = (1190, GetStringFunc("parsNonAdjacentTyargs",",,,") )
    /// The use of the type syntax 'int C' and 'C  <int>' is not permitted here. Consider adjusting this type to be written in the form 'C<int>'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1091)
    static member parsNonAtomicType() = (GetStringFunc("parsNonAtomicType",",,,") )
    /// The type %s did not contain the field '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1092)
    static member tastUndefinedTyconItemField(a0 : System.String, a1 : System.String) = (1191, GetStringFunc("tastUndefinedTyconItemField",",,,%s,,,%s,,,") a0 a1)
    /// The type %s did not contain the union case '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1093)
    static member tastUndefinedTyconItemUnionCase(a0 : System.String, a1 : System.String) = (1192, GetStringFunc("tastUndefinedTyconItemUnionCase",",,,%s,,,%s,,,") a0 a1)
    /// The module/namespace '%s' from compilation unit '%s' did not contain the module/namespace '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1094)
    static member tastUndefinedItemRefModuleNamespace(a0 : System.String, a1 : System.String, a2 : System.String) = (1193, GetStringFunc("tastUndefinedItemRefModuleNamespace",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// The module/namespace '%s' from compilation unit '%s' did not contain the val '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1095)
    static member tastUndefinedItemRefVal(a0 : System.String, a1 : System.String, a2 : System.String) = (1194, GetStringFunc("tastUndefinedItemRefVal",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// The module/namespace '%s' from compilation unit '%s' did not contain the namespace, module or type '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1096)
    static member tastUndefinedItemRefModuleNamespaceType(a0 : System.String, a1 : System.String, a2 : System.String) = (1195, GetStringFunc("tastUndefinedItemRefModuleNamespaceType",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// The 'UseNullAsTrueValue' attribute flag may only be used with union types that have one nullary case and at least one non-nullary case
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1097)
    static member tcInvalidUseNullAsTrueValue() = (1196, GetStringFunc("tcInvalidUseNullAsTrueValue",",,,") )
    /// The parameter '%s' was inferred to have byref type. Parameters of byref type must be given an explicit type annotation, e.g. 'x1: byref<int>'. When used, a byref parameter is implicitly dereferenced.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1098)
    static member tcParameterInferredByref(a0 : System.String) = (1197, GetStringFunc("tcParameterInferredByref",",,,%s,,,") a0)
    /// The generic member '%s' has been used at a non-uniform instantiation prior to this program point. Consider reordering the members so this member occurs first. Alternatively, specify the full type of the member explicitly, including argument types, return type and any additional generic parameters and constraints.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1099)
    static member tcNonUniformMemberUse(a0 : System.String) = (1198, GetStringFunc("tcNonUniformMemberUse",",,,%s,,,") a0)
    /// The use of named arguments in union case expressions is reserved for future use. Arguments of the form 'a=b' should be parenthesized.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1100)
    static member tcNamedArgumentsCannotBeUsedInUnionCaseConstructions() = (1199, GetStringFunc("tcNamedArgumentsCannotBeUsedInUnionCaseConstructions",",,,") )
    /// The attribute '%s' appears in both the implementation and the signature, but the attribute arguments differ. Only the attribute from the signature will be included in the compiled code.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1101)
    static member tcAttribArgsDiffer(a0 : System.String) = (1200, GetStringFunc("tcAttribArgsDiffer",",,,%s,,,") a0)
    /// Cannot call an abstract base member: '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1102)
    static member tcCannotCallAbstractBaseMember(a0 : System.String) = (1201, GetStringFunc("tcCannotCallAbstractBaseMember",",,,%s,,,") a0)
    /// Could not resolve the ambiguity in the use of a generic construct with an 'unmanaged' constraint at or near this position
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1103)
    static member typrelCannotResolveAmbiguityInUnmanaged() = (1202, GetStringFunc("typrelCannotResolveAmbiguityInUnmanaged",",,,") )
    /// This construct is for ML compatibility. %s. You can disable this warning by using '--mlcompatibility' or '--nowarn:62'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1106)
    static member mlCompatMessage(a0 : System.String) = (GetStringFunc("mlCompatMessage",",,,%s,,,") a0)
    /// The type '%s' has been marked as having an Explicit layout, but the field '%s' has not been marked with the 'FieldOffset' attribute
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1108)
    static member ilFieldDoesNotHaveValidOffsetForStructureLayout(a0 : System.String, a1 : System.String) = (1206, GetStringFunc("ilFieldDoesNotHaveValidOffsetForStructureLayout",",,,%s,,,%s,,,") a0 a1)
    /// Interfaces inherited by other interfaces should be declared using 'inherit ...' instead of 'interface ...'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1109)
    static member tcInterfacesShouldUseInheritNotInterface() = (1207, GetStringFunc("tcInterfacesShouldUseInheritNotInterface",",,,") )
    /// Invalid prefix operator
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1110)
    static member parsInvalidPrefixOperator() = (1208, GetStringFunc("parsInvalidPrefixOperator",",,,") )
    /// Invalid operator definition. Prefix operator definitions must use a valid prefix operator name.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1111)
    static member parsInvalidPrefixOperatorDefinition() = (1208, GetStringFunc("parsInvalidPrefixOperatorDefinition",",,,") )
    /// The file extensions '.ml' and '.mli' are for ML compatibility
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1112)
    static member buildCompilingExtensionIsForML() = (GetStringFunc("buildCompilingExtensionIsForML",",,,") )
    /// Consider using a file with extension '.ml' or '.mli' instead
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1113)
    static member lexIndentOffForML() = (GetStringFunc("lexIndentOffForML",",,,") )
    /// Active pattern '%s' is not a function
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1114)
    static member activePatternIdentIsNotFunctionTyped(a0 : System.String) = (1209, GetStringFunc("activePatternIdentIsNotFunctionTyped",",,,%s,,,") a0)
    /// Active pattern '%s' has a result type containing type variables that are not determined by the input. The common cause is a when a result case is not mentioned, e.g. 'let (|A|B|) (x:int) = A x'. This can be fixed with a type constraint, e.g. 'let (|A|B|) (x:int) : Choice<int,unit> = A x'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1115)
    static member activePatternChoiceHasFreeTypars(a0 : System.String) = (1210, GetStringFunc("activePatternChoiceHasFreeTypars",",,,%s,,,") a0)
    /// The FieldOffset attribute can only be placed on members of types marked with the StructLayout(LayoutKind.Explicit)
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1116)
    static member ilFieldHasOffsetForSequentialLayout() = (1211, GetStringFunc("ilFieldHasOffsetForSequentialLayout",",,,") )
    /// Optional arguments must come at the end of the argument list, after any non-optional arguments
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1117)
    static member tcOptionalArgsMustComeAfterNonOptionalArgs() = (1212, GetStringFunc("tcOptionalArgsMustComeAfterNonOptionalArgs",",,,") )
    /// Attribute 'System.Diagnostics.ConditionalAttribute' is only valid on methods or attribute classes
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1118)
    static member tcConditionalAttributeUsage() = (1213, GetStringFunc("tcConditionalAttributeUsage",",,,") )
    /// Extension members cannot provide operator overloads.  Consider defining the operator as part of the type definition instead.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1120)
    static member tcMemberOperatorDefinitionInExtrinsic() = (1215, GetStringFunc("tcMemberOperatorDefinitionInExtrinsic",",,,") )
    /// The name of the MDB file must be <assembly-file-name>.mdb. The --pdb option will be ignored.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1121)
    static member ilwriteMDBFileNameCannotBeChangedWarning() = (1216, GetStringFunc("ilwriteMDBFileNameCannotBeChangedWarning",",,,") )
    /// MDB generation failed. Could not find compatible member %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1122)
    static member ilwriteMDBMemberMissing(a0 : System.String) = (1217, GetStringFunc("ilwriteMDBMemberMissing",",,,%s,,,") a0)
    /// Cannot generate MDB debug information. Failed to load the 'MonoSymbolWriter' type from the 'Mono.CompilerServices.SymbolWriter.dll' assembly.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1123)
    static member ilwriteErrorCreatingMdb() = (1218, GetStringFunc("ilwriteErrorCreatingMdb",",,,") )
    /// The union case named '%s' conflicts with the generated type '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1124)
    static member tcUnionCaseNameConflictsWithGeneratedType(a0 : System.String, a1 : System.String) = (1219, GetStringFunc("tcUnionCaseNameConflictsWithGeneratedType",",,,%s,,,%s,,,") a0 a1)
    /// ReflectedDefinitionAttribute may not be applied to an instance member on a struct type, because the instance member takes an implicit 'this' byref parameter
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1125)
    static member chkNoReflectedDefinitionOnStructMember() = (1220, GetStringFunc("chkNoReflectedDefinitionOnStructMember",",,,") )
    /// DLLImport bindings must be static members in a class or function definitions in a module
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1126)
    static member tcDllImportNotAllowed() = (1221, GetStringFunc("tcDllImportNotAllowed",",,,") )
    /// When mscorlib.dll or FSharp.Core.dll is explicitly referenced the %s option must also be passed
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1127)
    static member buildExplicitCoreLibRequiresNoFramework(a0 : System.String) = (1222, GetStringFunc("buildExplicitCoreLibRequiresNoFramework",",,,%s,,,") a0)
    /// FSharp.Core.sigdata not found alongside FSharp.Core
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1128)
    static member buildExpectedSigdataFile() = (1223, GetStringFunc("buildExpectedSigdataFile",",,,") )
    /// Did not expect to find optdata resource in FSharp.Core.dll
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1129)
    static member buildDidNotExpectOptDataResource() = (1224, GetStringFunc("buildDidNotExpectOptDataResource",",,,") )
    /// File '%s' not found alongside FSharp.Core
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1130)
    static member buildExpectedFileAlongSideFSharpCore(a0 : System.String) = (1225, GetStringFunc("buildExpectedFileAlongSideFSharpCore",",,,%s,,,") a0)
    /// Did not expect to find sigdata resource in FSharp.Core.dll
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1131)
    static member buildDidNotExpectSigdataResource() = (1226, GetStringFunc("buildDidNotExpectSigdataResource",",,,") )
    /// Filename '%s' contains invalid character '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1132)
    static member buildUnexpectedFileNameCharacter(a0 : System.String, a1 : System.String) = (1227, GetStringFunc("buildUnexpectedFileNameCharacter",",,,%s,,,%s,,,") a0 a1)
    /// 'use!' bindings must be of the form 'use! <var> = <expr>'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1133)
    static member tcInvalidUseBangBinding() = (1228, GetStringFunc("tcInvalidUseBangBinding",",,,") )
    /// Inner generic functions are not permitted in quoted expressions. Consider adding some type constraints until this function is no longer generic.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1134)
    static member crefNoInnerGenericsInQuotations() = (1230, GetStringFunc("crefNoInnerGenericsInQuotations",",,,") )
    /// The type '%s' is not a valid enumerator type , i.e. does not have a 'MoveNext()' method returning a bool, and a 'Current' property
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1135)
    static member tcEnumTypeCannotBeEnumerated(a0 : System.String) = (1231, GetStringFunc("tcEnumTypeCannotBeEnumerated",",,,%s,,,") a0)
    /// End of file in triple-quote string begun at or before here
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1136)
    static member parsEofInTripleQuoteString() = (1232, GetStringFunc("parsEofInTripleQuoteString",",,,") )
    /// End of file in triple-quote string embedded in comment begun at or before here
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1137)
    static member parsEofInTripleQuoteStringInComment() = (1233, GetStringFunc("parsEofInTripleQuoteStringInComment",",,,") )
    /// This type test or downcast will ignore the unit-of-measure '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1138)
    static member tcTypeTestLosesMeasures(a0 : System.String) = (1240, GetStringFunc("tcTypeTestLosesMeasures",",,,%s,,,") a0)
    /// Expected type argument or static argument
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1139)
    static member parsMissingTypeArgs() = (1241, GetStringFunc("parsMissingTypeArgs",",,,") )
    /// Unmatched '<'. Expected closing '>'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1140)
    static member parsMissingGreaterThan() = (1242, GetStringFunc("parsMissingGreaterThan",",,,") )
    /// Unexpected quotation operator '<@' in type definition. If you intend to pass a verbatim string as a static argument to a type provider, put a space between the '<' and '@' characters.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1141)
    static member parsUnexpectedQuotationOperatorInTypeAliasDidYouMeanVerbatimString() = (1243, GetStringFunc("parsUnexpectedQuotationOperatorInTypeAliasDidYouMeanVerbatimString",",,,") )
    /// Attempted to parse this as an operator name, but failed
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1142)
    static member parsErrorParsingAsOperatorName() = (1244, GetStringFunc("parsErrorParsingAsOperatorName",",,,") )
    /// Exiting - too many errors
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1144)
    static member fscTooManyErrors() = (GetStringFunc("fscTooManyErrors",",,,") )
    /// The documentation file has no .xml suffix
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1145)
    static member docfileNoXmlSuffix() = (2001, GetStringFunc("docfileNoXmlSuffix",",,,") )
    /// No implementation files specified
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1146)
    static member fscNoImplementationFiles() = (2002, GetStringFunc("fscNoImplementationFiles",",,,") )
    /// An AssemblyVersionAttribute specified version '%s', but this value is invalid and has been ignored
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1147)
    static member fscBadAssemblyVersion(a0 : System.String) = (2003, GetStringFunc("fscBadAssemblyVersion",",,,%s,,,") a0)
    /// Conflicting options specified: 'win32manifest' and 'win32res'. Only one of these can be used.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1148)
    static member fscTwoResourceManifests() = (2004, GetStringFunc("fscTwoResourceManifests",",,,") )
    /// The code in assembly '%s' makes uses of quotation literals. Static linking may not include components that make use of quotation literals.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1149)
    static member fscQuotationLiteralsStaticLinking(a0 : System.String) = (2005, GetStringFunc("fscQuotationLiteralsStaticLinking",",,,%s,,,") a0)
    /// Code in this assembly makes uses of quotation literals. Static linking may not include components that make use of quotation literals.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1150)
    static member fscQuotationLiteralsStaticLinking0() = (2006, GetStringFunc("fscQuotationLiteralsStaticLinking0",",,,") )
    /// Static linking may not include a .EXE
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1151)
    static member fscStaticLinkingNoEXE() = (2007, GetStringFunc("fscStaticLinkingNoEXE",",,,") )
    /// Static linking may not include a mixed managed/unmanaged DLL
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1152)
    static member fscStaticLinkingNoMixedDLL() = (2008, GetStringFunc("fscStaticLinkingNoMixedDLL",",,,") )
    /// Ignoring mixed managed/unmanaged assembly '%s' during static linking
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1153)
    static member fscIgnoringMixedWhenLinking(a0 : System.String) = (2009, GetStringFunc("fscIgnoringMixedWhenLinking",",,,%s,,,") a0)
    /// Assembly '%s' was referenced transitively and the assembly could not be resolved automatically. Static linking will assume this DLL has no dependencies on the F# library or other statically linked DLLs. Consider adding an explicit reference to this DLL.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1154)
    static member fscAssumeStaticLinkContainsNoDependencies(a0 : System.String) = (2011, GetStringFunc("fscAssumeStaticLinkContainsNoDependencies",",,,%s,,,") a0)
    /// Assembly '%s' not found in dependency set of target binary. Statically linked roots should be specified using an assembly name, without a DLL or EXE extension. If this assembly was referenced explicitly then it is possible the assembly was not actually required by the generated binary, in which case it should not be statically linked.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1155)
    static member fscAssemblyNotFoundInDependencySet(a0 : System.String) = (2012, GetStringFunc("fscAssemblyNotFoundInDependencySet",",,,%s,,,") a0)
    /// The key file '%s' could not be opened
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1156)
    static member fscKeyFileCouldNotBeOpened(a0 : System.String) = (2013, GetStringFunc("fscKeyFileCouldNotBeOpened",",,,%s,,,") a0)
    /// A problem occurred writing the binary '%s': %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1157)
    static member fscProblemWritingBinary(a0 : System.String, a1 : System.String) = (2014, GetStringFunc("fscProblemWritingBinary",",,,%s,,,%s,,,") a0 a1)
    /// The 'AssemblyVersionAttribute' has been ignored because a version was given using a command line option
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1158)
    static member fscAssemblyVersionAttributeIgnored() = (2015, GetStringFunc("fscAssemblyVersionAttributeIgnored",",,,") )
    /// Error emitting 'System.Reflection.AssemblyCultureAttribute' attribute -- 'Executables cannot be satellite assemblies, Culture should always be empty'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1159)
    static member fscAssemblyCultureAttributeError() = (2016, GetStringFunc("fscAssemblyCultureAttributeError",",,,") )
    /// Option '--delaysign' overrides attribute 'System.Reflection.AssemblyDelaySignAttribute' given in a source file or added module
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1160)
    static member fscDelaySignWarning() = (2017, GetStringFunc("fscDelaySignWarning",",,,") )
    /// Option '--keyfile' overrides attribute 'System.Reflection.AssemblyKeyFileAttribute' given in a source file or added module
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1161)
    static member fscKeyFileWarning() = (2018, GetStringFunc("fscKeyFileWarning",",,,") )
    /// Option '--keycontainer' overrides attribute 'System.Reflection.AssemblyNameAttribute' given in a source file or added module
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1162)
    static member fscKeyNameWarning() = (2019, GetStringFunc("fscKeyNameWarning",",,,") )
    /// The assembly '%s' is listed on the command line. Assemblies should be referenced using a command line flag such as '-r'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1163)
    static member fscReferenceOnCommandLine(a0 : System.String) = (2020, GetStringFunc("fscReferenceOnCommandLine",",,,%s,,,") a0)
    /// The resident compilation service was not used because a problem occured in communicating with the server.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1164)
    static member fscRemotingError() = (2021, GetStringFunc("fscRemotingError",",,,") )
    /// Problem with filename '%s': Illegal characters in path.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1165)
    static member pathIsInvalid(a0 : System.String) = (2022, GetStringFunc("pathIsInvalid",",,,%s,,,") a0)
    /// Passing a .resx file (%s) as a source file to the compiler is deprecated. Use resgen.exe to transform the .resx file into a .resources file to pass as a --resource option. If you are using MSBuild, this can be done via an <EmbeddedResource> item in the .fsproj project file.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1166)
    static member fscResxSourceFileDeprecated(a0 : System.String) = (2023, GetStringFunc("fscResxSourceFileDeprecated",",,,%s,,,") a0)
    /// Character '%s' is not allowed in provided namespace name '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1170)
    static member etIllegalCharactersInNamespaceName(a0 : System.String, a1 : System.String) = (3000, GetStringFunc("etIllegalCharactersInNamespaceName",",,,%s,,,%s,,,") a0 a1)
    /// The provided type '%s' returned a member with a null or empty member name
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1171)
    static member etNullOrEmptyMemberName(a0 : System.String) = (3001, GetStringFunc("etNullOrEmptyMemberName",",,,%s,,,") a0)
    /// The provided type '%s' returned a null member
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1172)
    static member etNullMember(a0 : System.String) = (3002, GetStringFunc("etNullMember",",,,%s,,,") a0)
    /// The provided type '%s' member info '%s' has null declaring type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1173)
    static member etNullMemberDeclaringType(a0 : System.String, a1 : System.String) = (3003, GetStringFunc("etNullMemberDeclaringType",",,,%s,,,%s,,,") a0 a1)
    /// The provided type '%s' has member '%s' which has declaring type '%s'. Expected declaring type to be the same as provided type.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1174)
    static member etNullMemberDeclaringTypeDifferentFromProvidedType(a0 : System.String, a1 : System.String, a2 : System.String) = (3004, GetStringFunc("etNullMemberDeclaringTypeDifferentFromProvidedType",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// Referenced assembly '%s' has assembly level attribute '%s' but no public type provider classes were found
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1175)
    static member etHostingAssemblyFoundWithoutHosts(a0 : System.String, a1 : System.String) = (3005, GetStringFunc("etHostingAssemblyFoundWithoutHosts",",,,%s,,,%s,,,") a0 a1)
    /// Type '%s' from type provider '%s' has an empty namespace. Use 'null' for the global namespace.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1176)
    static member etEmptyNamespaceOfTypeNotAllowed(a0 : System.String, a1 : System.String) = (3006, GetStringFunc("etEmptyNamespaceOfTypeNotAllowed",",,,%s,,,%s,,,") a0 a1)
    /// Empty namespace found from the type provider '%s'. Use 'null' for the global namespace.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1177)
    static member etEmptyNamespaceNotAllowed(a0 : System.String) = (3007, GetStringFunc("etEmptyNamespaceNotAllowed",",,,%s,,,") a0)
    /// Provided type '%s' has 'IsGenericType' as true, but generic types are not supported.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1178)
    static member etMustNotBeGeneric(a0 : System.String) = (3011, GetStringFunc("etMustNotBeGeneric",",,,%s,,,") a0)
    /// Provided type '%s' has 'IsArray' as true, but array types are not supported.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1179)
    static member etMustNotBeAnArray(a0 : System.String) = (3013, GetStringFunc("etMustNotBeAnArray",",,,%s,,,") a0)
    /// Invalid member '%s' on provided type '%s'. Provided type members must be public, and not be generic, virtual, or abstract.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1180)
    static member etMethodHasRequirements(a0 : System.String, a1 : System.String) = (3014, GetStringFunc("etMethodHasRequirements",",,,%s,,,%s,,,") a0 a1)
    /// Invalid member '%s' on provided type '%s'. Only properties, methods and constructors are allowed
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1181)
    static member etUnsupportedMemberKind(a0 : System.String, a1 : System.String) = (3015, GetStringFunc("etUnsupportedMemberKind",",,,%s,,,%s,,,") a0 a1)
    /// Property '%s' on provided type '%s' has CanRead=true but there was no value from GetGetMethod()
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1182)
    static member etPropertyCanReadButHasNoGetter(a0 : System.String, a1 : System.String) = (3016, GetStringFunc("etPropertyCanReadButHasNoGetter",",,,%s,,,%s,,,") a0 a1)
    /// Property '%s' on provided type '%s' has CanRead=false but GetGetMethod() returned a method
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1183)
    static member etPropertyHasGetterButNoCanRead(a0 : System.String, a1 : System.String) = (3017, GetStringFunc("etPropertyHasGetterButNoCanRead",",,,%s,,,%s,,,") a0 a1)
    /// Property '%s' on provided type '%s' has CanWrite=true but there was no value from GetSetMethod()
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1184)
    static member etPropertyCanWriteButHasNoSetter(a0 : System.String, a1 : System.String) = (3018, GetStringFunc("etPropertyCanWriteButHasNoSetter",",,,%s,,,%s,,,") a0 a1)
    /// Property '%s' on provided type '%s' has CanWrite=false but GetSetMethod() returned a method
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1185)
    static member etPropertyHasSetterButNoCanWrite(a0 : System.String, a1 : System.String) = (3019, GetStringFunc("etPropertyHasSetterButNoCanWrite",",,,%s,,,%s,,,") a0 a1)
    /// One or more errors seen during provided type setup
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1186)
    static member etOneOrMoreErrorsSeenDuringExtensionTypeSetting() = (3020, GetStringFunc("etOneOrMoreErrorsSeenDuringExtensionTypeSetting",",,,") )
    /// Unexpected exception from provided type '%s' member '%s': %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1187)
    static member etUnexpectedExceptionFromProvidedTypeMember(a0 : System.String, a1 : System.String, a2 : System.String) = (3021, GetStringFunc("etUnexpectedExceptionFromProvidedTypeMember",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// Unsupported constant type '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1188)
    static member etUnsupportedConstantType(a0 : System.String) = (3022, GetStringFunc("etUnsupportedConstantType",",,,%s,,,") a0)
    /// Unsupported expression '%s' from type provider. If you are the author of this type provider, consider adjusting it to provide a different provided expression.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1189)
    static member etUnsupportedProvidedExpression(a0 : System.String) = (3025, GetStringFunc("etUnsupportedProvidedExpression",",,,%s,,,") a0)
    /// Expected provided type named '%s' but provided type has 'Name' with value '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1190)
    static member etProvidedTypeHasUnexpectedName(a0 : System.String, a1 : System.String) = (3028, GetStringFunc("etProvidedTypeHasUnexpectedName",",,,%s,,,%s,,,") a0 a1)
    /// Event '%s' on provided type '%s' has no value from GetAddMethod()
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1191)
    static member etEventNoAdd(a0 : System.String, a1 : System.String) = (3029, GetStringFunc("etEventNoAdd",",,,%s,,,%s,,,") a0 a1)
    /// Event '%s' on provided type '%s' has no value from GetRemoveMethod()
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1192)
    static member etEventNoRemove(a0 : System.String, a1 : System.String) = (3030, GetStringFunc("etEventNoRemove",",,,%s,,,%s,,,") a0 a1)
    /// Assembly attribute '%s' refers to a designer assembly '%s' which cannot be loaded or doesn't exist. %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1193)
    static member etProviderHasWrongDesignerAssembly(a0 : System.String, a1 : System.String, a2 : System.String) = (3031, GetStringFunc("etProviderHasWrongDesignerAssembly",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// The type provider does not have a valid constructor. A constructor taking either no arguments or one argument of type 'TypeProviderConfig' was expected.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1194)
    static member etProviderDoesNotHaveValidConstructor() = (3032, GetStringFunc("etProviderDoesNotHaveValidConstructor",",,,") )
    /// The type provider '%s' reported an error: %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1195)
    static member etProviderError(a0 : System.String, a1 : System.String) = (3033, GetStringFunc("etProviderError",",,,%s,,,%s,,,") a0 a1)
    /// The type provider '%s' used an invalid parameter in the ParameterExpression: %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1196)
    static member etIncorrectParameterExpression(a0 : System.String, a1 : System.String) = (3034, GetStringFunc("etIncorrectParameterExpression",",,,%s,,,%s,,,") a0 a1)
    /// The type provider '%s' provided a method with a name '%s' and metadata token '%d', which is not reported among its methods of its declaring type '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1197)
    static member etIncorrectProvidedMethod(a0 : System.String, a1 : System.String, a2 : System.Int32, a3 : System.String) = (3035, GetStringFunc("etIncorrectProvidedMethod",",,,%s,,,%s,,,%d,,,%s,,,") a0 a1 a2 a3)
    /// The type provider '%s' provided a constructor which is not reported among the constructors of its declaring type '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1198)
    static member etIncorrectProvidedConstructor(a0 : System.String, a1 : System.String) = (3036, GetStringFunc("etIncorrectProvidedConstructor",",,,%s,,,%s,,,") a0 a1)
    /// A direct reference to the generated type '%s' is not permitted. Instead, use a type definition, e.g. 'type TypeAlias = <path>'. This indicates that a type provider adds generated types to your assembly.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1199)
    static member etDirectReferenceToGeneratedTypeNotAllowed(a0 : System.String) = (3039, GetStringFunc("etDirectReferenceToGeneratedTypeNotAllowed",",,,%s,,,") a0)
    /// Expected provided type with path '%s' but provided type has path '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1200)
    static member etProvidedTypeHasUnexpectedPath(a0 : System.String, a1 : System.String) = (3041, GetStringFunc("etProvidedTypeHasUnexpectedPath",",,,%s,,,%s,,,") a0 a1)
    /// Unexpected 'null' return value from provided type '%s' member '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1201)
    static member etUnexpectedNullFromProvidedTypeMember(a0 : System.String, a1 : System.String) = (3042, GetStringFunc("etUnexpectedNullFromProvidedTypeMember",",,,%s,,,%s,,,") a0 a1)
    /// Unexpected exception from member '%s' of provided type '%s' member '%s': %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1202)
    static member etUnexpectedExceptionFromProvidedMemberMember(a0 : System.String, a1 : System.String, a2 : System.String, a3 : System.String) = (3043, GetStringFunc("etUnexpectedExceptionFromProvidedMemberMember",",,,%s,,,%s,,,%s,,,%s,,,") a0 a1 a2 a3)
    /// Nested provided types do not take static arguments or generic parameters
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1203)
    static member etNestedProvidedTypesDoNotTakeStaticArgumentsOrGenericParameters() = (3044, GetStringFunc("etNestedProvidedTypesDoNotTakeStaticArgumentsOrGenericParameters",",,,") )
    /// Invalid static argument to provided type. Expected an argument of kind '%s'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1204)
    static member etInvalidStaticArgument(a0 : System.String) = (3045, GetStringFunc("etInvalidStaticArgument",",,,%s,,,") a0)
    /// An error occured applying the static arguments to a provided type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1205)
    static member etErrorApplyingStaticArgumentsToType() = (3046, GetStringFunc("etErrorApplyingStaticArgumentsToType",",,,") )
    /// Unknown static argument kind '%s' when resolving a reference to a provided type '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1206)
    static member etUnknownStaticArgumentKind(a0 : System.String, a1 : System.String) = (3047, GetStringFunc("etUnknownStaticArgumentKind",",,,%s,,,%s,,,") a0 a1)
    /// invalid namespace for provided type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1207)
    static member invalidNamespaceForProvidedType() = (GetStringFunc("invalidNamespaceForProvidedType",",,,") )
    /// invalid full name for provided type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1208)
    static member invalidFullNameForProvidedType() = (GetStringFunc("invalidFullNameForProvidedType",",,,") )
    /// The type provider returned 'null', which is not a valid return value from '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1210)
    static member etProviderReturnedNull(a0 : System.String) = (3051, GetStringFunc("etProviderReturnedNull",",,,%s,,,") a0)
    /// The type provider constructor has thrown an exception: %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1211)
    static member etTypeProviderConstructorException(a0 : System.String) = (3053, GetStringFunc("etTypeProviderConstructorException",",,,%s,,,") a0)
    /// Type provider '%s' returned null from GetInvokerExpression.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1212)
    static member etNullProvidedExpression(a0 : System.String) = (3056, GetStringFunc("etNullProvidedExpression",",,,%s,,,") a0)
    /// The type provider '%s' returned an invalid type from 'ApplyStaticArguments'. A type with name '%s' was expected, but a type with name '%s' was returned.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1213)
    static member etProvidedAppliedTypeHadWrongName(a0 : System.String, a1 : System.String, a2 : System.String) = (3057, GetStringFunc("etProvidedAppliedTypeHadWrongName",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// This type test or downcast will erase the provided type '%s' to the type '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1214)
    static member tcTypeTestLossy(a0 : System.String, a1 : System.String) = (3060, GetStringFunc("tcTypeTestLossy",",,,%s,,,%s,,,") a0 a1)
    /// This downcast will erase the provided type '%s' to the type '%s'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1215)
    static member tcTypeCastErased(a0 : System.String, a1 : System.String) = (3061, GetStringFunc("tcTypeCastErased",",,,%s,,,%s,,,") a0 a1)
    /// This type test with a provided type '%s' is not allowed because this provided type will be erased to '%s' at runtime.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1216)
    static member tcTypeTestErased(a0 : System.String, a1 : System.String) = (3062, GetStringFunc("tcTypeTestErased",",,,%s,,,%s,,,") a0 a1)
    /// Cannot inherit from erased provided type
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1217)
    static member tcCannotInheritFromErasedType() = (3063, GetStringFunc("tcCannotInheritFromErasedType",",,,") )
    /// Assembly '%s' hase TypeProviderAssembly attribute with invalid value '%s'. The value should be a valid assembly name
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1218)
    static member etInvalidTypeProviderAssemblyName(a0 : System.String, a1 : System.String) = (3065, GetStringFunc("etInvalidTypeProviderAssemblyName",",,,%s,,,%s,,,") a0 a1)
    /// Invalid member name. Members may not have name '.ctor' or '.cctor'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1219)
    static member tcInvalidMemberNameCtor() = (3066, GetStringFunc("tcInvalidMemberNameCtor",",,,") )
    /// The function or member '%s' is used in a way that requires further type annotations at its definition to ensure consistency of inferred types. The inferred signature is '%s'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1220)
    static member tcInferredGenericTypeGivesRiseToInconsistency(a0 : System.String, a1 : System.String) = (3068, GetStringFunc("tcInferredGenericTypeGivesRiseToInconsistency",",,,%s,,,%s,,,") a0 a1)
    /// The number of type arguments did not match: '%d' given, '%d' expected. This may be related to a previously reported error.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1221)
    static member tcInvalidTypeArgumentCount(a0 : System.Int32, a1 : System.Int32) = (3069, GetStringFunc("tcInvalidTypeArgumentCount",",,,%d,,,%d,,,") a0 a1)
    /// Cannot override inherited member '%s' because it is sealed
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1222)
    static member tcCannotOverrideSealedMethod(a0 : System.String) = (3070, GetStringFunc("tcCannotOverrideSealedMethod",",,,%s,,,") a0)
    /// The type provider '%s' reported an error in the context of provided type '%s', member '%s'. The error: %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1223)
    static member etProviderErrorWithContext(a0 : System.String, a1 : System.String, a2 : System.String, a3 : System.String) = (3071, GetStringFunc("etProviderErrorWithContext",",,,%s,,,%s,,,%s,,,%s,,,") a0 a1 a2 a3)
    /// An exception occurred when accessing the '%s' of a provided type: %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1224)
    static member etProvidedTypeWithNameException(a0 : System.String, a1 : System.String) = (3072, GetStringFunc("etProvidedTypeWithNameException",",,,%s,,,%s,,,") a0 a1)
    /// The '%s' of a provided type was null or empty.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1225)
    static member etProvidedTypeWithNullOrEmptyName(a0 : System.String) = (3073, GetStringFunc("etProvidedTypeWithNullOrEmptyName",",,,%s,,,") a0)
    /// Character '%s' is not allowed in provided type name '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1226)
    static member etIllegalCharactersInTypeName(a0 : System.String, a1 : System.String) = (3075, GetStringFunc("etIllegalCharactersInTypeName",",,,%s,,,%s,,,") a0 a1)
    /// In queries, '%s' must use a simple pattern
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1227)
    static member tcJoinMustUseSimplePattern(a0 : System.String) = (3077, GetStringFunc("tcJoinMustUseSimplePattern",",,,%s,,,") a0)
    /// A custom query operation for '%s' is required but not specified
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1228)
    static member tcMissingCustomOperation(a0 : System.String) = (3078, GetStringFunc("tcMissingCustomOperation",",,,%s,,,") a0)
    /// Named static arguments must come after all unnamed static arguments
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1229)
    static member etBadUnnamedStaticArgs() = (3080, GetStringFunc("etBadUnnamedStaticArgs",",,,") )
    /// The static parameter '%s' of the provided type '%s' requires a value. Static parameters to type providers may be optionally specified using named arguments, e.g. '%s<%s=...>'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1230)
    static member etStaticParameterRequiresAValue(a0 : System.String, a1 : System.String, a2 : System.String, a3 : System.String) = (3081, GetStringFunc("etStaticParameterRequiresAValue",",,,%s,,,%s,,,%s,,,%s,,,") a0 a1 a2 a3)
    /// No static parameter exists with name '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1231)
    static member etNoStaticParameterWithName(a0 : System.String) = (3082, GetStringFunc("etNoStaticParameterWithName",",,,%s,,,") a0)
    /// The static parameter '%s' has already been given a value
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1232)
    static member etStaticParameterAlreadyHasValue(a0 : System.String) = (3083, GetStringFunc("etStaticParameterAlreadyHasValue",",,,%s,,,") a0)
    /// Multiple static parameters exist with name '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1233)
    static member etMultipleStaticParameterWithName(a0 : System.String) = (3084, GetStringFunc("etMultipleStaticParameterWithName",",,,%s,,,") a0)
    /// A custom operation may not be used in conjunction with a non-value or recursive 'let' binding in another part of this computation expression
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1234)
    static member tcCustomOperationMayNotBeUsedInConjunctionWithNonSimpleLetBindings() = (3085, GetStringFunc("tcCustomOperationMayNotBeUsedInConjunctionWithNonSimpleLetBindings",",,,") )
    /// A custom operation may not be used in conjunction with 'use', 'try/with', 'try/finally', 'if/then/else' or 'match' operators within this computation expression
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1235)
    static member tcCustomOperationMayNotBeUsedHere() = (3086, GetStringFunc("tcCustomOperationMayNotBeUsedHere",",,,") )
    /// The custom operation '%s' refers to a method which is overloaded. The implementations of custom operations may not be overloaded.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1236)
    static member tcCustomOperationMayNotBeOverloaded(a0 : System.String) = (3087, GetStringFunc("tcCustomOperationMayNotBeOverloaded",",,,%s,,,") a0)
    /// A try/finally expression may not be used within a computation expression with uses of custom operators.  Consider using a sequence expression instead.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1237)
    static member tcTryFinallyMayNotBeUsedWithCustomOperators() = (3088, GetStringFunc("tcTryFinallyMayNotBeUsedWithCustomOperators",",,,") )
    /// A try/with expression may not be used within a within a computation expression with uses of custom operators. Consider using a sequence expression instead.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1238)
    static member tcTryWithMayNotBeUsedWithCustomOperators() = (3089, GetStringFunc("tcTryWithMayNotBeUsedWithCustomOperators",",,,") )
    /// An if/then/else expression may not be used within a computation expression with uses of custom operators. Consider using either an if/then expression, or use a sequence expression instead.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1239)
    static member tcIfThenElseMayNotBeUsedWithCustomOperators() = (3090, GetStringFunc("tcIfThenElseMayNotBeUsedWithCustomOperators",",,,") )
    /// Invalid argument to 'methodhandleof' during codegen
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1240)
    static member ilxgenUnexpectedArgumentToMethodHandleOfDuringCodegen() = (3091, GetStringFunc("ilxgenUnexpectedArgumentToMethodHandleOfDuringCodegen",",,,") )
    /// A reference to a provided type was missing a value for the static parameter '%s'. You may need to recompile one or more referenced assemblies.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1241)
    static member etProvidedTypeReferenceMissingArgument(a0 : System.String) = (3092, GetStringFunc("etProvidedTypeReferenceMissingArgument",",,,%s,,,") a0)
    /// A reference to a provided type had an invalid value '%s' for a static parameter. You may need to recompile one or more referenced assemblies.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1242)
    static member etProvidedTypeReferenceInvalidText(a0 : System.String) = (3093, GetStringFunc("etProvidedTypeReferenceInvalidText",",,,%s,,,") a0)
    /// '%s' is not used correctly. This is a custom operation in this query or computation expression.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1243)
    static member tcCustomOperationNotUsedCorrectly(a0 : System.String) = (3095, GetStringFunc("tcCustomOperationNotUsedCorrectly",",,,%s,,,") a0)
    /// '%s' is not used correctly. Usage: %s. This is a custom operation in this query or computation expression.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1244)
    static member tcCustomOperationNotUsedCorrectly2(a0 : System.String, a1 : System.String) = (3095, GetStringFunc("tcCustomOperationNotUsedCorrectly2",",,,%s,,,%s,,,") a0 a1)
    /// %s var in collection %s (outerKey = innerKey). Note that parentheses are required after '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1245)
    static member customOperationTextLikeJoin(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("customOperationTextLikeJoin",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// %s var in collection %s (outerKey = innerKey) into group. Note that parentheses are required after '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1246)
    static member customOperationTextLikeGroupJoin(a0 : System.String, a1 : System.String, a2 : System.String) = (GetStringFunc("customOperationTextLikeGroupJoin",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// %s collection into var
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1247)
    static member customOperationTextLikeZip(a0 : System.String) = (GetStringFunc("customOperationTextLikeZip",",,,%s,,,") a0)
    /// '%s' must be followed by a variable name. Usage: %s.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1248)
    static member tcBinaryOperatorRequiresVariable(a0 : System.String, a1 : System.String) = (3096, GetStringFunc("tcBinaryOperatorRequiresVariable",",,,%s,,,%s,,,") a0 a1)
    /// Incorrect syntax for '%s'. Usage: %s.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1249)
    static member tcOperatorIncorrectSyntax(a0 : System.String, a1 : System.String) = (3097, GetStringFunc("tcOperatorIncorrectSyntax",",,,%s,,,%s,,,") a0 a1)
    /// '%s' must come after a 'for' selection clause and be followed by the rest of the query. Syntax: ... %s ...
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1250)
    static member tcBinaryOperatorRequiresBody(a0 : System.String, a1 : System.String) = (3098, GetStringFunc("tcBinaryOperatorRequiresBody",",,,%s,,,%s,,,") a0 a1)
    /// '%s' is used with an incorrect number of arguments. This is a custom operation in this query or computation expression. Expected %d argument(s), but given %d.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1251)
    static member tcCustomOperationHasIncorrectArgCount(a0 : System.String, a1 : System.Int32, a2 : System.Int32) = (3099, GetStringFunc("tcCustomOperationHasIncorrectArgCount",",,,%s,,,%d,,,%d,,,") a0 a1 a2)
    /// Expected an expression after this point
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1252)
    static member parsExpectedExpressionAfterToken() = (3100, GetStringFunc("parsExpectedExpressionAfterToken",",,,") )
    /// Expected a type after this point
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1253)
    static member parsExpectedTypeAfterToken() = (3101, GetStringFunc("parsExpectedTypeAfterToken",",,,") )
    /// Unmatched '[<'. Expected closing '>]'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1254)
    static member parsUnmatchedLBrackLess() = (3102, GetStringFunc("parsUnmatchedLBrackLess",",,,") )
    /// Unexpected end of input in 'match' expression. Expected 'match <expr> with | <pat> -> <expr> | <pat> -> <expr> ...'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1255)
    static member parsUnexpectedEndOfFileMatch() = (3103, GetStringFunc("parsUnexpectedEndOfFileMatch",",,,") )
    /// Unexpected end of input in 'try' expression. Expected 'try <expr> with <rules>' or 'try <expr> finally <expr>'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1256)
    static member parsUnexpectedEndOfFileTry() = (3104, GetStringFunc("parsUnexpectedEndOfFileTry",",,,") )
    /// Unexpected end of input in 'while' expression. Expected 'while <expr> do <expr>'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1257)
    static member parsUnexpectedEndOfFileWhile() = (3105, GetStringFunc("parsUnexpectedEndOfFileWhile",",,,") )
    /// Unexpected end of input in 'for' expression. Expected 'for <pat> in <expr> do <expr>'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1258)
    static member parsUnexpectedEndOfFileFor() = (3106, GetStringFunc("parsUnexpectedEndOfFileFor",",,,") )
    /// Unexpected end of input in 'match' or 'try' expression
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1259)
    static member parsUnexpectedEndOfFileWith() = (3107, GetStringFunc("parsUnexpectedEndOfFileWith",",,,") )
    /// Unexpected end of input in 'then' branch of conditional expression. Expected 'if <expr> then <expr>' or 'if <expr> then <expr> else <expr>'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1260)
    static member parsUnexpectedEndOfFileThen() = (3108, GetStringFunc("parsUnexpectedEndOfFileThen",",,,") )
    /// Unexpected end of input in 'else' branch of conditional expression. Expected 'if <expr> then <expr>' or 'if <expr> then <expr> else <expr>'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1261)
    static member parsUnexpectedEndOfFileElse() = (3109, GetStringFunc("parsUnexpectedEndOfFileElse",",,,") )
    /// Unexpected end of input in body of lambda expression. Expected 'fun <pat> ... <pat> -> <expr>'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1262)
    static member parsUnexpectedEndOfFileFunBody() = (3110, GetStringFunc("parsUnexpectedEndOfFileFunBody",",,,") )
    /// Unexpected end of input in type arguments
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1263)
    static member parsUnexpectedEndOfFileTypeArgs() = (3111, GetStringFunc("parsUnexpectedEndOfFileTypeArgs",",,,") )
    /// Unexpected end of input in type signature
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1264)
    static member parsUnexpectedEndOfFileTypeSignature() = (3112, GetStringFunc("parsUnexpectedEndOfFileTypeSignature",",,,") )
    /// Unexpected end of input in type definition
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1265)
    static member parsUnexpectedEndOfFileTypeDefinition() = (3113, GetStringFunc("parsUnexpectedEndOfFileTypeDefinition",",,,") )
    /// Unexpected end of input in object members
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1266)
    static member parsUnexpectedEndOfFileObjectMembers() = (3114, GetStringFunc("parsUnexpectedEndOfFileObjectMembers",",,,") )
    /// Unexpected end of input in value, function or member definition
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1267)
    static member parsUnexpectedEndOfFileDefinition() = (3115, GetStringFunc("parsUnexpectedEndOfFileDefinition",",,,") )
    /// Unexpected end of input in expression
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1268)
    static member parsUnexpectedEndOfFileExpression() = (3116, GetStringFunc("parsUnexpectedEndOfFileExpression",",,,") )
    /// Unexpected end of type. Expected a name after this point.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1269)
    static member parsExpectedNameAfterToken() = (3117, GetStringFunc("parsExpectedNameAfterToken",",,,") )
    /// Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1270)
    static member parsUnmatchedLet() = (3118, GetStringFunc("parsUnmatchedLet",",,,") )
    /// Incomplete value definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let!' keyword.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1271)
    static member parsUnmatchedLetBang() = (3119, GetStringFunc("parsUnmatchedLetBang",",,,") )
    /// Incomplete value definition. If this is in an expression, the body of the expression must be indented to the same column as the 'use!' keyword.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1272)
    static member parsUnmatchedUseBang() = (3120, GetStringFunc("parsUnmatchedUseBang",",,,") )
    /// Incomplete value definition. If this is in an expression, the body of the expression must be indented to the same column as the 'use' keyword.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1273)
    static member parsUnmatchedUse() = (3121, GetStringFunc("parsUnmatchedUse",",,,") )
    /// Missing 'do' in 'while' expression. Expected 'while <expr> do <expr>'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1274)
    static member parsWhileDoExpected() = (3122, GetStringFunc("parsWhileDoExpected",",,,") )
    /// Missing 'do' in 'for' expression. Expected 'for <pat> in <expr> do <expr>'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1275)
    static member parsForDoExpected() = (3123, GetStringFunc("parsForDoExpected",",,,") )
    /// Invalid join relation in '%s'. Expected 'expr <op> expr', where <op> is =, =?, ?= or ?=?.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1276)
    static member tcInvalidRelationInJoin(a0 : System.String) = (3125, GetStringFunc("tcInvalidRelationInJoin",",,,%s,,,") a0)
    /// Calls
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1277)
    static member typeInfoCallsWord() = (GetStringFunc("typeInfoCallsWord",",,,") )
    /// Invalid number of generic arguments to type '%s' in provided type. Expected '%d' arguments, given '%d'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1278)
    static member impInvalidNumberOfGenericArguments(a0 : System.String, a1 : System.Int32, a2 : System.Int32) = (3126, GetStringFunc("impInvalidNumberOfGenericArguments",",,,%s,,,%d,,,%d,,,") a0 a1 a2)
    /// Invalid value '%s' for unit-of-measure parameter '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1279)
    static member impInvalidMeasureArgument1(a0 : System.String, a1 : System.String) = (3127, GetStringFunc("impInvalidMeasureArgument1",",,,%s,,,%s,,,") a0 a1)
    /// Invalid value unit-of-measure parameter '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1280)
    static member impInvalidMeasureArgument2(a0 : System.String) = (3127, GetStringFunc("impInvalidMeasureArgument2",",,,%s,,,") a0)
    /// Property '%s' on provided type '%s' is neither readable nor writable as it has CanRead=false and CanWrite=false
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1281)
    static member etPropertyNeedsCanWriteOrCanRead(a0 : System.String, a1 : System.String) = (3128, GetStringFunc("etPropertyNeedsCanWriteOrCanRead",",,,%s,,,%s,,,") a0 a1)
    /// A use of 'into' must be followed by the remainder of the computation
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1282)
    static member tcIntoNeedsRestOfQuery() = (3129, GetStringFunc("tcIntoNeedsRestOfQuery",",,,") )
    /// The operator '%s' does not accept the use of 'into'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1283)
    static member tcOperatorDoesntAcceptInto(a0 : System.String) = (3130, GetStringFunc("tcOperatorDoesntAcceptInto",",,,%s,,,") a0)
    /// The definition of the custom operator '%s' does not use a valid combination of attribute flags
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1284)
    static member tcCustomOperationInvalid(a0 : System.String) = (3131, GetStringFunc("tcCustomOperationInvalid",",,,%s,,,") a0)
    /// This type definition may not have the 'CLIMutable' attribute. Only record types may have this attribute.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1285)
    static member tcThisTypeMayNotHaveACLIMutableAttribute() = (3132, GetStringFunc("tcThisTypeMayNotHaveACLIMutableAttribute",",,,") )
    /// 'member val' definitions are only permitted in types with a primary constructor. Consider adding arguments to your type definition, e.g. 'type X(args) = ...'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1286)
    static member tcAutoPropertyRequiresImplicitConstructionSequence() = (3133, GetStringFunc("tcAutoPropertyRequiresImplicitConstructionSequence",",,,") )
    /// Property definitions may not be declared mutable. To indicate that this property can be set, use 'member val PropertyName = expr with get,set'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1287)
    static member parsMutableOnAutoPropertyShouldBeGetSet() = (3134, GetStringFunc("parsMutableOnAutoPropertyShouldBeGetSet",",,,") )
    /// To indicate that this property can be set, use 'member val PropertyName = expr with get,set'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1288)
    static member parsMutableOnAutoPropertyShouldBeGetSetNotJustSet() = (3135, GetStringFunc("parsMutableOnAutoPropertyShouldBeGetSetNotJustSet",",,,") )
    /// Type '%s' is illegal because in byref<T>, T cannot contain byref types.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1289)
    static member chkNoByrefsOfByrefs(a0 : System.String) = (3136, GetStringFunc("chkNoByrefsOfByrefs",",,,%s,,,") a0)
    /// Type provider assembly '%s' is not trusted and will not be loaded for security reasons. This may cause subsequent build errors. See the 'F# Tools' section of Visual Studio options for more information.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1290)
    static member etTypeProviderNotApproved(a0 : System.String) = (3137, GetStringFunc("etTypeProviderNotApproved",",,,%s,,,") a0)
    /// F# supports a maximum array rank of 4
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1291)
    static member tastopsMaxArrayFour() = (3138, GetStringFunc("tastopsMaxArrayFour",",,,") )
    /// In queries, use the form 'for x in n .. m do ...' for ranging over integers
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1292)
    static member tcNoIntegerForLoopInQuery() = (3139, GetStringFunc("tcNoIntegerForLoopInQuery",",,,") )
    /// 'while' expressions may not be used in queries
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1293)
    static member tcNoWhileInQuery() = (3140, GetStringFunc("tcNoWhileInQuery",",,,") )
    /// 'try/finally' expressions may not be used in queries
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1294)
    static member tcNoTryFinallyInQuery() = (3141, GetStringFunc("tcNoTryFinallyInQuery",",,,") )
    /// 'use' expressions may not be used in queries
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1295)
    static member tcUseMayNotBeUsedInQueries() = (3142, GetStringFunc("tcUseMayNotBeUsedInQueries",",,,") )
    /// 'let!', 'use!' and 'do!' expressions may not be used in queries
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1296)
    static member tcBindMayNotBeUsedInQueries() = (3143, GetStringFunc("tcBindMayNotBeUsedInQueries",",,,") )
    /// 'return' and 'return!' may not be used in queries
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1297)
    static member tcReturnMayNotBeUsedInQueries() = (3144, GetStringFunc("tcReturnMayNotBeUsedInQueries",",,,") )
    /// This is not a known query operator. Query operators are identifiers such as 'select', 'where', 'sortBy', 'thenBy', 'groupBy', 'groupValBy', 'join', 'groupJoin', 'sumBy' and 'averageBy', defined using corresponding methods on the 'QueryBuilder' type.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1298)
    static member tcUnrecognizedQueryOperator() = (3145, GetStringFunc("tcUnrecognizedQueryOperator",",,,") )
    /// 'try/with' expressions may not be used in queries
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1299)
    static member tcTryWithMayNotBeUsedInQueries() = (3146, GetStringFunc("tcTryWithMayNotBeUsedInQueries",",,,") )
    /// This 'let' definition may not be used in a query. Only simple value definitions may be used in queries.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1300)
    static member tcNonSimpleLetBindingInQuery() = (3147, GetStringFunc("tcNonSimpleLetBindingInQuery",",,,") )
    /// Too many static parameters. Expected at most %d parameters, but got %d unnamed and %d named parameters.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1301)
    static member etTooManyStaticParameters(a0 : System.Int32, a1 : System.Int32, a2 : System.Int32) = (3148, GetStringFunc("etTooManyStaticParameters",",,,%d,,,%d,,,%d,,,") a0 a1 a2)
    /// Invalid provided literal value '%s'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1302)
    static member infosInvalidProvidedLiteralValue(a0 : System.String) = (3149, GetStringFunc("infosInvalidProvidedLiteralValue",",,,%s,,,") a0)
    /// The 'anycpu32bitpreferred' platform can only be used with EXE targets. You must use 'anycpu' instead.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1303)
    static member invalidPlatformTarget() = (3150, GetStringFunc("invalidPlatformTarget",",,,") )
    /// This member, function or value declaration may not be declared 'inline'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1304)
    static member tcThisValueMayNotBeInlined() = (3151, GetStringFunc("tcThisValueMayNotBeInlined",",,,") )
    /// The provider '%s' returned a non-generated type '%s' in the context of a set of generated types. Consider adjusting the type provider to only return generated types.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1305)
    static member etErasedTypeUsedInGeneration(a0 : System.String, a1 : System.String) = (3152, GetStringFunc("etErasedTypeUsedInGeneration",",,,%s,,,%s,,,") a0 a1)
    /// Arguments to query operators may require parentheses, e.g. 'where (x > y)' or 'groupBy (x.Length / 10)'
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1306)
    static member tcUnrecognizedQueryBinaryOperator() = (3153, GetStringFunc("tcUnrecognizedQueryBinaryOperator",",,,") )
    /// The 'anycpu32bitpreferred' platform flag may only be used with .NET Framework versions 4.5 and greater.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1307)
    static member invalidPlatformTargetForOldFramework() = (3154, GetStringFunc("invalidPlatformTargetForOldFramework",",,,") )
    /// A quotation may not involve an assignment to or taking the address of a captured local variable
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1308)
    static member crefNoSetOfHole() = (3155, GetStringFunc("crefNoSetOfHole",",,,") )
    /// + 1 overload
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1309)
    static member nicePrintOtherOverloads1() = (GetStringFunc("nicePrintOtherOverloads1",",,,") )
    /// + %d overloads
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1310)
    static member nicePrintOtherOverloadsN(a0 : System.Int32) = (GetStringFunc("nicePrintOtherOverloadsN",",,,%d,,,") a0)
    /// Erased to
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1311)
    static member erasedTo() = (GetStringFunc("erasedTo",",,,") )
    /// Unexpected token '%s' or incomplete expression
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1312)
    static member parsUnfinishedExpression(a0 : System.String) = (3156, GetStringFunc("parsUnfinishedExpression",",,,%s,,,") a0)
    /// Quotations cannot contain byref types
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1313)
    static member crefQuotationsCantContainByrefTypes() = (3157, GetStringFunc("crefQuotationsCantContainByrefTypes",",,,") )
    /// Cannot find code target for this attribute, possibly because the code after the attribute is incomplete.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1314)
    static member parsAttributeOnIncompleteCode() = (3158, GetStringFunc("parsAttributeOnIncompleteCode",",,,") )
    /// Type name cannot be empty.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1315)
    static member parsTypeNameCannotBeEmpty() = (3159, GetStringFunc("parsTypeNameCannotBeEmpty",",,,") )
    /// Problem reading assembly '%s': %s
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1316)
    static member buildProblemReadingAssembly(a0 : System.String, a1 : System.String) = (3160, GetStringFunc("buildProblemReadingAssembly",",,,%s,,,%s,,,") a0 a1)
    /// Invalid provided field. Provided fields of erased provided types must be literals.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1317)
    static member tcTPFieldMustBeLiteral() = (3161, GetStringFunc("tcTPFieldMustBeLiteral",",,,") )
    /// (loading description...)
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1318)
    static member loadingDescription() = (GetStringFunc("loadingDescription",",,,") )
    /// (description unavailable...)
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1319)
    static member descriptionUnavailable() = (GetStringFunc("descriptionUnavailable",",,,") )
    /// A type variable has been constrained by multiple different class types. A type variable may only have one class constraint.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1320)
    static member chkTyparMultipleClassConstraints() = (3162, GetStringFunc("chkTyparMultipleClassConstraints",",,,") )
    /// 'match' expressions may not be used in queries
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1321)
    static member tcMatchMayNotBeUsedWithQuery() = (3163, GetStringFunc("tcMatchMayNotBeUsedWithQuery",",,,") )
    /// Infix operator member '%s' has %d initial argument(s). Expected a tuple of 3 arguments
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1322)
    static member memberOperatorDefinitionWithNonTripleArgument(a0 : System.String, a1 : System.Int32) = (3164, GetStringFunc("memberOperatorDefinitionWithNonTripleArgument",",,,%s,,,%d,,,") a0 a1)
    /// The operator '%s' cannot be resolved. Consider opening the module 'Microsoft.FSharp.Linq.NullableOperators'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1323)
    static member cannotResolveNullableOperators(a0 : System.String) = (3165, GetStringFunc("cannotResolveNullableOperators",",,,%s,,,") a0)
    /// '%s' must be followed by 'in'. Usage: %s.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1324)
    static member tcOperatorRequiresIn(a0 : System.String, a1 : System.String) = (3167, GetStringFunc("tcOperatorRequiresIn",",,,%s,,,%s,,,") a0 a1)
    /// Neither 'member val' nor 'override val' definitions are permitted in object expressions.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1325)
    static member parsIllegalMemberVarInObjectImplementation() = (3168, GetStringFunc("parsIllegalMemberVarInObjectImplementation",",,,") )
    /// Copy-and-update record expressions must include at least one field.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1326)
    static member tcEmptyCopyAndUpdateRecordInvalid() = (3169, GetStringFunc("tcEmptyCopyAndUpdateRecordInvalid",",,,") )
    /// '_' cannot be used as field name
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1327)
    static member parsUnderscoreInvalidFieldName() = (3170, GetStringFunc("parsUnderscoreInvalidFieldName",",,,") )
    /// The provided types generated by this use of a type provider may not be used from other F# assemblies and should be marked internal or private. Consider using 'type internal TypeName = ...' or 'type private TypeName = ...'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1328)
    static member tcGeneratedTypesShouldBeInternalOrPrivate() = (3171, GetStringFunc("tcGeneratedTypesShouldBeInternalOrPrivate",",,,") )
    /// A property's getter and setter must have the same type. Property '%s' has getter of type '%s' but setter of type '%s'.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1329)
    static member chkGetterAndSetterHaveSamePropertyType(a0 : System.String, a1 : System.String, a2 : System.String) = (3172, GetStringFunc("chkGetterAndSetterHaveSamePropertyType",",,,%s,,,%s,,,%s,,,") a0 a1 a2)
    /// Array method '%s' is supplied by the runtime and cannot be directly used in code. For operations with array elements consider using family of GetArray/SetArray functions from LanguagePrimitives.IntrinsicFunctions module.
    /// (Originally from C:\code\clrish\src\fsharp\FSComp.txt:1330)
    static member tcRuntimeSuppliedMethodCannotBeUsedInUserCode(a0 : System.String) = (3173, GetStringFunc("tcRuntimeSuppliedMethodCannotBeUsedInUserCode",",,,%s,,,") a0)

    /// Call this method once to validate that all known resources are valid; throws if not
    static member RunStartupValidation() =
        ignore(GetString("undefinedNameNamespace"))
        ignore(GetString("undefinedNameNamespaceOrModule"))
        ignore(GetString("undefinedNameFieldConstructorOrMember"))
        ignore(GetString("undefinedNameValueConstructorNamespaceOrType"))
        ignore(GetString("undefinedNameValueOfConstructor"))
        ignore(GetString("undefinedNameValueNamespaceTypeOrModule"))
        ignore(GetString("undefinedNameConstructorModuleOrNamespace"))
        ignore(GetString("undefinedNameType"))
        ignore(GetString("undefinedNameRecordLabelOrNamespace"))
        ignore(GetString("undefinedNameRecordLabel"))
        ignore(GetString("undefinedNameTypeParameter"))
        ignore(GetString("undefinedNamePatternDiscriminator"))
        ignore(GetString("buildUnexpectedTypeArgs"))
        ignore(GetString("buildInvalidWarningNumber"))
        ignore(GetString("buildInvalidVersionString"))
        ignore(GetString("buildInvalidVersionFile"))
        ignore(GetString("buildProductName"))
        ignore(GetString("buildProblemWithFilename"))
        ignore(GetString("buildNoInputsSpecified"))
        ignore(GetString("buildMismatchOutputExtension"))
        ignore(GetString("buildPdbRequiresDebug"))
        ignore(GetString("buildInvalidSearchDirectory"))
        ignore(GetString("buildSearchDirectoryNotFound"))
        ignore(GetString("buildInvalidFilename"))
        ignore(GetString("buildInvalidAssemblyName"))
        ignore(GetString("buildInvalidPrivacy"))
        ignore(GetString("buildMultipleReferencesNotAllowed"))
        ignore(GetString("buildRequiresCLI2"))
        ignore(GetString("buildCouldNotReadVersionInfoFromMscorlib"))
        ignore(GetString("buildMscorlibAndReferencedAssemblyMismatch"))
        ignore(GetString("buildCannotReadAssembly"))
        ignore(GetString("buildMscorLibAndFSharpCoreMismatch"))
        ignore(GetString("buildAssemblyResolutionFailed"))
        ignore(GetString("buildImplicitModuleIsNotLegalIdentifier"))
        ignore(GetString("buildMultiFileRequiresNamespaceOrModule"))
        ignore(GetString("buildMultipleToplevelModules"))
        ignore(GetString("buildUnknownFileSuffix"))
        ignore(GetString("buildOptionRequiresParameter"))
        ignore(GetString("buildCouldNotFindSourceFile"))
        ignore(GetString("buildInvalidSourceFileExtension"))
        ignore(GetString("buildCouldNotResolveAssembly"))
        ignore(GetString("buildCouldNotResolveAssemblyRequiredByFile"))
        ignore(GetString("buildErrorOpeningBinaryFile"))
        ignore(GetString("buildDifferentVersionMustRecompile"))
        ignore(GetString("buildInvalidHashIDirective"))
        ignore(GetString("buildInvalidHashrDirective"))
        ignore(GetString("buildInvalidHashloadDirective"))
        ignore(GetString("buildInvalidHashtimeDirective"))
        ignore(GetString("buildDirectivesInModulesAreIgnored"))
        ignore(GetString("buildSignatureAlreadySpecified"))
        ignore(GetString("buildImplementationAlreadyGivenDetail"))
        ignore(GetString("buildImplementationAlreadyGiven"))
        ignore(GetString("buildSignatureWithoutImplementation"))
        ignore(GetString("buildArgInvalidInt"))
        ignore(GetString("buildArgInvalidFloat"))
        ignore(GetString("buildUnrecognizedOption"))
        ignore(GetString("buildInvalidModuleOrNamespaceName"))
        ignore(GetString("pickleErrorReadingWritingMetadata"))
        ignore(GetString("tastTypeOrModuleNotConcrete"))
        ignore(GetString("tastTypeHasAssemblyCodeRepresentation"))
        ignore(GetString("tastNamespaceAndModuleWithSameNameInAssembly"))
        ignore(GetString("tastTwoModulesWithSameNameInAssembly"))
        ignore(GetString("tastDuplicateTypeDefinitionInAssembly"))
        ignore(GetString("tastConflictingModuleAndTypeDefinitionInAssembly"))
        ignore(GetString("tastInvalidMemberSignature"))
        ignore(GetString("tastValueDoesNotHaveSetterType"))
        ignore(GetString("tastInvalidFormForPropertyGetter"))
        ignore(GetString("tastInvalidFormForPropertySetter"))
        ignore(GetString("tastUnexpectedByRef"))
        ignore(GetString("tastValueMustBeLocalAndMutable"))
        ignore(GetString("tastInvalidMutationOfConstant"))
        ignore(GetString("tastValueHasBeenCopied"))
        ignore(GetString("tastRecursiveValuesMayNotBeInConstructionOfTuple"))
        ignore(GetString("tastRecursiveValuesMayNotAppearInConstructionOfType"))
        ignore(GetString("tastRecursiveValuesMayNotBeAssignedToNonMutableField"))
        ignore(GetString("tastUnexpectedDecodeOfAutoOpenAttribute"))
        ignore(GetString("tastUnexpectedDecodeOfInternalsVisibleToAttribute"))
        ignore(GetString("tastUnexpectedDecodeOfInterfaceDataVersionAttribute"))
        ignore(GetString("tastActivePatternsLimitedToSeven"))
        ignore(GetString("tastConstantCannotBeCustomAttribute"))
        ignore(GetString("tastNotAConstantExpression"))
        ignore(GetString("ValueNotContainedMutabilityAttributesDiffer"))
        ignore(GetString("ValueNotContainedMutabilityNamesDiffer"))
        ignore(GetString("ValueNotContainedMutabilityCompiledNamesDiffer"))
        ignore(GetString("ValueNotContainedMutabilityDisplayNamesDiffer"))
        ignore(GetString("ValueNotContainedMutabilityAccessibilityMore"))
        ignore(GetString("ValueNotContainedMutabilityInlineFlagsDiffer"))
        ignore(GetString("ValueNotContainedMutabilityLiteralConstantValuesDiffer"))
        ignore(GetString("ValueNotContainedMutabilityOneIsTypeFunction"))
        ignore(GetString("ValueNotContainedMutabilityParameterCountsDiffer"))
        ignore(GetString("ValueNotContainedMutabilityTypesDiffer"))
        ignore(GetString("ValueNotContainedMutabilityExtensionsDiffer"))
        ignore(GetString("ValueNotContainedMutabilityArityNotInferred"))
        ignore(GetString("ValueNotContainedMutabilityGenericParametersDiffer"))
        ignore(GetString("ValueNotContainedMutabilityGenericParametersAreDifferentKinds"))
        ignore(GetString("ValueNotContainedMutabilityAritiesDiffer"))
        ignore(GetString("ValueNotContainedMutabilityDotNetNamesDiffer"))
        ignore(GetString("ValueNotContainedMutabilityStaticsDiffer"))
        ignore(GetString("ValueNotContainedMutabilityVirtualsDiffer"))
        ignore(GetString("ValueNotContainedMutabilityAbstractsDiffer"))
        ignore(GetString("ValueNotContainedMutabilityFinalsDiffer"))
        ignore(GetString("ValueNotContainedMutabilityOverridesDiffer"))
        ignore(GetString("ValueNotContainedMutabilityOneIsConstructor"))
        ignore(GetString("ValueNotContainedMutabilityStaticButInstance"))
        ignore(GetString("ValueNotContainedMutabilityInstanceButStatic"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleNamesDiffer"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleParameterCountsDiffer"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleAccessibilityDiffer"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleMissingInterface"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleImplementationSaysNull"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleImplementationSaysNull2"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleSignatureSaysNull"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleSignatureSaysNull2"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleImplementationSealed"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleImplementationIsNotSealed"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleImplementationIsAbstract"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleSignatureIsAbstract"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleTypesHaveDifferentBaseTypes"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleNumbersDiffer"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleSignatureDefinesButImplDoesNot"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleImplDefinesButSignatureDoesNot"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleImplDefinesStruct"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleDotNetTypeRepresentationIsHidden"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleTypeIsHidden"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleTypeIsDifferentKind"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleILDiffer"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleRepresentationsDiffer"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleFieldWasPresent"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleFieldOrderDiffer"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleFieldRequiredButNotSpecified"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleFieldIsInImplButNotSig"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleAbstractMemberMissingInImpl"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleAbstractMemberMissingInSig"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleSignatureDeclaresDiffer"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleAbbreviationsDiffer"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleAbbreviationHiddenBySig"))
        ignore(GetString("DefinitionsInSigAndImplNotCompatibleSigHasAbbreviation"))
        ignore(GetString("ModuleContainsConstructorButNamesDiffer"))
        ignore(GetString("ModuleContainsConstructorButDataFieldsDiffer"))
        ignore(GetString("ModuleContainsConstructorButTypesOfFieldsDiffer"))
        ignore(GetString("ModuleContainsConstructorButAccessibilityDiffers"))
        ignore(GetString("FieldNotContainedNamesDiffer"))
        ignore(GetString("FieldNotContainedAccessibilitiesDiffer"))
        ignore(GetString("FieldNotContainedStaticsDiffer"))
        ignore(GetString("FieldNotContainedMutablesDiffer"))
        ignore(GetString("FieldNotContainedLiteralsDiffer"))
        ignore(GetString("FieldNotContainedTypesDiffer"))
        ignore(GetString("typrelCannotResolveImplicitGenericInstantiation"))
        ignore(GetString("typrelCannotResolveAmbiguityInOverloadedOperator"))
        ignore(GetString("typrelCannotResolveAmbiguityInPrintf"))
        ignore(GetString("typrelCannotResolveAmbiguityInEnum"))
        ignore(GetString("typrelCannotResolveAmbiguityInDelegate"))
        ignore(GetString("typrelInvalidValue"))
        ignore(GetString("typrelSigImplNotCompatibleParamCountsDiffer"))
        ignore(GetString("typrelSigImplNotCompatibleCompileTimeRequirementsDiffer"))
        ignore(GetString("typrelSigImplNotCompatibleConstraintsDiffer"))
        ignore(GetString("typrelSigImplNotCompatibleConstraintsDifferRemove"))
        ignore(GetString("typrelTypeImplementsIComparableShouldOverrideObjectEquals"))
        ignore(GetString("typrelTypeImplementsIComparableDefaultObjectEqualsProvided"))
        ignore(GetString("typrelExplicitImplementationOfGetHashCodeOrEquals"))
        ignore(GetString("typrelExplicitImplementationOfGetHashCode"))
        ignore(GetString("typrelExplicitImplementationOfEquals"))
        ignore(GetString("ExceptionDefsNotCompatibleHiddenBySignature"))
        ignore(GetString("ExceptionDefsNotCompatibleDotNetRepresentationsDiffer"))
        ignore(GetString("ExceptionDefsNotCompatibleAbbreviationHiddenBySignature"))
        ignore(GetString("ExceptionDefsNotCompatibleSignaturesDiffer"))
        ignore(GetString("ExceptionDefsNotCompatibleExceptionDeclarationsDiffer"))
        ignore(GetString("ExceptionDefsNotCompatibleFieldInSigButNotImpl"))
        ignore(GetString("ExceptionDefsNotCompatibleFieldInImplButNotSig"))
        ignore(GetString("ExceptionDefsNotCompatibleFieldOrderDiffers"))
        ignore(GetString("typrelModuleNamespaceAttributesDifferInSigAndImpl"))
        ignore(GetString("typrelMethodIsOverconstrained"))
        ignore(GetString("typrelOverloadNotFound"))
        ignore(GetString("typrelOverrideWasAmbiguous"))
        ignore(GetString("typrelMoreThenOneOverride"))
        ignore(GetString("typrelMethodIsSealed"))
        ignore(GetString("typrelOverrideImplementsMoreThenOneSlot"))
        ignore(GetString("typrelDuplicateInterface"))
        ignore(GetString("typrelNeedExplicitImplementation"))
        ignore(GetString("typrelNamedArgumentHasBeenAssignedMoreThenOnce"))
        ignore(GetString("typrelNoImplementationGiven"))
        ignore(GetString("typrelNoImplementationGivenWithSuggestion"))
        ignore(GetString("typrelMemberDoesNotHaveCorrectNumberOfArguments"))
        ignore(GetString("typrelMemberDoesNotHaveCorrectNumberOfTypeParameters"))
        ignore(GetString("typrelMemberDoesNotHaveCorrectKindsOfGenericParameters"))
        ignore(GetString("typrelMemberCannotImplement"))
        ignore(GetString("astParseEmbeddedILError"))
        ignore(GetString("astParseEmbeddedILTypeError"))
        ignore(GetString("astDeprecatedIndexerNotation"))
        ignore(GetString("astInvalidExprLeftHandOfAssignment"))
        ignore(GetString("augNoRefEqualsOnStruct"))
        ignore(GetString("augInvalidAttrs"))
        ignore(GetString("augNoEqualityNeedsNoComparison"))
        ignore(GetString("augStructCompNeedsStructEquality"))
        ignore(GetString("augStructEqNeedsNoCompOrStructComp"))
        ignore(GetString("augTypeCantHaveRefEqAndStructAttrs"))
        ignore(GetString("augOnlyCertainTypesCanHaveAttrs"))
        ignore(GetString("augRefEqCantHaveObjEquals"))
        ignore(GetString("augCustomEqNeedsObjEquals"))
        ignore(GetString("augCustomCompareNeedsIComp"))
        ignore(GetString("augNoEqNeedsNoObjEquals"))
        ignore(GetString("augNoCompCantImpIComp"))
        ignore(GetString("augCustomEqNeedsNoCompOrCustomComp"))
        ignore(GetString("forPositionalSpecifiersNotPermitted"))
        ignore(GetString("forMissingFormatSpecifier"))
        ignore(GetString("forFlagSetTwice"))
        ignore(GetString("forPrefixFlagSpacePlusSetTwice"))
        ignore(GetString("forHashSpecifierIsInvalid"))
        ignore(GetString("forBadPrecision"))
        ignore(GetString("forBadWidth"))
        ignore(GetString("forDoesNotSupportZeroFlag"))
        ignore(GetString("forPrecisionMissingAfterDot"))
        ignore(GetString("forFormatDoesntSupportPrecision"))
        ignore(GetString("forBadFormatSpecifier"))
        ignore(GetString("forLIsUnnecessary"))
        ignore(GetString("forHIsUnnecessary"))
        ignore(GetString("forDoesNotSupportPrefixFlag"))
        ignore(GetString("forBadFormatSpecifierGeneral"))
        ignore(GetString("elSysEnvExitDidntExit"))
        ignore(GetString("elDeprecatedOperator"))
        ignore(GetString("chkProtectedOrBaseCalled"))
        ignore(GetString("chkByrefUsedInInvalidWay"))
        ignore(GetString("chkMutableUsedInInvalidWay"))
        ignore(GetString("chkBaseUsedInInvalidWay"))
        ignore(GetString("chkVariableUsedInInvalidWay"))
        ignore(GetString("chkTypeLessAccessibleThanType"))
        ignore(GetString("chkSystemVoidOnlyInTypeof"))
        ignore(GetString("chkErrorUseOfByref"))
        ignore(GetString("chkErrorContainsCallToRethrow"))
        ignore(GetString("chkSplicingOnlyInQuotations"))
        ignore(GetString("chkNoFirstClassSplicing"))
        ignore(GetString("chkNoFirstClassAddressOf"))
        ignore(GetString("chkNoFirstClassRethrow"))
        ignore(GetString("chkNoByrefAtThisPoint"))
        ignore(GetString("chkLimitationsOfBaseKeyword"))
        ignore(GetString("chkObjCtorsCantUseExceptionHandling"))
        ignore(GetString("chkNoAddressOfAtThisPoint"))
        ignore(GetString("chkNoAddressStaticFieldAtThisPoint"))
        ignore(GetString("chkNoAddressFieldAtThisPoint"))
        ignore(GetString("chkNoAddressOfArrayElementAtThisPoint"))
        ignore(GetString("chkFirstClassFuncNoByref"))
        ignore(GetString("chkReturnTypeNoByref"))
        ignore(GetString("chkInvalidCustAttrVal"))
        ignore(GetString("chkAttrHasAllowMultiFalse"))
        ignore(GetString("chkMemberUsedInInvalidWay"))
        ignore(GetString("chkNoByrefAsTopValue"))
        ignore(GetString("chkReflectedDefCantSplice"))
        ignore(GetString("chkEntryPointUsage"))
        ignore(GetString("chkUnionCaseCompiledForm"))
        ignore(GetString("chkUnionCaseDefaultAugmentation"))
        ignore(GetString("chkPropertySameNameMethod"))
        ignore(GetString("chkGetterSetterDoNotMatchAbstract"))
        ignore(GetString("chkPropertySameNameIndexer"))
        ignore(GetString("chkCantStoreByrefValue"))
        ignore(GetString("chkDuplicateMethod"))
        ignore(GetString("chkDuplicateMethodWithSuffix"))
        ignore(GetString("chkDuplicateMethodCurried"))
        ignore(GetString("chkCurriedMethodsCantHaveOutParams"))
        ignore(GetString("chkDuplicateProperty"))
        ignore(GetString("chkDuplicatePropertyWithSuffix"))
        ignore(GetString("chkDuplicateMethodInheritedType"))
        ignore(GetString("chkDuplicateMethodInheritedTypeWithSuffix"))
        ignore(GetString("chkMultipleGenericInterfaceInstantiations"))
        ignore(GetString("chkValueWithDefaultValueMustHaveDefaultValue"))
        ignore(GetString("chkNoByrefInTypeAbbrev"))
        ignore(GetString("crefBoundVarUsedInSplice"))
        ignore(GetString("crefQuotationsCantContainGenericExprs"))
        ignore(GetString("crefQuotationsCantContainGenericFunctions"))
        ignore(GetString("crefQuotationsCantContainObjExprs"))
        ignore(GetString("crefQuotationsCantContainAddressOf"))
        ignore(GetString("crefQuotationsCantContainStaticFieldRef"))
        ignore(GetString("crefQuotationsCantContainInlineIL"))
        ignore(GetString("crefQuotationsCantContainDescendingForLoops"))
        ignore(GetString("crefQuotationsCantFetchUnionIndexes"))
        ignore(GetString("crefQuotationsCantSetUnionFields"))
        ignore(GetString("crefQuotationsCantSetExceptionFields"))
        ignore(GetString("crefQuotationsCantRequireByref"))
        ignore(GetString("crefQuotationsCantCallTraitMembers"))
        ignore(GetString("crefQuotationsCantContainThisConstant"))
        ignore(GetString("crefQuotationsCantContainThisPatternMatch"))
        ignore(GetString("crefQuotationsCantContainArrayPatternMatching"))
        ignore(GetString("crefQuotationsCantContainThisType"))
        ignore(GetString("csTypeCannotBeResolvedAtCompileTime"))
        ignore(GetString("csCodeLessGeneric"))
        ignore(GetString("csTypeInferenceMaxDepth"))
        ignore(GetString("csExpectedArguments"))
        ignore(GetString("csIndexArgumentMismatch"))
        ignore(GetString("csExpectTypeWithOperatorButGivenFunction"))
        ignore(GetString("csExpectTypeWithOperatorButGivenTuple"))
        ignore(GetString("csTypesDoNotSupportOperator"))
        ignore(GetString("csTypeDoesNotSupportOperator"))
        ignore(GetString("csTypesDoNotSupportOperatorNullable"))
        ignore(GetString("csTypeDoesNotSupportOperatorNullable"))
        ignore(GetString("csTypeDoesNotSupportConversion"))
        ignore(GetString("csMethodFoundButIsStatic"))
        ignore(GetString("csMethodFoundButIsNotStatic"))
        ignore(GetString("csStructConstraintInconsistent"))
        ignore(GetString("csTypeDoesNotHaveNull"))
        ignore(GetString("csNullableTypeDoesNotHaveNull"))
        ignore(GetString("csTypeDoesNotSupportComparison1"))
        ignore(GetString("csTypeDoesNotSupportComparison2"))
        ignore(GetString("csTypeDoesNotSupportComparison3"))
        ignore(GetString("csTypeDoesNotSupportEquality1"))
        ignore(GetString("csTypeDoesNotSupportEquality2"))
        ignore(GetString("csTypeDoesNotSupportEquality3"))
        ignore(GetString("csTypeIsNotEnumType"))
        ignore(GetString("csTypeHasNonStandardDelegateType"))
        ignore(GetString("csTypeIsNotDelegateType"))
        ignore(GetString("csTypeParameterCannotBeNullable"))
        ignore(GetString("csGenericConstructRequiresStructType"))
        ignore(GetString("csGenericConstructRequiresUnmanagedType"))
        ignore(GetString("csTypeNotCompatibleBecauseOfPrintf"))
        ignore(GetString("csGenericConstructRequiresReferenceSemantics"))
        ignore(GetString("csGenericConstructRequiresNonAbstract"))
        ignore(GetString("csGenericConstructRequiresPublicDefaultConstructor"))
        ignore(GetString("csTypeInstantiationLengthMismatch"))
        ignore(GetString("csOptionalArgumentNotPermittedHere"))
        ignore(GetString("csMemberIsNotStatic"))
        ignore(GetString("csMemberIsNotInstance"))
        ignore(GetString("csArgumentLengthMismatch"))
        ignore(GetString("csArgumentTypesDoNotMatch"))
        ignore(GetString("csMethodExpectsParams"))
        ignore(GetString("csMemberIsNotAccessible"))
        ignore(GetString("csMemberIsNotAccessible2"))
        ignore(GetString("csMethodIsNotAStaticMethod"))
        ignore(GetString("csMethodIsNotAnInstanceMethod"))
        ignore(GetString("csMemberHasNoArgumentOrReturnProperty"))
        ignore(GetString("csRequiredSignatureIs"))
        ignore(GetString("csMemberSignatureMismatch"))
        ignore(GetString("csMemberSignatureMismatch2"))
        ignore(GetString("csMemberSignatureMismatch3"))
        ignore(GetString("csMemberSignatureMismatch4"))
        ignore(GetString("csMemberSignatureMismatchArityNamed"))
        ignore(GetString("csMemberSignatureMismatchArity"))
        ignore(GetString("csMemberSignatureMismatchArityType"))
        ignore(GetString("csMemberNotAccessible"))
        ignore(GetString("csIncorrectGenericInstantiation"))
        ignore(GetString("csMemberOverloadArityMismatch"))
        ignore(GetString("csNoMemberTakesTheseArguments"))
        ignore(GetString("csNoMemberTakesTheseArguments2"))
        ignore(GetString("csNoMemberTakesTheseArguments3"))
        ignore(GetString("csMethodNotFound"))
        ignore(GetString("csNoOverloadsFound"))
        ignore(GetString("csMethodIsOverloaded"))
        ignore(GetString("csCandidates"))
        ignore(GetString("csSeeAvailableOverloads"))
        ignore(GetString("parsDoCannotHaveVisibilityDeclarations"))
        ignore(GetString("parsEofInHashIf"))
        ignore(GetString("parsEofInString"))
        ignore(GetString("parsEofInVerbatimString"))
        ignore(GetString("parsEofInComment"))
        ignore(GetString("parsEofInStringInComment"))
        ignore(GetString("parsEofInVerbatimStringInComment"))
        ignore(GetString("parsEofInIfOcaml"))
        ignore(GetString("parsEofInDirective"))
        ignore(GetString("parsNoHashEndIfFound"))
        ignore(GetString("parsAttributesIgnored"))
        ignore(GetString("parsUseBindingsIllegalInImplicitClassConstructors"))
        ignore(GetString("parsUseBindingsIllegalInModules"))
        ignore(GetString("parsIntegerForLoopRequiresSimpleIdentifier"))
        ignore(GetString("parsOnlyOneWithAugmentationAllowed"))
        ignore(GetString("parsUnexpectedSemicolon"))
        ignore(GetString("parsUnexpectedEndOfFile"))
        ignore(GetString("parsUnexpectedVisibilityDeclaration"))
        ignore(GetString("parsOnlyHashDirectivesAllowed"))
        ignore(GetString("parsVisibilityDeclarationsShouldComePriorToIdentifier"))
        ignore(GetString("parsNamespaceOrModuleNotBoth"))
        ignore(GetString("parsModuleAbbreviationMustBeSimpleName"))
        ignore(GetString("parsIgnoreAttributesOnModuleAbbreviation"))
        ignore(GetString("parsIgnoreAttributesOnModuleAbbreviationAlwaysPrivate"))
        ignore(GetString("parsIgnoreVisibilityOnModuleAbbreviationAlwaysPrivate"))
        ignore(GetString("parsUnClosedBlockInHashLight"))
        ignore(GetString("parsUnmatchedBeginOrStruct"))
        ignore(GetString("parsModuleDefnMustBeSimpleName"))
        ignore(GetString("parsUnexpectedEmptyModuleDefn"))
        ignore(GetString("parsAttributesMustComeBeforeVal"))
        ignore(GetString("parsAttributesAreNotPermittedOnInterfaceImplementations"))
        ignore(GetString("parsSyntaxError"))
        ignore(GetString("parsAugmentationsIllegalOnDelegateType"))
        ignore(GetString("parsUnmatchedClassInterfaceOrStruct"))
        ignore(GetString("parsEmptyTypeDefinition"))
        ignore(GetString("parsUnmatchedWith"))
        ignore(GetString("parsGetOrSetRequired"))
        ignore(GetString("parsOnlyClassCanTakeValueArguments"))
        ignore(GetString("parsUnmatchedBegin"))
        ignore(GetString("parsInvalidDeclarationSyntax"))
        ignore(GetString("parsGetAndOrSetRequired"))
        ignore(GetString("parsTypeAnnotationsOnGetSet"))
        ignore(GetString("parsGetterMustHaveAtLeastOneArgument"))
        ignore(GetString("parsMultipleAccessibilitiesForGetSet"))
        ignore(GetString("parsSetSyntax"))
        ignore(GetString("parsInterfacesHaveSameVisibilityAsEnclosingType"))
        ignore(GetString("parsAccessibilityModsIllegalForAbstract"))
        ignore(GetString("parsAttributesIllegalOnInherit"))
        ignore(GetString("parsVisibilityIllegalOnInherit"))
        ignore(GetString("parsInheritDeclarationsCannotHaveAsBindings"))
        ignore(GetString("parsAttributesIllegalHere"))
        ignore(GetString("parsTypeAbbreviationsCannotHaveVisibilityDeclarations"))
        ignore(GetString("parsEnumTypesCannotHaveVisibilityDeclarations"))
        ignore(GetString("parsAllEnumFieldsRequireValues"))
        ignore(GetString("parsInlineAssemblyCannotHaveVisibilityDeclarations"))
        ignore(GetString("parsUnexpectedIdentifier"))
        ignore(GetString("parsUnionCasesCannotHaveVisibilityDeclarations"))
        ignore(GetString("parsEnumFieldsCannotHaveVisibilityDeclarations"))
        ignore(GetString("parsConsiderUsingSeparateRecordType"))
        ignore(GetString("parsRecordFieldsCannotHaveVisibilityDeclarations"))
        ignore(GetString("parsLetAndForNonRecBindings"))
        ignore(GetString("parsUnmatchedParen"))
        ignore(GetString("parsSuccessivePatternsShouldBeSpacedOrTupled"))
        ignore(GetString("parsNoMatchingInForLet"))
        ignore(GetString("parsErrorInReturnForLetIncorrectIndentation"))
        ignore(GetString("parsExpectedStatementAfterLet"))
        ignore(GetString("parsIncompleteIf"))
        ignore(GetString("parsAssertIsNotFirstClassValue"))
        ignore(GetString("parsIdentifierExpected"))
        ignore(GetString("parsInOrEqualExpected"))
        ignore(GetString("parsArrowUseIsLimited"))
        ignore(GetString("parsSuccessiveArgsShouldBeSpacedOrTupled"))
        ignore(GetString("parsUnmatchedBracket"))
        ignore(GetString("parsMissingQualificationAfterDot"))
        ignore(GetString("parsParenFormIsForML"))
        ignore(GetString("parsMismatchedQuote"))
        ignore(GetString("parsUnmatched"))
        ignore(GetString("parsUnmatchedBracketBar"))
        ignore(GetString("parsUnmatchedBrace"))
        ignore(GetString("parsFieldBinding"))
        ignore(GetString("parsMemberIllegalInObjectImplementation"))
        ignore(GetString("parsMissingFunctionBody"))
        ignore(GetString("parsSyntaxErrorInLabeledType"))
        ignore(GetString("parsUnexpectedInfixOperator"))
        ignore(GetString("parsMultiArgumentGenericTypeFormDeprecated"))
        ignore(GetString("parsInvalidLiteralInType"))
        ignore(GetString("parsUnexpectedOperatorForUnitOfMeasure"))
        ignore(GetString("parsUnexpectedIntegerLiteralForUnitOfMeasure"))
        ignore(GetString("parsUnexpectedTypeParameter"))
        ignore(GetString("parsMismatchedQuotationName"))
        ignore(GetString("parsActivePatternCaseMustBeginWithUpperCase"))
        ignore(GetString("parsNoEqualShouldFollowNamespace"))
        ignore(GetString("parsSyntaxModuleStructEndDeprecated"))
        ignore(GetString("parsSyntaxModuleSigEndDeprecated"))
        ignore(GetString("tcStaticFieldUsedWhenInstanceFieldExpected"))
        ignore(GetString("tcMethodNotAccessible"))
        ignore(GetString("tcImplicitMeasureFollowingSlash"))
        ignore(GetString("tcUnexpectedMeasureAnon"))
        ignore(GetString("tcNonZeroConstantCannotHaveGenericUnit"))
        ignore(GetString("tcSeqResultsUseYield"))
        ignore(GetString("tcUnexpectedBigRationalConstant"))
        ignore(GetString("tcInvalidTypeForUnitsOfMeasure"))
        ignore(GetString("tcUnexpectedConstUint16Array"))
        ignore(GetString("tcUnexpectedConstByteArray"))
        ignore(GetString("tcParameterRequiresName"))
        ignore(GetString("tcReturnValuesCannotHaveNames"))
        ignore(GetString("tcMemberKindPropertyGetSetNotExpected"))
        ignore(GetString("tcNamespaceCannotContainValues"))
        ignore(GetString("tcNamespaceCannotContainExtensionMembers"))
        ignore(GetString("tcMultipleVisibilityAttributes"))
        ignore(GetString("tcMultipleVisibilityAttributesWithLet"))
        ignore(GetString("tcUnrecognizedAccessibilitySpec"))
        ignore(GetString("tcInvalidMethodNameForRelationalOperator"))
        ignore(GetString("tcInvalidMethodNameForEquality"))
        ignore(GetString("tcInvalidMemberName"))
        ignore(GetString("tcInvalidMemberNameFixedTypes"))
        ignore(GetString("tcInvalidOperatorDefinitionRelational"))
        ignore(GetString("tcInvalidOperatorDefinitionEquality"))
        ignore(GetString("tcInvalidOperatorDefinition"))
        ignore(GetString("tcInvalidIndexOperatorDefinition"))
        ignore(GetString("tcExpectModuleOrNamespaceParent"))
        ignore(GetString("tcImplementsIComparableExplicitly"))
        ignore(GetString("tcImplementsGenericIComparableExplicitly"))
        ignore(GetString("tcImplementsIStructuralComparableExplicitly"))
        ignore(GetString("tcRecordFieldInconsistentTypes"))
        ignore(GetString("tcDllImportStubsCannotBeInlined"))
        ignore(GetString("tcStructsCanOnlyBindThisAtMemberDeclaration"))
        ignore(GetString("tcUnexpectedExprAtRecInfPoint"))
        ignore(GetString("tcLessGenericBecauseOfAnnotation"))
        ignore(GetString("tcConstrainedTypeVariableCannotBeGeneralized"))
        ignore(GetString("tcGenericParameterHasBeenConstrained"))
        ignore(GetString("tcTypeParameterHasBeenConstrained"))
        ignore(GetString("tcTypeParametersInferredAreNotStable"))
        ignore(GetString("tcExplicitTypeParameterInvalid"))
        ignore(GetString("tcOverridingMethodRequiresAllOrNoTypeParameters"))
        ignore(GetString("tcFieldsDoNotDetermineUniqueRecordType"))
        ignore(GetString("tcFieldAppearsTwiceInRecord"))
        ignore(GetString("tcUnknownUnion"))
        ignore(GetString("tcNotSufficientlyGenericBecauseOfScope"))
        ignore(GetString("tcPropertyRequiresExplicitTypeParameters"))
        ignore(GetString("tcConstructorCannotHaveTypeParameters"))
        ignore(GetString("tcInstanceMemberRequiresTarget"))
        ignore(GetString("tcUnexpectedPropertyInSyntaxTree"))
        ignore(GetString("tcStaticInitializerRequiresArgument"))
        ignore(GetString("tcObjectConstructorRequiresArgument"))
        ignore(GetString("tcStaticMemberShouldNotHaveThis"))
        ignore(GetString("tcExplicitStaticInitializerSyntax"))
        ignore(GetString("tcExplicitObjectConstructorSyntax"))
        ignore(GetString("tcUnexpectedPropertySpec"))
        ignore(GetString("tcObjectExpressionFormDeprecated"))
        ignore(GetString("tcInvalidDeclaration"))
        ignore(GetString("tcAttributesInvalidInPatterns"))
        ignore(GetString("tcFunctionRequiresExplicitTypeArguments"))
        ignore(GetString("tcDoesNotAllowExplicitTypeArguments"))
        ignore(GetString("tcTypeParameterArityMismatch"))
        ignore(GetString("tcDefaultStructConstructorCall"))
        ignore(GetString("tcCouldNotFindIDisposable"))
        ignore(GetString("tcNonLiteralCannotBeUsedInPattern"))
        ignore(GetString("tcFieldIsReadonly"))
        ignore(GetString("tcNameArgumentsMustAppearLast"))
        ignore(GetString("tcFunctionRequiresExplicitLambda"))
        ignore(GetString("tcTypeCannotBeEnumerated"))
        ignore(GetString("tcBadReturnTypeForGetEnumerator"))
        ignore(GetString("tcInvalidMixtureOfRecursiveForms"))
        ignore(GetString("tcInvalidObjectConstructionExpression"))
        ignore(GetString("tcInvalidConstraint"))
        ignore(GetString("tcInvalidConstraintTypeSealed"))
        ignore(GetString("tcInvalidEnumConstraint"))
        ignore(GetString("tcInvalidNewConstraint"))
        ignore(GetString("tcInvalidPropertyType"))
        ignore(GetString("tcExpectedUnitOfMeasureMarkWithAttribute"))
        ignore(GetString("tcExpectedTypeParameter"))
        ignore(GetString("tcExpectedTypeNotUnitOfMeasure"))
        ignore(GetString("tcExpectedUnitOfMeasureNotType"))
        ignore(GetString("tcInvalidUnitsOfMeasurePrefix"))
        ignore(GetString("tcUnitsOfMeasureInvalidInTypeConstructor"))
        ignore(GetString("tcRequireBuilderMethod"))
        ignore(GetString("tcTypeHasNoNestedTypes"))
        ignore(GetString("tcUnexpectedSymbolInTypeExpression"))
        ignore(GetString("tcTypeParameterInvalidAsTypeConstructor"))
        ignore(GetString("tcIllegalSyntaxInTypeExpression"))
        ignore(GetString("tcAnonymousUnitsOfMeasureCannotBeNested"))
        ignore(GetString("tcAnonymousTypeInvalidInDeclaration"))
        ignore(GetString("tcUnexpectedSlashInType"))
        ignore(GetString("tcUnexpectedTypeArguments"))
        ignore(GetString("tcOptionalArgsOnlyOnMembers"))
        ignore(GetString("tcNameNotBoundInPattern"))
        ignore(GetString("tcInvalidNonPrimitiveLiteralInPatternMatch"))
        ignore(GetString("tcInvalidTypeArgumentUsage"))
        ignore(GetString("tcRequireActivePatternWithOneResult"))
        ignore(GetString("tcInvalidArgForParameterizedPattern"))
        ignore(GetString("tcInvalidIndexIntoActivePatternArray"))
        ignore(GetString("tcUnionCaseDoesNotTakeArguments"))
        ignore(GetString("tcUnionCaseRequiresOneArgument"))
        ignore(GetString("tcUnionCaseExpectsTupledArguments"))
        ignore(GetString("tcFieldIsNotStatic"))
        ignore(GetString("tcFieldNotLiteralCannotBeUsedInPattern"))
        ignore(GetString("tcRequireVarConstRecogOrLiteral"))
        ignore(GetString("tcInvalidPattern"))
        ignore(GetString("tcUseWhenPatternGuard"))
        ignore(GetString("tcIllegalPattern"))
        ignore(GetString("tcSyntaxErrorUnexpectedQMark"))
        ignore(GetString("tcExpressionCountMisMatch"))
        ignore(GetString("tcExprUndelayed"))
        ignore(GetString("tcExpressionRequiresSequence"))
        ignore(GetString("tcInvalidObjectExpressionSyntaxForm"))
        ignore(GetString("tcInvalidObjectSequenceOrRecordExpression"))
        ignore(GetString("tcInvalidSequenceExpressionSyntaxForm"))
        ignore(GetString("tcExpressionWithIfRequiresParenthesis"))
        ignore(GetString("tcUnableToParseFormatString"))
        ignore(GetString("tcListLiteralMaxSize"))
        ignore(GetString("tcExpressionFormRequiresObjectConstructor"))
        ignore(GetString("tcNamedArgumentsCannotBeUsedInMemberTraits"))
        ignore(GetString("tcNotValidEnumCaseName"))
        ignore(GetString("tcFieldIsNotMutable"))
        ignore(GetString("tcConstructRequiresListArrayOrSequence"))
        ignore(GetString("tcConstructRequiresComputationExpressions"))
        ignore(GetString("tcConstructRequiresSequenceOrComputations"))
        ignore(GetString("tcConstructRequiresComputationExpression"))
        ignore(GetString("tcInvalidIndexerExpression"))
        ignore(GetString("tcObjectOfIndeterminateTypeUsedRequireTypeConstraint"))
        ignore(GetString("tcCannotInheritFromVariableType"))
        ignore(GetString("tcObjectConstructorsOnTypeParametersCannotTakeArguments"))
        ignore(GetString("tcCompiledNameAttributeMisused"))
        ignore(GetString("tcNamedTypeRequired"))
        ignore(GetString("tcInheritCannotBeUsedOnInterfaceType"))
        ignore(GetString("tcNewCannotBeUsedOnInterfaceType"))
        ignore(GetString("tcAbstractTypeCannotBeInstantiated"))
        ignore(GetString("tcIDisposableTypeShouldUseNew"))
        ignore(GetString("tcSyntaxCanOnlyBeUsedToCreateObjectTypes"))
        ignore(GetString("tcConstructorRequiresCall"))
        ignore(GetString("tcUndefinedField"))
        ignore(GetString("tcFieldRequiresAssignment"))
        ignore(GetString("tcExtraneousFieldsGivenValues"))
        ignore(GetString("tcObjectExpressionsCanOnlyOverrideAbstractOrVirtual"))
        ignore(GetString("tcNoAbstractOrVirtualMemberFound"))
        ignore(GetString("tcArgumentArityMismatch"))
        ignore(GetString("tcArgumentArityMismatchOneOverload"))
        ignore(GetString("tcSimpleMethodNameRequired"))
        ignore(GetString("tcPredefinedTypeCannotBeUsedAsSuperType"))
        ignore(GetString("tcNewMustBeUsedWithNamedType"))
        ignore(GetString("tcCannotCreateExtensionOfSealedType"))
        ignore(GetString("tcNoArgumentsForRecordValue"))
        ignore(GetString("tcNoInterfaceImplementationForConstructionExpression"))
        ignore(GetString("tcObjectConstructionCanOnlyBeUsedInClassTypes"))
        ignore(GetString("tcOnlySimpleBindingsCanBeUsedInConstructionExpressions"))
        ignore(GetString("tcObjectsMustBeInitializedWithObjectExpression"))
        ignore(GetString("tcExpectedInterfaceType"))
        ignore(GetString("tcConstructorForInterfacesDoNotTakeArguments"))
        ignore(GetString("tcConstructorRequiresArguments"))
        ignore(GetString("tcNewRequiresObjectConstructor"))
        ignore(GetString("tcAtLeastOneOverrideIsInvalid"))
        ignore(GetString("tcNumericLiteralRequiresModule"))
        ignore(GetString("tcInvalidRecordConstruction"))
        ignore(GetString("tcExpressionFormRequiresRecordTypes"))
        ignore(GetString("tcInheritedTypeIsNotObjectModelType"))
        ignore(GetString("tcObjectConstructionExpressionCanOnlyImplementConstructorsInObjectModelTypes"))
        ignore(GetString("tcEmptyRecordInvalid"))
        ignore(GetString("tcTypeIsNotARecordTypeNeedConstructor"))
        ignore(GetString("tcTypeIsNotARecordType"))
        ignore(GetString("tcConstructIsAmbiguousInComputationExpression"))
        ignore(GetString("tcConstructIsAmbiguousInSequenceExpression"))
        ignore(GetString("tcDoBangIllegalInSequenceExpression"))
        ignore(GetString("tcUseForInSequenceExpression"))
        ignore(GetString("tcTryIllegalInSequenceExpression"))
        ignore(GetString("tcUseYieldBangForMultipleResults"))
        ignore(GetString("tcInvalidAssignment"))
        ignore(GetString("tcInvalidUseOfTypeName"))
        ignore(GetString("tcTypeHasNoAccessibleConstructor"))
        ignore(GetString("tcInvalidUseOfTypeNameOrConstructor"))
        ignore(GetString("tcInvalidUseOfTypeNameOrConstructorWithOverloads"))
        ignore(GetString("tcInvalidUseOfInterfaceType"))
        ignore(GetString("tcInvalidUseOfDelegate"))
        ignore(GetString("tcPropertyIsNotStatic"))
        ignore(GetString("tcPropertyIsNotReadable"))
        ignore(GetString("tcLookupMayNotBeUsedHere"))
        ignore(GetString("tcPropertyIsStatic"))
        ignore(GetString("tcPropertyCannotBeSet1"))
        ignore(GetString("tcConstructorsCannotBeFirstClassValues"))
        ignore(GetString("tcSyntaxFormUsedOnlyWithRecordLabelsPropertiesAndFields"))
        ignore(GetString("tcEventIsStatic"))
        ignore(GetString("tcEventIsNotStatic"))
        ignore(GetString("tcNamedArgumentDidNotMatch"))
        ignore(GetString("tcOverloadsCannotHaveCurriedArguments"))
        ignore(GetString("tcUnnamedArgumentsDoNotFormPrefix"))
        ignore(GetString("tcStaticOptimizationConditionalsOnlyForFSharpLibrary"))
        ignore(GetString("tcFormalArgumentIsNotOptional"))
        ignore(GetString("tcInvalidOptionalAssignmentToPropertyOrField"))
        ignore(GetString("tcDelegateConstructorMustBePassed"))
        ignore(GetString("tcBindingCannotBeUseAndRec"))
        ignore(GetString("tcVolatileOnlyOnClassLetBindings"))
        ignore(GetString("tcAttributesAreNotPermittedOnLetBindings"))
        ignore(GetString("tcDefaultValueAttributeRequiresVal"))
        ignore(GetString("tcConditionalAttributeRequiresMembers"))
        ignore(GetString("tcInvalidActivePatternName"))
        ignore(GetString("tcEntryPointAttributeRequiresFunctionInModule"))
        ignore(GetString("tcMutableValuesCannotBeInline"))
        ignore(GetString("tcMutableValuesMayNotHaveGenericParameters"))
        ignore(GetString("tcMutableValuesSyntax"))
        ignore(GetString("tcOnlyFunctionsCanBeInline"))
        ignore(GetString("tcIllegalAttributesForLiteral"))
        ignore(GetString("tcLiteralCannotBeMutable"))
        ignore(GetString("tcLiteralCannotBeInline"))
        ignore(GetString("tcLiteralCannotHaveGenericParameters"))
        ignore(GetString("tcInvalidConstantExpression"))
        ignore(GetString("tcTypeIsInaccessible"))
        ignore(GetString("tcUnexpectedConditionInImportedAssembly"))
        ignore(GetString("tcUnrecognizedAttributeTarget"))
        ignore(GetString("tcAttributeIsNotValidForLanguageElementUseDo"))
        ignore(GetString("tcAttributeIsNotValidForLanguageElement"))
        ignore(GetString("tcOptionalArgumentsCannotBeUsedInCustomAttribute"))
        ignore(GetString("tcPropertyCannotBeSet0"))
        ignore(GetString("tcPropertyOrFieldNotFoundInAttribute"))
        ignore(GetString("tcCustomAttributeMustBeReferenceType"))
        ignore(GetString("tcCustomAttributeArgumentMismatch"))
        ignore(GetString("tcCustomAttributeMustInvokeConstructor"))
        ignore(GetString("tcAttributeExpressionsMustBeConstructorCalls"))
        ignore(GetString("tcUnsupportedAttribute"))
        ignore(GetString("tcInvalidInlineSpecification"))
        ignore(GetString("tcInvalidUseBinding"))
        ignore(GetString("tcAbstractMembersIllegalInAugmentation"))
        ignore(GetString("tcMethodOverridesIllegalHere"))
        ignore(GetString("tcNoMemberFoundForOverride"))
        ignore(GetString("tcOverrideArityMismatch"))
        ignore(GetString("tcDefaultImplementationAlreadyExists"))
        ignore(GetString("tcDefaultAmbiguous"))
        ignore(GetString("tcNoPropertyFoundForOverride"))
        ignore(GetString("tcAbstractPropertyMissingGetOrSet"))
        ignore(GetString("tcInvalidSignatureForSet"))
        ignore(GetString("tcPropertyAlreadyHasDefaultImplementation"))
        ignore(GetString("tcPropertyImplementedIsAmbiguous"))
        ignore(GetString("tcNewMemberHidesAbstractMember"))
        ignore(GetString("tcNewMemberHidesAbstractMemberWithSuffix"))
        ignore(GetString("tcStaticInitializersIllegalInInterface"))
        ignore(GetString("tcObjectConstructorsIllegalInInterface"))
        ignore(GetString("tcMemberOverridesIllegalInInterface"))
        ignore(GetString("tcConcreteMembersIllegalInInterface"))
        ignore(GetString("tcConstructorsDisallowedInExceptionAugmentation"))
        ignore(GetString("tcStructsCannotHaveConstructorWithNoArguments"))
        ignore(GetString("tcConstructorsIllegalForThisType"))
        ignore(GetString("tcRecursiveBindingsWithMembersMustBeDirectAugmentation"))
        ignore(GetString("tcOnlySimplePatternsInLetRec"))
        ignore(GetString("tcOnlyRecordFieldsAndSimpleLetCanBeMutable"))
        ignore(GetString("tcMemberIsNotSufficientlyGeneric"))
        ignore(GetString("tcLiteralAttributeRequiresConstantValue"))
        ignore(GetString("tcValueInSignatureRequiresLiteralAttribute"))
        ignore(GetString("tcThreadStaticAndContextStaticMustBeStatic"))
        ignore(GetString("tcVolatileFieldsMustBeMutable"))
        ignore(GetString("tcUninitializedValFieldsMustBeMutable"))
        ignore(GetString("tcStaticValFieldsMustBeMutableAndPrivate"))
        ignore(GetString("tcFieldRequiresName"))
        ignore(GetString("tcInvalidNamespaceModuleTypeUnionName"))
        ignore(GetString("tcIllegalFormForExplicitTypeDeclaration"))
        ignore(GetString("tcReturnTypesForUnionMustBeSameAsType"))
        ignore(GetString("tcInvalidEnumerationLiteral"))
        ignore(GetString("tcTypeIsNotInterfaceType1"))
        ignore(GetString("tcDuplicateSpecOfInterface"))
        ignore(GetString("tcFieldValIllegalHere"))
        ignore(GetString("tcInheritIllegalHere"))
        ignore(GetString("tcModuleRequiresQualifiedAccess"))
        ignore(GetString("tcOpenUsedWithPartiallyQualifiedPath"))
        ignore(GetString("tcLocalClassBindingsCannotBeInline"))
        ignore(GetString("tcTypeAbbreviationsMayNotHaveMembers"))
        ignore(GetString("tcEnumerationsMayNotHaveMembers"))
        ignore(GetString("tcMeasureDeclarationsRequireStaticMembers"))
        ignore(GetString("tcStructsMayNotContainDoBindings"))
        ignore(GetString("tcStructsMayNotContainLetBindings"))
        ignore(GetString("tcStaticLetBindingsRequireClassesWithImplicitConstructors"))
        ignore(GetString("tcMeasureDeclarationsRequireStaticMembersNotConstructors"))
        ignore(GetString("tcMemberAndLocalClassBindingHaveSameName"))
        ignore(GetString("tcTypeAbbreviationsCannotHaveInterfaceDeclaration"))
        ignore(GetString("tcEnumerationsCannotHaveInterfaceDeclaration"))
        ignore(GetString("tcTypeIsNotInterfaceType0"))
        ignore(GetString("tcAllImplementedInterfacesShouldBeDeclared"))
        ignore(GetString("tcDefaultImplementationForInterfaceHasAlreadyBeenAdded"))
        ignore(GetString("tcMemberNotPermittedInInterfaceImplementation"))
        ignore(GetString("tcDeclarationElementNotPermittedInAugmentation"))
        ignore(GetString("tcTypesCannotContainNestedTypes"))
        ignore(GetString("tcTypeExceptionOrModule"))
        ignore(GetString("tcTypeOrModule"))
        ignore(GetString("tcImplementsIStructuralEquatableExplicitly"))
        ignore(GetString("tcImplementsIEquatableExplicitly"))
        ignore(GetString("tcExplicitTypeSpecificationCannotBeUsedForExceptionConstructors"))
        ignore(GetString("tcExceptionAbbreviationsShouldNotHaveArgumentList"))
        ignore(GetString("tcAbbreviationsFordotNetExceptionsCannotTakeArguments"))
        ignore(GetString("tcExceptionAbbreviationsMustReferToValidExceptions"))
        ignore(GetString("tcAbbreviationsFordotNetExceptionsMustHaveMatchingObjectConstructor"))
        ignore(GetString("tcNotAnException"))
        ignore(GetString("tcInvalidModuleName"))
        ignore(GetString("tcInvalidTypeExtension"))
        ignore(GetString("tcAttributesOfTypeSpecifyMultipleKindsForType"))
        ignore(GetString("tcKindOfTypeSpecifiedDoesNotMatchDefinition"))
        ignore(GetString("tcMeasureDefinitionsCannotHaveTypeParameters"))
        ignore(GetString("tcTypeRequiresDefinition"))
        ignore(GetString("tcTypeAbbreviationHasTypeParametersMissingOnType"))
        ignore(GetString("tcStructsInterfacesEnumsDelegatesMayNotInheritFromOtherTypes"))
        ignore(GetString("tcTypesCannotInheritFromMultipleConcreteTypes"))
        ignore(GetString("tcRecordsUnionsAbbreviationsStructsMayNotHaveAllowNullLiteralAttribute"))
        ignore(GetString("tcAllowNullTypesMayOnlyInheritFromAllowNullTypes"))
        ignore(GetString("tcGenericTypesCannotHaveStructLayout"))
        ignore(GetString("tcOnlyStructsCanHaveStructLayout"))
        ignore(GetString("tcRepresentationOfTypeHiddenBySignature"))
        ignore(GetString("tcOnlyClassesCanHaveAbstract"))
        ignore(GetString("tcOnlyTypesRepresentingUnitsOfMeasureCanHaveMeasure"))
        ignore(GetString("tcOverridesCannotHaveVisibilityDeclarations"))
        ignore(GetString("tcTypesAreAlwaysSealedDU"))
        ignore(GetString("tcTypesAreAlwaysSealedRecord"))
        ignore(GetString("tcTypesAreAlwaysSealedAssemblyCode"))
        ignore(GetString("tcTypesAreAlwaysSealedStruct"))
        ignore(GetString("tcTypesAreAlwaysSealedDelegate"))
        ignore(GetString("tcTypesAreAlwaysSealedEnum"))
        ignore(GetString("tcInterfaceTypesAndDelegatesCannotContainFields"))
        ignore(GetString("tcAbbreviatedTypesCannotBeSealed"))
        ignore(GetString("tcCannotInheritFromSealedType"))
        ignore(GetString("tcCannotInheritFromInterfaceType"))
        ignore(GetString("tcStructTypesCannotContainAbstractMembers"))
        ignore(GetString("tcInterfaceTypesCannotBeSealed"))
        ignore(GetString("tcInvalidDelegateSpecification"))
        ignore(GetString("tcDelegatesCannotBeCurried"))
        ignore(GetString("tcInvalidTypeForLiteralEnumeration"))
        ignore(GetString("tcTypeDefinitionIsCyclic"))
        ignore(GetString("tcTypeDefinitionIsCyclicThroughInheritance"))
        ignore(GetString("tcReservedSyntaxForAugmentation"))
        ignore(GetString("tcMembersThatExtendInterfaceMustBePlacedInSeparateModule"))
        ignore(GetString("tcDeclaredTypeParametersForExtensionDoNotMatchOriginal"))
        ignore(GetString("tcTypeDefinitionsWithImplicitConstructionMustHaveOneInherit"))
        ignore(GetString("tcTypeDefinitionsWithImplicitConstructionMustHaveLocalBindingsBeforeMembers"))
        ignore(GetString("tcInheritDeclarationMissingArguments"))
        ignore(GetString("tcInheritConstructionCallNotPartOfImplicitSequence"))
        ignore(GetString("tcLetAndDoRequiresImplicitConstructionSequence"))
        ignore(GetString("tcTypeAbbreviationsCannotHaveAugmentations"))
        ignore(GetString("tcModuleAbbreviationForNamespace"))
        ignore(GetString("tcTypeUsedInInvalidWay"))
        ignore(GetString("tcMemberUsedInInvalidWay"))
        ignore(GetString("tcAttributeAutoOpenWasIgnored"))
        ignore(GetString("ilUndefinedValue"))
        ignore(GetString("ilLabelNotFound"))
        ignore(GetString("ilIncorrectNumberOfTypeArguments"))
        ignore(GetString("ilDynamicInvocationNotSupported"))
        ignore(GetString("ilAddressOfLiteralFieldIsInvalid"))
        ignore(GetString("ilAddressOfValueHereIsInvalid"))
        ignore(GetString("ilValuesWithLiteralAttributeCannotBeMutable"))
        ignore(GetString("ilValuesWithLiteralAttributeMustBeSimple"))
        ignore(GetString("ilCustomMarshallersCannotBeUsedInFSharp"))
        ignore(GetString("ilMarshalAsAttributeCannotBeDecoded"))
        ignore(GetString("ilSignatureForExternalFunctionContainsTypeParameters"))
        ignore(GetString("ilDllImportAttributeCouldNotBeDecoded"))
        ignore(GetString("ilLiteralFieldsCannotBeSet"))
        ignore(GetString("ilStaticMethodIsNotLambda"))
        ignore(GetString("ilMutableVariablesCannotEscapeMethod"))
        ignore(GetString("ilUnexpectedUnrealizedValue"))
        ignore(GetString("ilMainModuleEmpty"))
        ignore(GetString("ilTypeCannotBeUsedForLiteralField"))
        ignore(GetString("ilUnexpectedGetSetAnnotation"))
        ignore(GetString("ilFieldOffsetAttributeCouldNotBeDecoded"))
        ignore(GetString("ilStructLayoutAttributeCouldNotBeDecoded"))
        ignore(GetString("ilDefaultAugmentationAttributeCouldNotBeDecoded"))
        ignore(GetString("ilReflectedDefinitionsCannotUseSliceOperator"))
        ignore(GetString("optsProblemWithCodepage"))
        ignore(GetString("optsCopyright"))
        ignore(GetString("optsNameOfOutputFile"))
        ignore(GetString("optsBuildConsole"))
        ignore(GetString("optsBuildWindows"))
        ignore(GetString("optsBuildLibrary"))
        ignore(GetString("optsBuildModule"))
        ignore(GetString("optsDelaySign"))
        ignore(GetString("optsWriteXml"))
        ignore(GetString("optsStrongKeyFile"))
        ignore(GetString("optsStrongKeyContainer"))
        ignore(GetString("optsPlatform"))
        ignore(GetString("optsNoOpt"))
        ignore(GetString("optsNoInterface"))
        ignore(GetString("optsSig"))
        ignore(GetString("optsReference"))
        ignore(GetString("optsWin32res"))
        ignore(GetString("optsWin32manifest"))
        ignore(GetString("optsNowin32manifest"))
        ignore(GetString("optsResource"))
        ignore(GetString("optsLinkresource"))
        ignore(GetString("optsDebugPM"))
        ignore(GetString("optsDebug"))
        ignore(GetString("optsOptimize"))
        ignore(GetString("optsTailcalls"))
        ignore(GetString("optsCrossoptimize"))
        ignore(GetString("optsWarnaserrorPM"))
        ignore(GetString("optsWarnaserror"))
        ignore(GetString("optsWarn"))
        ignore(GetString("optsNowarn"))
        ignore(GetString("optsWarnOn"))
        ignore(GetString("optsChecked"))
        ignore(GetString("optsDefine"))
        ignore(GetString("optsMlcompatibility"))
        ignore(GetString("optsNologo"))
        ignore(GetString("optsHelp"))
        ignore(GetString("optsCodepage"))
        ignore(GetString("optsUtf8output"))
        ignore(GetString("optsFullpaths"))
        ignore(GetString("optsLib"))
        ignore(GetString("optsBaseaddress"))
        ignore(GetString("optsNoframework"))
        ignore(GetString("optsStandalone"))
        ignore(GetString("optsStaticlink"))
        ignore(GetString("optsResident"))
        ignore(GetString("optsPdb"))
        ignore(GetString("optsSimpleresolution"))
        ignore(GetString("optsUnrecognizedTarget"))
        ignore(GetString("optsUnrecognizedDebugType"))
        ignore(GetString("optsInvalidWarningLevel"))
        ignore(GetString("optsShortFormOf"))
        ignore(GetString("optsClirootDeprecatedMsg"))
        ignore(GetString("optsClirootDescription"))
        ignore(GetString("optsHelpBannerOutputFiles"))
        ignore(GetString("optsHelpBannerInputFiles"))
        ignore(GetString("optsHelpBannerResources"))
        ignore(GetString("optsHelpBannerCodeGen"))
        ignore(GetString("optsHelpBannerAdvanced"))
        ignore(GetString("optsHelpBannerMisc"))
        ignore(GetString("optsHelpBannerLanguage"))
        ignore(GetString("optsHelpBannerErrsAndWarns"))
        ignore(GetString("optsUnknownArgumentToTheTestSwitch"))
        ignore(GetString("optsUnknownPlatform"))
        ignore(GetString("optsInternalNoDescription"))
        ignore(GetString("optsDCLONoDescription"))
        ignore(GetString("optsDCLODeprecatedSuggestAlternative"))
        ignore(GetString("optsDCLOHtmlDoc"))
        ignore(GetString("optsConsoleColors"))
        ignore(GetString("optsUseHighEntropyVA"))
        ignore(GetString("optsSubSystemVersion"))
        ignore(GetString("optsInvalidSubSystemVersion"))
        ignore(GetString("typeInfoFullName"))
        ignore(GetString("typeInfoType"))
        ignore(GetString("typeInfoInherits"))
        ignore(GetString("typeInfoImplements"))
        ignore(GetString("typeInfoOtherOverloads"))
        ignore(GetString("typeInfoUnionCase"))
        ignore(GetString("typeInfoActivePatternResult"))
        ignore(GetString("typeInfoActiveRecognizer"))
        ignore(GetString("typeInfoField"))
        ignore(GetString("typeInfoEvent"))
        ignore(GetString("typeInfoProperty"))
        ignore(GetString("typeInfoCustomOperation"))
        ignore(GetString("typeInfoArgument"))
        ignore(GetString("typeInfoPatternVariable"))
        ignore(GetString("typeInfoNamespace"))
        ignore(GetString("typeInfoModule"))
        ignore(GetString("typeInfoNamespaceOrModule"))
        ignore(GetString("typeInfoFromFirst"))
        ignore(GetString("typeInfoFromNext"))
        ignore(GetString("typeInfoGeneratedProperty"))
        ignore(GetString("typeInfoGeneratedType"))
        ignore(GetString("assemblyResolutionFoundByAssemblyFoldersKey"))
        ignore(GetString("assemblyResolutionFoundByAssemblyFoldersExKey"))
        ignore(GetString("assemblyResolutionNetFramework"))
        ignore(GetString("assemblyResolutionGAC"))
        ignore(GetString("recursiveClassHierarchy"))
        ignore(GetString("InvalidRecursiveReferenceToAbstractSlot"))
        ignore(GetString("eventHasNonStandardType"))
        ignore(GetString("typeIsNotAccessible"))
        ignore(GetString("unionCasesAreNotAccessible"))
        ignore(GetString("valueIsNotAccessible"))
        ignore(GetString("unionCaseIsNotAccessible"))
        ignore(GetString("fieldIsNotAccessible"))
        ignore(GetString("structOrClassFieldIsNotAccessible"))
        ignore(GetString("experimentalConstruct"))
        ignore(GetString("noInvokeMethodsFound"))
        ignore(GetString("moreThanOneInvokeMethodFound"))
        ignore(GetString("delegatesNotAllowedToHaveCurriedSignatures"))
        ignore(GetString("tlrUnexpectedTExpr"))
        ignore(GetString("tlrLambdaLiftingOptimizationsNotApplied"))
        ignore(GetString("lexhlpIdentifiersContainingAtSymbolReserved"))
        ignore(GetString("lexhlpIdentifierReserved"))
        ignore(GetString("patcMissingVariable"))
        ignore(GetString("patcPartialActivePatternsGenerateOneResult"))
        ignore(GetString("impTypeRequiredUnavailable"))
        ignore(GetString("impReferencedTypeCouldNotBeFoundInAssembly"))
        ignore(GetString("impNotEnoughTypeParamsInScopeWhileImporting"))
        ignore(GetString("impReferenceToDllRequiredByAssembly"))
        ignore(GetString("impImportedAssemblyUsesNotPublicType"))
        ignore(GetString("optValueMarkedInlineButIncomplete"))
        ignore(GetString("optValueMarkedInlineButWasNotBoundInTheOptEnv"))
        ignore(GetString("optLocalValueNotFoundDuringOptimization"))
        ignore(GetString("optValueMarkedInlineHasUnexpectedValue"))
        ignore(GetString("optValueMarkedInlineCouldNotBeInlined"))
        ignore(GetString("optFailedToInlineValue"))
        ignore(GetString("optRecursiveValValue"))
        ignore(GetString("lexfltIncorrentIndentationOfIn"))
        ignore(GetString("lexfltTokenIsOffsideOfContextStartedEarlier"))
        ignore(GetString("lexfltSeparatorTokensOfPatternMatchMisaligned"))
        ignore(GetString("nrInvalidModuleExprType"))
        ignore(GetString("nrTypeInstantiationNeededToDisambiguateTypesWithSameName"))
        ignore(GetString("nrTypeInstantiationIsMissingAndCouldNotBeInferred"))
        ignore(GetString("nrGlobalUsedOnlyAsFirstName"))
        ignore(GetString("nrIsNotConstructorOrLiteral"))
        ignore(GetString("nrUnexpectedEmptyLongId"))
        ignore(GetString("nrTypeDoesNotContainSuchField"))
        ignore(GetString("nrInvalidFieldLabel"))
        ignore(GetString("nrInvalidExpression"))
        ignore(GetString("nrNoConstructorsAvailableForType"))
        ignore(GetString("ilwriteErrorCreatingPdb"))
        ignore(GetString("lexOutsideIntegerRange"))
        ignore(GetString("lexCharNotAllowedInOperatorNames"))
        ignore(GetString("lexUnexpectedChar"))
        ignore(GetString("lexByteArrayCannotEncode"))
        ignore(GetString("lexIdentEndInMarkReserved"))
        ignore(GetString("lexOutsideEightBitSigned"))
        ignore(GetString("lexOutsideEightBitSignedHex"))
        ignore(GetString("lexOutsideEightBitUnsigned"))
        ignore(GetString("lexOutsideSixteenBitSigned"))
        ignore(GetString("lexOutsideSixteenBitUnsigned"))
        ignore(GetString("lexOutsideThirtyTwoBitSigned"))
        ignore(GetString("lexOutsideThirtyTwoBitUnsigned"))
        ignore(GetString("lexOutsideSixtyFourBitSigned"))
        ignore(GetString("lexOutsideSixtyFourBitUnsigned"))
        ignore(GetString("lexOutsideNativeSigned"))
        ignore(GetString("lexOutsideNativeUnsigned"))
        ignore(GetString("lexInvalidFloat"))
        ignore(GetString("lexOusideDecimal"))
        ignore(GetString("lexOusideThirtyTwoBitFloat"))
        ignore(GetString("lexInvalidNumericLiteral"))
        ignore(GetString("lexInvalidByteLiteral"))
        ignore(GetString("lexInvalidCharLiteral"))
        ignore(GetString("lexThisUnicodeOnlyInStringLiterals"))
        ignore(GetString("lexTokenReserved"))
        ignore(GetString("lexTabsNotAllowed"))
        ignore(GetString("lexInvalidLineNumber"))
        ignore(GetString("lexHashIfMustBeFirst"))
        ignore(GetString("lexHashElseNoMatchingIf"))
        ignore(GetString("lexHashEndifRequiredForElse"))
        ignore(GetString("lexHashElseMustBeFirst"))
        ignore(GetString("lexHashEndingNoMatchingIf"))
        ignore(GetString("lexHashEndifMustBeFirst"))
        ignore(GetString("lexHashIfMustHaveIdent"))
        ignore(GetString("lexWrongNestedHashEndif"))
        ignore(GetString("lexExpectedSingleLineComment"))
        ignore(GetString("memberOperatorDefinitionWithNoArguments"))
        ignore(GetString("memberOperatorDefinitionWithNonPairArgument"))
        ignore(GetString("memberOperatorDefinitionWithCurriedArguments"))
        ignore(GetString("tcFSharpCoreRequiresExplicit"))
        ignore(GetString("tcStructuralComparisonNotSatisfied1"))
        ignore(GetString("tcStructuralComparisonNotSatisfied2"))
        ignore(GetString("tcNoComparisonNeeded1"))
        ignore(GetString("tcNoComparisonNeeded2"))
        ignore(GetString("tcNoEqualityNeeded1"))
        ignore(GetString("tcNoEqualityNeeded2"))
        ignore(GetString("tcStructuralEqualityNotSatisfied1"))
        ignore(GetString("tcStructuralEqualityNotSatisfied2"))
        ignore(GetString("tcStructsMustDeclareTypesOfImplicitCtorArgsExplicitly"))
        ignore(GetString("chkUnusedValue"))
        ignore(GetString("chkUnusedThisVariable"))
        ignore(GetString("parsGetterAtMostOneArgument"))
        ignore(GetString("parsSetterAtMostTwoArguments"))
        ignore(GetString("parsInvalidProperty"))
        ignore(GetString("parsIndexerPropertyRequiresAtLeastOneArgument"))
        ignore(GetString("tastInvalidAddressOfMutableAcrossAssemblyBoundary"))
        ignore(GetString("parsNonAdjacentTypars"))
        ignore(GetString("parsNonAdjacentTyargs"))
        ignore(GetString("parsNonAtomicType"))
        ignore(GetString("tastUndefinedTyconItemField"))
        ignore(GetString("tastUndefinedTyconItemUnionCase"))
        ignore(GetString("tastUndefinedItemRefModuleNamespace"))
        ignore(GetString("tastUndefinedItemRefVal"))
        ignore(GetString("tastUndefinedItemRefModuleNamespaceType"))
        ignore(GetString("tcInvalidUseNullAsTrueValue"))
        ignore(GetString("tcParameterInferredByref"))
        ignore(GetString("tcNonUniformMemberUse"))
        ignore(GetString("tcNamedArgumentsCannotBeUsedInUnionCaseConstructions"))
        ignore(GetString("tcAttribArgsDiffer"))
        ignore(GetString("tcCannotCallAbstractBaseMember"))
        ignore(GetString("typrelCannotResolveAmbiguityInUnmanaged"))
        ignore(GetString("mlCompatMessage"))
        ignore(GetString("ilFieldDoesNotHaveValidOffsetForStructureLayout"))
        ignore(GetString("tcInterfacesShouldUseInheritNotInterface"))
        ignore(GetString("parsInvalidPrefixOperator"))
        ignore(GetString("parsInvalidPrefixOperatorDefinition"))
        ignore(GetString("buildCompilingExtensionIsForML"))
        ignore(GetString("lexIndentOffForML"))
        ignore(GetString("activePatternIdentIsNotFunctionTyped"))
        ignore(GetString("activePatternChoiceHasFreeTypars"))
        ignore(GetString("ilFieldHasOffsetForSequentialLayout"))
        ignore(GetString("tcOptionalArgsMustComeAfterNonOptionalArgs"))
        ignore(GetString("tcConditionalAttributeUsage"))
        ignore(GetString("tcMemberOperatorDefinitionInExtrinsic"))
        ignore(GetString("ilwriteMDBFileNameCannotBeChangedWarning"))
        ignore(GetString("ilwriteMDBMemberMissing"))
        ignore(GetString("ilwriteErrorCreatingMdb"))
        ignore(GetString("tcUnionCaseNameConflictsWithGeneratedType"))
        ignore(GetString("chkNoReflectedDefinitionOnStructMember"))
        ignore(GetString("tcDllImportNotAllowed"))
        ignore(GetString("buildExplicitCoreLibRequiresNoFramework"))
        ignore(GetString("buildExpectedSigdataFile"))
        ignore(GetString("buildDidNotExpectOptDataResource"))
        ignore(GetString("buildExpectedFileAlongSideFSharpCore"))
        ignore(GetString("buildDidNotExpectSigdataResource"))
        ignore(GetString("buildUnexpectedFileNameCharacter"))
        ignore(GetString("tcInvalidUseBangBinding"))
        ignore(GetString("crefNoInnerGenericsInQuotations"))
        ignore(GetString("tcEnumTypeCannotBeEnumerated"))
        ignore(GetString("parsEofInTripleQuoteString"))
        ignore(GetString("parsEofInTripleQuoteStringInComment"))
        ignore(GetString("tcTypeTestLosesMeasures"))
        ignore(GetString("parsMissingTypeArgs"))
        ignore(GetString("parsMissingGreaterThan"))
        ignore(GetString("parsUnexpectedQuotationOperatorInTypeAliasDidYouMeanVerbatimString"))
        ignore(GetString("parsErrorParsingAsOperatorName"))
        ignore(GetString("fscTooManyErrors"))
        ignore(GetString("docfileNoXmlSuffix"))
        ignore(GetString("fscNoImplementationFiles"))
        ignore(GetString("fscBadAssemblyVersion"))
        ignore(GetString("fscTwoResourceManifests"))
        ignore(GetString("fscQuotationLiteralsStaticLinking"))
        ignore(GetString("fscQuotationLiteralsStaticLinking0"))
        ignore(GetString("fscStaticLinkingNoEXE"))
        ignore(GetString("fscStaticLinkingNoMixedDLL"))
        ignore(GetString("fscIgnoringMixedWhenLinking"))
        ignore(GetString("fscAssumeStaticLinkContainsNoDependencies"))
        ignore(GetString("fscAssemblyNotFoundInDependencySet"))
        ignore(GetString("fscKeyFileCouldNotBeOpened"))
        ignore(GetString("fscProblemWritingBinary"))
        ignore(GetString("fscAssemblyVersionAttributeIgnored"))
        ignore(GetString("fscAssemblyCultureAttributeError"))
        ignore(GetString("fscDelaySignWarning"))
        ignore(GetString("fscKeyFileWarning"))
        ignore(GetString("fscKeyNameWarning"))
        ignore(GetString("fscReferenceOnCommandLine"))
        ignore(GetString("fscRemotingError"))
        ignore(GetString("pathIsInvalid"))
        ignore(GetString("fscResxSourceFileDeprecated"))
        ignore(GetString("etIllegalCharactersInNamespaceName"))
        ignore(GetString("etNullOrEmptyMemberName"))
        ignore(GetString("etNullMember"))
        ignore(GetString("etNullMemberDeclaringType"))
        ignore(GetString("etNullMemberDeclaringTypeDifferentFromProvidedType"))
        ignore(GetString("etHostingAssemblyFoundWithoutHosts"))
        ignore(GetString("etEmptyNamespaceOfTypeNotAllowed"))
        ignore(GetString("etEmptyNamespaceNotAllowed"))
        ignore(GetString("etMustNotBeGeneric"))
        ignore(GetString("etMustNotBeAnArray"))
        ignore(GetString("etMethodHasRequirements"))
        ignore(GetString("etUnsupportedMemberKind"))
        ignore(GetString("etPropertyCanReadButHasNoGetter"))
        ignore(GetString("etPropertyHasGetterButNoCanRead"))
        ignore(GetString("etPropertyCanWriteButHasNoSetter"))
        ignore(GetString("etPropertyHasSetterButNoCanWrite"))
        ignore(GetString("etOneOrMoreErrorsSeenDuringExtensionTypeSetting"))
        ignore(GetString("etUnexpectedExceptionFromProvidedTypeMember"))
        ignore(GetString("etUnsupportedConstantType"))
        ignore(GetString("etUnsupportedProvidedExpression"))
        ignore(GetString("etProvidedTypeHasUnexpectedName"))
        ignore(GetString("etEventNoAdd"))
        ignore(GetString("etEventNoRemove"))
        ignore(GetString("etProviderHasWrongDesignerAssembly"))
        ignore(GetString("etProviderDoesNotHaveValidConstructor"))
        ignore(GetString("etProviderError"))
        ignore(GetString("etIncorrectParameterExpression"))
        ignore(GetString("etIncorrectProvidedMethod"))
        ignore(GetString("etIncorrectProvidedConstructor"))
        ignore(GetString("etDirectReferenceToGeneratedTypeNotAllowed"))
        ignore(GetString("etProvidedTypeHasUnexpectedPath"))
        ignore(GetString("etUnexpectedNullFromProvidedTypeMember"))
        ignore(GetString("etUnexpectedExceptionFromProvidedMemberMember"))
        ignore(GetString("etNestedProvidedTypesDoNotTakeStaticArgumentsOrGenericParameters"))
        ignore(GetString("etInvalidStaticArgument"))
        ignore(GetString("etErrorApplyingStaticArgumentsToType"))
        ignore(GetString("etUnknownStaticArgumentKind"))
        ignore(GetString("invalidNamespaceForProvidedType"))
        ignore(GetString("invalidFullNameForProvidedType"))
        ignore(GetString("etProviderReturnedNull"))
        ignore(GetString("etTypeProviderConstructorException"))
        ignore(GetString("etNullProvidedExpression"))
        ignore(GetString("etProvidedAppliedTypeHadWrongName"))
        ignore(GetString("tcTypeTestLossy"))
        ignore(GetString("tcTypeCastErased"))
        ignore(GetString("tcTypeTestErased"))
        ignore(GetString("tcCannotInheritFromErasedType"))
        ignore(GetString("etInvalidTypeProviderAssemblyName"))
        ignore(GetString("tcInvalidMemberNameCtor"))
        ignore(GetString("tcInferredGenericTypeGivesRiseToInconsistency"))
        ignore(GetString("tcInvalidTypeArgumentCount"))
        ignore(GetString("tcCannotOverrideSealedMethod"))
        ignore(GetString("etProviderErrorWithContext"))
        ignore(GetString("etProvidedTypeWithNameException"))
        ignore(GetString("etProvidedTypeWithNullOrEmptyName"))
        ignore(GetString("etIllegalCharactersInTypeName"))
        ignore(GetString("tcJoinMustUseSimplePattern"))
        ignore(GetString("tcMissingCustomOperation"))
        ignore(GetString("etBadUnnamedStaticArgs"))
        ignore(GetString("etStaticParameterRequiresAValue"))
        ignore(GetString("etNoStaticParameterWithName"))
        ignore(GetString("etStaticParameterAlreadyHasValue"))
        ignore(GetString("etMultipleStaticParameterWithName"))
        ignore(GetString("tcCustomOperationMayNotBeUsedInConjunctionWithNonSimpleLetBindings"))
        ignore(GetString("tcCustomOperationMayNotBeUsedHere"))
        ignore(GetString("tcCustomOperationMayNotBeOverloaded"))
        ignore(GetString("tcTryFinallyMayNotBeUsedWithCustomOperators"))
        ignore(GetString("tcTryWithMayNotBeUsedWithCustomOperators"))
        ignore(GetString("tcIfThenElseMayNotBeUsedWithCustomOperators"))
        ignore(GetString("ilxgenUnexpectedArgumentToMethodHandleOfDuringCodegen"))
        ignore(GetString("etProvidedTypeReferenceMissingArgument"))
        ignore(GetString("etProvidedTypeReferenceInvalidText"))
        ignore(GetString("tcCustomOperationNotUsedCorrectly"))
        ignore(GetString("tcCustomOperationNotUsedCorrectly2"))
        ignore(GetString("customOperationTextLikeJoin"))
        ignore(GetString("customOperationTextLikeGroupJoin"))
        ignore(GetString("customOperationTextLikeZip"))
        ignore(GetString("tcBinaryOperatorRequiresVariable"))
        ignore(GetString("tcOperatorIncorrectSyntax"))
        ignore(GetString("tcBinaryOperatorRequiresBody"))
        ignore(GetString("tcCustomOperationHasIncorrectArgCount"))
        ignore(GetString("parsExpectedExpressionAfterToken"))
        ignore(GetString("parsExpectedTypeAfterToken"))
        ignore(GetString("parsUnmatchedLBrackLess"))
        ignore(GetString("parsUnexpectedEndOfFileMatch"))
        ignore(GetString("parsUnexpectedEndOfFileTry"))
        ignore(GetString("parsUnexpectedEndOfFileWhile"))
        ignore(GetString("parsUnexpectedEndOfFileFor"))
        ignore(GetString("parsUnexpectedEndOfFileWith"))
        ignore(GetString("parsUnexpectedEndOfFileThen"))
        ignore(GetString("parsUnexpectedEndOfFileElse"))
        ignore(GetString("parsUnexpectedEndOfFileFunBody"))
        ignore(GetString("parsUnexpectedEndOfFileTypeArgs"))
        ignore(GetString("parsUnexpectedEndOfFileTypeSignature"))
        ignore(GetString("parsUnexpectedEndOfFileTypeDefinition"))
        ignore(GetString("parsUnexpectedEndOfFileObjectMembers"))
        ignore(GetString("parsUnexpectedEndOfFileDefinition"))
        ignore(GetString("parsUnexpectedEndOfFileExpression"))
        ignore(GetString("parsExpectedNameAfterToken"))
        ignore(GetString("parsUnmatchedLet"))
        ignore(GetString("parsUnmatchedLetBang"))
        ignore(GetString("parsUnmatchedUseBang"))
        ignore(GetString("parsUnmatchedUse"))
        ignore(GetString("parsWhileDoExpected"))
        ignore(GetString("parsForDoExpected"))
        ignore(GetString("tcInvalidRelationInJoin"))
        ignore(GetString("typeInfoCallsWord"))
        ignore(GetString("impInvalidNumberOfGenericArguments"))
        ignore(GetString("impInvalidMeasureArgument1"))
        ignore(GetString("impInvalidMeasureArgument2"))
        ignore(GetString("etPropertyNeedsCanWriteOrCanRead"))
        ignore(GetString("tcIntoNeedsRestOfQuery"))
        ignore(GetString("tcOperatorDoesntAcceptInto"))
        ignore(GetString("tcCustomOperationInvalid"))
        ignore(GetString("tcThisTypeMayNotHaveACLIMutableAttribute"))
        ignore(GetString("tcAutoPropertyRequiresImplicitConstructionSequence"))
        ignore(GetString("parsMutableOnAutoPropertyShouldBeGetSet"))
        ignore(GetString("parsMutableOnAutoPropertyShouldBeGetSetNotJustSet"))
        ignore(GetString("chkNoByrefsOfByrefs"))
        ignore(GetString("etTypeProviderNotApproved"))
        ignore(GetString("tastopsMaxArrayFour"))
        ignore(GetString("tcNoIntegerForLoopInQuery"))
        ignore(GetString("tcNoWhileInQuery"))
        ignore(GetString("tcNoTryFinallyInQuery"))
        ignore(GetString("tcUseMayNotBeUsedInQueries"))
        ignore(GetString("tcBindMayNotBeUsedInQueries"))
        ignore(GetString("tcReturnMayNotBeUsedInQueries"))
        ignore(GetString("tcUnrecognizedQueryOperator"))
        ignore(GetString("tcTryWithMayNotBeUsedInQueries"))
        ignore(GetString("tcNonSimpleLetBindingInQuery"))
        ignore(GetString("etTooManyStaticParameters"))
        ignore(GetString("infosInvalidProvidedLiteralValue"))
        ignore(GetString("invalidPlatformTarget"))
        ignore(GetString("tcThisValueMayNotBeInlined"))
        ignore(GetString("etErasedTypeUsedInGeneration"))
        ignore(GetString("tcUnrecognizedQueryBinaryOperator"))
        ignore(GetString("invalidPlatformTargetForOldFramework"))
        ignore(GetString("crefNoSetOfHole"))
        ignore(GetString("nicePrintOtherOverloads1"))
        ignore(GetString("nicePrintOtherOverloadsN"))
        ignore(GetString("erasedTo"))
        ignore(GetString("parsUnfinishedExpression"))
        ignore(GetString("crefQuotationsCantContainByrefTypes"))
        ignore(GetString("parsAttributeOnIncompleteCode"))
        ignore(GetString("parsTypeNameCannotBeEmpty"))
        ignore(GetString("buildProblemReadingAssembly"))
        ignore(GetString("tcTPFieldMustBeLiteral"))
        ignore(GetString("loadingDescription"))
        ignore(GetString("descriptionUnavailable"))
        ignore(GetString("chkTyparMultipleClassConstraints"))
        ignore(GetString("tcMatchMayNotBeUsedWithQuery"))
        ignore(GetString("memberOperatorDefinitionWithNonTripleArgument"))
        ignore(GetString("cannotResolveNullableOperators"))
        ignore(GetString("tcOperatorRequiresIn"))
        ignore(GetString("parsIllegalMemberVarInObjectImplementation"))
        ignore(GetString("tcEmptyCopyAndUpdateRecordInvalid"))
        ignore(GetString("parsUnderscoreInvalidFieldName"))
        ignore(GetString("tcGeneratedTypesShouldBeInternalOrPrivate"))
        ignore(GetString("chkGetterAndSetterHaveSamePropertyType"))
        ignore(GetString("tcRuntimeSuppliedMethodCannotBeUsedInUserCode"))
        ()
