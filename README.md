This is FSharp.AbsIL, a tool kit for analyzing and creating CIL. It is used as the F# compilers backend. It uses the Apache 2.0 license.

FSharp.AbsIL is a toolkit very similar to the excellent [mono.cecil](https://github.com/jbevain/cecil) for parsing or creating CIL file 
(i.e. .NET assemblies / .dlls and .exes). As FSharp.AbsIL uses F# union types to represent CIL F# users wishing to analyze files containing CIL 
may find it easier to use than mono.cecil as union types work very well with F# powerful pattern matching features.

## Samples

clrish - this is an attempt to create a version of the CLR implemented in F# by parsing the assemblies with FSharp.AbsIL, the interpreting the instructions. 
It's not intended to be useful, it was started as a learning exercise. As it became clear it would take a little longer than the couple of hours
I originally estimated it at, I stopped work on it for the moment, till I have more free time.

## History 

FSharp.AbsIL was originally a separate research project by Don Syme at MSR Cambridge, as time went by it became clear that F# was the more interesting/important
research area, so FSharp.AbsIL was rolled into the F# compiler. Now the compiler sources are available under an open source license I have taken advatage of this
to extract FSharp.AbsIL into a seperate project again.

F# compiler sources dropped by Microsoft are available from [fsharppowerpack.codeplex.com](http://fsharppowerpack.codeplex.com).


