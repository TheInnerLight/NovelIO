namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("NovelIO")>]
[<assembly: AssemblyProductAttribute("NovelIO")>]
[<assembly: AssemblyDescriptionAttribute("F# Functional IO Library")>]
[<assembly: AssemblyVersionAttribute("0.1.0")>]
[<assembly: AssemblyFileVersionAttribute("0.1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.1.0"
