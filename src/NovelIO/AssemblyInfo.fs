namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("NovelIO")>]
[<assembly: AssemblyProductAttribute("NovelIO")>]
[<assembly: AssemblyDescriptionAttribute("F# Functional IO Library")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
