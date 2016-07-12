namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("NovelIO")>]
[<assembly: AssemblyProductAttribute("NovelIO")>]
[<assembly: AssemblyDescriptionAttribute("Functional IO Library")>]
[<assembly: AssemblyVersionAttribute("0.3.0")>]
[<assembly: AssemblyFileVersionAttribute("0.3.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.3.0"
    let [<Literal>] InformationalVersion = "0.3.0"
