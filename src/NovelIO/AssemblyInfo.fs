namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("NovelIO")>]
[<assembly: AssemblyProductAttribute("NovelIO")>]
[<assembly: AssemblyDescriptionAttribute("Functional IO Library")>]
[<assembly: AssemblyVersionAttribute("0.1.1")>]
[<assembly: AssemblyFileVersionAttribute("0.1.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.1.1"
    let [<Literal>] InformationalVersion = "0.1.1"
