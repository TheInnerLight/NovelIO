// include Fake lib
#r @"packages/FAKE.4.10.3/tools/FakeLib.dll"
open Fake
open Fake.Testing

let solution = "NovelIO.sln"
let testDir  = "test/"

// Targets
Target "Clean" (fun _ ->
    CleanDirs ["bin"; "temp"])

Target "Build" (fun _ ->
    !! solution
    |> MSBuildRelease "" "Rebuild"
    |> ignore)

Target "xUnitTest" (fun _ ->
    !! ("**/xUnit.Test.*.dll") 
        -- ("**/obj/**/*.dll")
        |> xUnit (fun p -> 
            {p with 
                 ToolPath = "packages/xunit.runner.console.2.1.0/tools/xunit.console.exe"}))

Target "All" DoNothing

// Build order
"Clean"
  ==> "Build"
  ==> "xUnitTest"  // only if FAKE was called with parameter xUnitTest
  ==> "All"

// start build
RunTargetOrDefault "All"
