// include Fake lib
#r @"packages/FAKE.4.10.3/tools/FakeLib.dll"
open Fake

let solution = "NovelIO.sln"
let testDir  = "./test/"

// Targets
Target "Clean" (fun _ ->
    CleanDirs ["bin"; "temp"])

Target "Build" (fun _ ->
    !! solution
    |> MSBuildRelease "" "Rebuild"
    |> ignore)

let testDlls = !! ("*Tests.dll")

Target "xUnitTest" (fun _ ->
    testDlls
        |> Fake.Testing.XUnit.xUnit (fun p -> 
            {p with 
                ShadowCopy = false;}))

Target "All" DoNothing

// Build order
"Clean"
  ==> "Build"
  ==> "xUnitTest"  // only if FAKE was called with parameter xUnitTest
  ==> "All"

// start build
RunTargetOrDefault "All"
