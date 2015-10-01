(*
   Copyright 2015 Philip Curzon

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

open NovelFS.NovelIO

[<EntryPoint>]
let main argv = 
    let result = BinaryReading.createBinaryReadToken "test.txt"
    match result with
    |IOSuccess (_, token) -> 
        let test = 
            token |> GeneralIO.listOfN (GeneralIO.tuple3 BinaryReading.readByte BinaryReading.readByte BinaryReading.readByte) 5
        match test with
        |IOSuccess (res, _) -> printfn "%A" res
        |IOError error -> printfn "%A" error
    |IOError error -> printfn "%A" error
    printfn "%A" argv
    0 // return an integer exit code
