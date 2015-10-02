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

    let iotest = BinaryReadingExpressions.createReaderBuilder "test.txt"
    let result2 =
        iotest{
            let! test1 = BinaryReading.readChar
            let! test2 = BinaryReading.readChar
            let! test3 = BinaryReading.readChar
            let! test4 = BinaryReading.readChar
            let! test5 = GeneralIO.listOfN (BinaryReading.readChar) 76
            return (test1, test2, test3, test4, test5)
        }
    match result2 with
    |IOSuccess (res, _) -> printfn "%A" res
    |IOError error -> printfn "%A" error
    0 // return an integer exit code
