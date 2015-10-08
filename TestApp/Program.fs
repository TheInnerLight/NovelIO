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

    let newTest1 = BinaryReading.readByte
    let newTest2 = BinaryReading.readByte
    let newTest3 = BinaryReading.readByte
    let newTestTup = IO.tuple3 newTest1 newTest2 newTest3
    let newTestTup2 = IO.tuple3 newTestTup newTestTup newTestTup

    let token = BinaryReading.createToken "test.txt"
    let result = BinaryReading.read newTestTup2 token
    match result with
    |IOSuccess (res, token2) ->
        printfn "%A" res
        let result2 = BinaryReading.read newTestTup2 token2
        let result3 = BinaryReading.read newTestTup2 token
        let result4 = BinaryReading.read newTestTup2 token2
        printfn "%A" result2
        printfn "%A" result3
        printfn "%A" result4
    |IOError _ -> printfn "Errroooooooorrrrrrr"

    //let bytes = BinaryReading.apply "test.txt" newTestTup
    //printfn "%A" result

    let iotest = BinaryReaderExpr.createFileReaderBuilder "test.txt"
    let result2 =
        iotest{
            let! test1 = BinaryReader.readChar
            let! test2 = BinaryReader.readChar
            let! test3 = BinaryReader.readChar
            let! test4 = BinaryReader.readChar
            return! (GeneralIO.remaining (GeneralIO.tuple3 BinaryReader.readChar BinaryReader.readChar BinaryReader.readChar)) 
            //return (test1, test2, test3, test4, test5)
        }
    match result2 with
    |IOSuccess (res, _) -> printfn "%A" res
    |IOError error -> printfn "%A" error
    0 // return an integer exit code
