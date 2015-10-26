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

    let newTest1 = BinaryReadFormatter.readChar
    let newTest2 = BinaryReadFormatter.readFloat
    let newTest3 = BinaryReadFormatter.readChar
    let newTestTup = IO.tuple3 newTest1 newTest2 newTest3
    let newTestTup2 = IO.tuple3 newTestTup newTestTup newTestTup
    let newListTest = IO.list 5 newTest3

    let testTxtIo = io (BinaryIO.createToken "test.txt") (BinaryIO.destroyToken) 
    let res = testTxtIo {
        let! result1 = BinaryIO.read newTestTup2
        let! result2 = BinaryIO.read newTestTup2
        let! result3 = BinaryIO.read newTestTup2
        let! result4 = BinaryIO.read newTestTup2
        let! result5 = BinaryIO.read newTestTup2
        return (result1, result2, result3, result4, result5)
        }

    printfn "%A" res

    let token = BinaryIO.createToken "test.txt"
    let result = BinaryIO.read newTestTup2 token
    match result with
    |IOSuccess (res, token2) ->
        printfn "%A" res
        let result2 = BinaryIO.read newTestTup2 token2
        let result3 = BinaryIO.read newTestTup2 token
        let result4 = BinaryIO.read newTestTup2 token2
        printfn "%A" result2
        printfn "%A" result3
        printfn "%A" result4
    |IOError _ -> printfn "Error reading from file."
    0

