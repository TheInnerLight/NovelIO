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

    let fileReader = io {
        let! a = BinaryIO.readByte
        let! b = BinaryIO.readChar
        let! c = BinaryIO.readFloat32
        let! d = BinaryIO.readByte
        let! lst1 = IO.list (int d) (BinaryIO.readFloat)
        let! lst2 = IO.mapM [BinaryIO.readChar; BinaryIO.readChar; BinaryIO.readChar]
        return a, b, c, d, lst1, lst2
        }

    let result =  
        match BinaryIO.run "test.txt" fileReader with
        |IOSuccess (res, tok2) -> res
        |IOError e -> failwith "error"

    printfn "%A" result

    let token = BinaryIO.createToken "test.txt"

    let result, token2 = 
        match fileReader token with
        |IOSuccess (res, tok2) -> res, tok2
        |IOError e -> failwith "error"

    printfn "%A" result

    let result2 = 
        match fileReader token2 with
        |IOSuccess (res, _) -> res
        |IOError e -> failwith "error"

    printfn "%A" result2

    let result2 = 
        match fileReader token2 with
        |IOSuccess (res, _) -> res
        |IOError e -> failwith "error"

    printfn "%A" result2

    let result2 = 
        match fileReader token2 with
        |IOSuccess (res, _) -> res
        |IOError e -> failwith "error"

    printfn "%A" result2

    let a = 
        match fileReader token with
        |IOSuccess (res, _) -> res
        |IOError e -> failwith "error"

    printfn "%A" a

    let a = 
        match fileReader token with
        |IOSuccess (res, _) -> res
        |IOError e -> failwith "error"

    printfn "%A" a

    0

