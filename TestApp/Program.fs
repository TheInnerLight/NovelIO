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
open System.Net
open System.Text

[<EntryPoint>]
let main argv = 
    let read3Tuple =
        ioformatter{
            let! a = BinaryIO.readFloat32()
            let! b = BinaryIO.readFloat64()
            let! c = BinaryIO.readInt16()
            return (a, b, c)
        }
    let validPath = File.assumeValid "test.txt"
    let handle = BinaryIO.createBinaryReadHandle validPath
    let test = BinaryIO.run read3Tuple handle
    match test with
    |IOSuccess res -> printfn "%A" res

    0

     