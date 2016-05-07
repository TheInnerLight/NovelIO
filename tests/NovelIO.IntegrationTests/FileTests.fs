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

namespace NovelFS.NovelIO.IntegrationTests

open NovelFS.NovelIO
open NovelFS.NovelIO.BinaryPickler
open FsCheck
open FsCheck.Xunit

type ``File Integration Tests``() =

    [<Property>]
    static member ``Read All Bytes from file`` (bytes : byte[]) =
        let fnameStr = "readbytestest.tst"
        System.IO.File.WriteAllBytes(fnameStr, bytes)
        let fname = File.assumeValidFilename fnameStr
        IO.run <| File.readAllBytes fname = bytes

    [<Property>]
    static member ``Read lines from file`` (strA : NonEmptyArray<NonEmptyString>) =
        let fnameStr = "readlinestest.tst"
        let lstStrs = 
            strA.Get 
            |> Array.collect (fun str -> str.Get.Split('\r','\n'))
            |> List.ofArray 
        System.IO.File.WriteAllLines(fnameStr, lstStrs)
        let fname = File.assumeValidFilename fnameStr
        let lineIO =
            io {
                let! lineSeq = File.readLines fname
                let! uwSeq = IO.sequence lineSeq
                return List.ofSeq uwSeq
            }
        IO.run lineIO = lstStrs

    [<Property>]
    static member ``Read all lines from file`` (strA : NonEmptyArray<NonEmptyString>) =
        let fnameStr = "readlinestest.tst"
        let lstStrs = 
            strA.Get 
            |> Array.collect (fun str -> str.Get.Split('\r','\n'))
            |> List.ofArray 
        System.IO.File.WriteAllLines(fnameStr, lstStrs)
        let fname = File.assumeValidFilename fnameStr
        let lineIO =
            io {
                return! File.readAllLines fname
            }
        IO.run lineIO  = lstStrs

    

