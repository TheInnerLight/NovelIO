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

namespace NovelFS.NovelIO.UnitTests

open NovelFS.NovelIO
open NovelFS.NovelIO.BinaryParser
open FsCheck
open FsCheck.Xunit

type ``File Unit Tests`` =

    [<Property>]
    static member ``Function: getPathString returns contained path string``() =
        let fnameStr = System.IO.Path.GetRandomFileName()
        let fname = File.assumeValidFilename fnameStr
        File.getPathString fname = fnameStr

    [<Property>]
    static member ``Function: creationTime returns correct date/time for test file``() =
        let fnameStr = "creationtimetest.txt"
        let fname = File.assumeValidFilename fnameStr
        match IO.run <| File.creationTime fname with
        |IOSuccess dt -> dt = System.IO.File.GetCreationTime fnameStr
        |IOError err -> failwith "error"

    [<Property>]
    static member ``Function: creationTimeUTC returns correct date/time for test file``() =
        let fnameStr = "creationtimetest.txt"
        let fname = File.assumeValidFilename fnameStr
        match IO.run <| File.creationTimeUTC fname with
        |IOSuccess dt -> dt = System.IO.File.GetCreationTimeUtc fnameStr
        |IOError err -> failwith "error"

    [<Property>]
    static member ``Function: lastAccessTime returns correct date/time for test file``() =
        let fnameStr = "creationtimetest.txt"
        let fname = File.assumeValidFilename fnameStr
        match IO.run <| File.lastAccessTime fname with
        |IOSuccess dt -> dt = System.IO.File.GetLastAccessTime fnameStr
        |IOError err -> failwith "error"

    [<Property>]
    static member ``Function: lastAccessTimeUTC returns correct date/time for test file``() =
        let fnameStr = "creationtimetest.txt"
        let fname = File.assumeValidFilename fnameStr
        match IO.run <| File.lastAccessTimeUTC fname with
        |IOSuccess dt -> dt = System.IO.File.GetLastAccessTimeUtc fnameStr
        |IOError err -> failwith "error"

    [<Property>]
    static member ``Function: lastWriteTime returns correct date/time for test file``() =
        let fnameStr = "creationtimetest.txt"
        let fname = File.assumeValidFilename fnameStr
        match IO.run <| File.lastWriteTime fname with
        |IOSuccess dt -> dt = System.IO.File.GetLastWriteTime fnameStr
        |IOError err -> failwith "error"

    [<Property>]
    static member ``Function: lastWriteTimeUTC returns correct date/time for test file``() =
        let fnameStr = "creationtimetest.txt"
        let fname = File.assumeValidFilename fnameStr
        match IO.run <| File.lastWriteTimeUTC fname with
        |IOSuccess dt -> dt = System.IO.File.GetLastWriteTimeUtc fnameStr
        |IOError err -> failwith "error"

    [<Property>]
    static member ``File that does not exist is not found``() =
        let fnameStr = 
            Seq.initInfinite (fun _ -> System.IO.Path.GetRandomFileName())
            |> Seq.find (not << System.IO.File.Exists)
        let fname = File.assumeValidFilename fnameStr
        match IO.run <| File.fileExists fname with
        |IOSuccess b -> b = false
        |IOError err -> failwith "error"

    [<Property>]
    static member ``File that does exist can be found``() =
        let fnameStr = 
            Seq.initInfinite (fun _ -> System.IO.Path.GetRandomFileName())
            |> Seq.find (not << System.IO.File.Exists)
        System.IO.File.WriteAllLines(fnameStr, [|""|])
        try
            let fname = File.assumeValidFilename fnameStr
            match IO.run <| File.fileExists fname with
            |IOSuccess b -> b = true
            |IOError err -> failwith "error"
        finally
            System.IO.File.Delete fnameStr