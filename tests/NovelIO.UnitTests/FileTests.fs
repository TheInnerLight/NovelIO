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

type ``File Unit Tests``() =

    static let firstRandomFileThatDoesNotExist() =
        Seq.initInfinite (fun _ -> System.IO.Path.GetRandomFileName())
        |> Seq.find (not << System.IO.File.Exists)

    [<Property>]
    static member ``Function: assumeValidFilename returns a valid filename for a valid file path``() =
        let fnameStr = System.IO.Path.GetRandomFileName()
        let fname = File.assumeValidFilename fnameStr // throws exception in failure case
        true

    [<Property>]
    static member ``Function: assumeValidFilename throws exception for an invalid file path``() =
        let invStr = string << Array.head <| System.IO.Path.GetInvalidFileNameChars()
        let fnameStr = System.IO.Path.GetRandomFileName() + invStr
        try
            let fname = File.assumeValidFilename fnameStr // throws exception in failure case
            failwith "path was not expected to be valid"
        with
            | :? System.ArgumentException as aex -> true

    [<Property>]
    static member ``ValidFilename path disciminator matches for a valid file path``() =
        let fnameStr = System.IO.Path.GetRandomFileName()
        match fnameStr with
        |ValidFilename fname -> true
        |InvalidFilename -> failwith "path was expected not be invalid"

    [<Property>]
    static member ``InvalidFilename path disciminator matches for a invalid file path``() =
        let invStr = string << Array.head <| System.IO.Path.GetInvalidFileNameChars()
        let fnameStr = System.IO.Path.GetRandomFileName() + invStr
        match fnameStr with
        |ValidFilename fname -> failwith "path was invalid"
        |InvalidFilename -> true

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
    static member ``Function: setCreationTime sets correct date/time for test file`` (dt : System.DateTime) =
        let fnameStr = """creationtimetestwriting.txt"""
        let fname = File.assumeValidFilename fnameStr
        match IO.run <| File.setCreationTime dt fname with
        |IOSuccess _ -> dt = System.IO.File.GetCreationTime fnameStr
        |IOError err -> failwith "error"

    [<Property>]
    static member ``Function: setCreationTimeUTC sets correct date/time for test file`` (dt : System.DateTime) =
        let fnameStr = """creationtimetestwriting.txt"""
        let fname = File.assumeValidFilename fnameStr
        match IO.run <| File.setCreationTimeUTC dt fname with
        |IOSuccess _ -> dt = System.IO.File.GetCreationTimeUtc fnameStr
        |IOError err -> failwith "error"

    [<Property>]
    static member ``Function: setLastAccessTime sets correct date/time for test file`` (dt : System.DateTime) =
        let fnameStr = """creationtimetestwriting.txt"""
        let fname = File.assumeValidFilename fnameStr
        match IO.run <| File.setLastAccessTime dt fname with
        |IOSuccess _ -> dt = System.IO.File.GetLastAccessTime fnameStr
        |IOError err -> failwith "error"

    [<Property>]
    static member ``Function: setLastAccessTimeUTC sets correct date/time for test file`` (dt : System.DateTime) =
        let fnameStr = """creationtimetestwriting.txt"""
        let fname = File.assumeValidFilename fnameStr
        match IO.run <| File.setLastAccessTimeUTC dt fname with
        |IOSuccess _ -> dt = System.IO.File.GetLastAccessTimeUtc fnameStr
        |IOError err -> failwith "error"

    [<Property>]
    static member ``Function: setLastWriteTime sets correct date/time for test file`` (dt : System.DateTime) =
        let fnameStr = """creationtimetestwriting.txt"""
        let fname = File.assumeValidFilename fnameStr
        match IO.run <| File.setLastWriteTime dt fname with
        |IOSuccess _ -> dt = System.IO.File.GetLastWriteTime fnameStr
        |IOError err -> failwith "error"

    [<Property>]
    static member ``Function: setLastWriteTimeUTC sets correct date/time for test file`` (dt : System.DateTime) =
        let fnameStr = """creationtimetestwriting.txt"""
        let fname = File.assumeValidFilename fnameStr
        match IO.run <| File.setLastWriteTimeUTC dt fname with
        |IOSuccess _ -> dt = System.IO.File.GetLastWriteTimeUtc fnameStr
        |IOError err -> failwith "error"

    [<Property>]
    static member ``File that does not exist is not found``() =
        let fnameStr = firstRandomFileThatDoesNotExist()
        let fname = File.assumeValidFilename fnameStr
        match IO.run <| File.exists fname with
        |IOSuccess b -> b = false
        |IOError err -> failwith "error"

    [<Property>]
    static member ``File that does exist can be found``() =
        let fnameStr = firstRandomFileThatDoesNotExist()
        System.IO.File.WriteAllLines(fnameStr, [|""|])
        try
            let fname = File.assumeValidFilename fnameStr
            match IO.run <| File.exists fname with
            |IOSuccess b -> b = true
            |IOError err -> failwith "error"
        finally
            System.IO.File.Delete fnameStr

    [<Property>]
    static member ``Random file can be deleted``() =
        let fnameStr = firstRandomFileThatDoesNotExist()
        let fname = File.assumeValidFilename fnameStr
        System.IO.File.AppendAllLines(fnameStr, [""])
        match IO.run <| File.delete fname with
        |IOSuccess _ -> not <| System.IO.File.Exists fnameStr
        |IOError err -> failwith "error"

    [<Property>]
    static member ``Random file can be copied``() =
        let fnameStr = firstRandomFileThatDoesNotExist()
        let fname = File.assumeValidFilename fnameStr
        System.IO.File.AppendAllLines(fnameStr, [""])
        let fnameStr2 = firstRandomFileThatDoesNotExist()
        let fname2 = File.assumeValidFilename fnameStr2
        match IO.run <| File.copy fname fname2 with
        |IOSuccess _ -> 
            try
                System.IO.File.Exists fnameStr2
            finally
                System.IO.File.Delete fnameStr
                System.IO.File.Delete fnameStr2
        |IOError err ->
            System.IO.File.Delete fnameStr
            failwith "error"

    [<Property>]
    static member ``Random file can be moved``() =
        let fnameStr = firstRandomFileThatDoesNotExist()
        let fname = File.assumeValidFilename fnameStr
        System.IO.File.AppendAllLines(fnameStr, [""])
        let fnameStr2 = firstRandomFileThatDoesNotExist()
        let fname2 = File.assumeValidFilename fnameStr2
        match IO.run <| File.move fname fname2 with
        |IOSuccess _ -> 
            try
                System.IO.File.Exists fnameStr2
            finally
                System.IO.File.Delete fnameStr2
        |IOError err ->
            System.IO.File.Delete fnameStr
            failwith "error"