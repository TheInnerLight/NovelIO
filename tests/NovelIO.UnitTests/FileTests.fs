(*
   Copyright 2015-2016 Philip Curzon

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
open NovelFS.NovelIO.BinaryPickler
open FsCheck
open FsCheck.Xunit

type ``File Unit Tests``() =

    static let firstRandomFileThatDoesNotExist() =
        Seq.initInfinite (fun _ -> System.IO.Path.GetRandomFileName())
        |> Seq.find (not << System.IO.File.Exists)

    [<Property>]
    static member ``File.Path.fromValid returns a valid filename for a valid file path``() =
        let fnameStr = System.IO.Path.GetRandomFileName()
        let fname = File.Path.fromValid fnameStr // throws exception in failure case
        true

    [<Property>]
    static member ``File.Path.fromValid throws exception for an invalid file path``() =
        let invStr = string << Array.head <| System.IO.Path.GetInvalidFileNameChars()
        let fnameStr = System.IO.Path.GetRandomFileName() + invStr
        try
            let fname = File.Path.fromValid fnameStr // throws exception in failure case
            failwith "path was not expected to be valid"
        with
            | :? System.ArgumentException as aex -> true

    [<Property>]
    static member ``File.Open.defaultRead has FileMode.Open``() =
        match File.Open.defaultRead.FileMode with
        |FileMode.Open -> true
        |_ -> false

    [<Property>]
    static member ``File.Open.defaultRead has FileAccess.Read``() =
        match File.Open.defaultRead.FileAccess with
        |FileAccess.Read -> true
        |_ -> false

    [<Property>]
    static member ``File.Open.defaultReadWrite has FileMode.OpenOrCreate``() =
        match File.Open.defaultReadWrite.FileMode with
        |FileMode.OpenOrCreate -> true
        |_ -> false

    [<Property>]
    static member ``File.Open.defaultReadWrite has FileAccess.ReadWrite``() =
        match File.Open.defaultReadWrite.FileAccess with
        |FileAccess.ReadWrite -> true
        |_ -> false

    [<Property>]
    static member ``File.Open.defaultWrite has FileMode.Create``() =
        match File.Open.defaultWrite.FileMode with
        |FileMode.Create -> true
        |_ -> false

    [<Property>]
    static member ``File.Open.defaultWrite has FileAccess.ReadWrite``() =
        match File.Open.defaultWrite.FileAccess with
        |FileAccess.Write -> true
        |_ -> false

    [<Property>]
    static member ``ValidFilePath path disciminator matches for a valid file path``() =
        let fnameStr = System.IO.Path.GetRandomFileName()
        match fnameStr with
        |ValidFilePath fname -> true
        |InvalidFilePath -> failwith "path was expected not be invalid"

    [<Property>]
    static member ``ValidFilePath path disciminator matches for a invalid file path``() =
        let invStr = string << Array.head <| System.IO.Path.GetInvalidFileNameChars()
        let fnameStr = System.IO.Path.GetRandomFileName() + invStr
        match fnameStr with
        |ValidFilePath fname -> failwith "path was invalid"
        |InvalidFilePath -> true

    [<Property>]
    static member ``File.Path.pathString returns contained path string``() =
        let fnameStr = System.IO.Path.GetRandomFileName()
        let fname = File.Path.fromValid fnameStr
        File.Path.pathString fname = fnameStr

    [<Property>]
    static member ``File.creationTime returns correct date/time for test file``() =
        let fnameStr = "creationtimetest.txt"
        let fname = File.Path.fromValid fnameStr
        IO.run <| File.creationTime fname = System.IO.File.GetCreationTime fnameStr

    [<Property>]
    static member ``File.creationTimeUTC returns correct date/time for test file``() =
        let fnameStr = "creationtimetest.txt"
        let fname = File.Path.fromValid fnameStr
        IO.run <| File.creationTimeUTC fname = System.IO.File.GetCreationTimeUtc fnameStr

    [<Property>]
    static member ``File.lastAccessTime returns correct date/time for test file``() =
        let fnameStr = "creationtimetest.txt"
        let fname = File.Path.fromValid fnameStr
        IO.run <| File.lastAccessTime fname = System.IO.File.GetLastAccessTime fnameStr

    [<Property>]
    static member ``File.lastAccessTimeUTC returns correct date/time for test file``() =
        let fnameStr = "creationtimetest.txt"
        let fname = File.Path.fromValid fnameStr
        IO.run <| File.lastAccessTimeUTC fname = System.IO.File.GetLastAccessTimeUtc fnameStr

    [<Property>]
    static member ``File.lastWriteTime returns correct date/time for test file``() =
        let fnameStr = "creationtimetest.txt"
        let fname = File.Path.fromValid fnameStr
        IO.run <| File.lastWriteTime fname = System.IO.File.GetLastWriteTime fnameStr

    [<Property>]
    static member ``File.lastWriteTimeUTC returns correct date/time for test file``() =
        let fnameStr = "creationtimetest.txt"
        let fname = File.Path.fromValid fnameStr
        IO.run <| File.lastWriteTimeUTC fname = System.IO.File.GetLastWriteTimeUtc fnameStr

    [<Property>]
    static member ``File.setCreationTime sets correct date/time for test file`` (dt : System.DateTime) =
        let fnameStr = """creationtimetestwriting.txt"""
        let fname = File.Path.fromValid fnameStr
        IO.run <| File.setCreationTime dt fname
        dt = System.IO.File.GetCreationTime fnameStr

    [<Property>]
    static member ``File.setCreationTimeUTC sets correct date/time for test file`` (dt : System.DateTime) =
        let fnameStr = """creationtimetestwriting.txt"""
        let fname = File.Path.fromValid fnameStr
        IO.run <| File.setCreationTimeUTC dt fname
        dt = System.IO.File.GetCreationTimeUtc fnameStr

    [<Property>]
    static member ``File.setLastAccessTime sets correct date/time for test file`` (dt : System.DateTime) =
        let fnameStr = """creationtimetestwriting.txt"""
        let fname = File.Path.fromValid fnameStr
        IO.run <| File.setLastAccessTime dt fname 
        dt = System.IO.File.GetLastAccessTime fnameStr

    [<Property>]
    static member ``File.setLastAccessTimeUTC sets correct date/time for test file`` (dt : System.DateTime) =
        let fnameStr = """creationtimetestwriting.txt"""
        let fname = File.Path.fromValid fnameStr
        IO.run <| File.setLastAccessTimeUTC dt fname 
        dt = System.IO.File.GetLastAccessTimeUtc fnameStr

    [<Property>]
    static member ``File.setLastWriteTime sets correct date/time for test file`` (dt : System.DateTime) =
        let fnameStr = """creationtimetestwriting.txt"""
        let fname = File.Path.fromValid fnameStr
        IO.run <| File.setLastWriteTime dt fname 
        dt = System.IO.File.GetLastWriteTime fnameStr

    [<Property>]
    static member ``File.setLastWriteTimeUTC sets correct date/time for test file`` (dt : System.DateTime) =
        let fnameStr = """creationtimetestwriting.txt"""
        let fname = File.Path.fromValid fnameStr
        IO.run <| File.setLastWriteTimeUTC dt fname 
        dt = System.IO.File.GetLastWriteTimeUtc fnameStr

    [<Property>]
    static member ``File that does not exist is not found``() =
        let fnameStr = firstRandomFileThatDoesNotExist()
        let fname = File.Path.fromValid fnameStr
        not << IO.run <| File.exists fname

    [<Property>]
    static member ``File that does exist can be found``() =
        let fnameStr = firstRandomFileThatDoesNotExist()
        System.IO.File.WriteAllLines(fnameStr, [|""|])
        try
            let fname = File.Path.fromValid fnameStr
            IO.run <| File.exists fname
        finally
            System.IO.File.Delete fnameStr

    [<Property>]
    static member ``Random file can be deleted``() =
        let fnameStr = firstRandomFileThatDoesNotExist()
        let fname = File.Path.fromValid fnameStr
        System.IO.File.AppendAllLines(fnameStr, [""])
        IO.run <| File.delete fname
        not <| System.IO.File.Exists fnameStr

    [<Property>]
    static member ``Random file can be copied``() =
        let fnameStr = firstRandomFileThatDoesNotExist()
        let fname = File.Path.fromValid fnameStr
        System.IO.File.AppendAllLines(fnameStr, [""])
        let fnameStr2 = firstRandomFileThatDoesNotExist()
        let fname2 = File.Path.fromValid fnameStr2
        IO.run <| File.copy fname fname2 
        try
            System.IO.File.Exists fnameStr2
        finally
            System.IO.File.Delete fnameStr
            System.IO.File.Delete fnameStr2


    [<Property>]
    static member ``Random file can be moved``() =
        let fnameStr = firstRandomFileThatDoesNotExist()
        let fname = File.Path.fromValid fnameStr
        System.IO.File.AppendAllLines(fnameStr, [""])
        let fnameStr2 = firstRandomFileThatDoesNotExist()
        let fname2 = File.Path.fromValid fnameStr2
        try
            IO.run <| File.move fname fname2 
            try
                System.IO.File.Exists fnameStr2
            finally
                System.IO.File.Delete fnameStr2
        with
            |_ -> 
                System.IO.File.Delete fnameStr
                reraise ()
