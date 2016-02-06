namespace NovelFS.NovelIO.IntegrationTests

open NovelFS.NovelIO
open NovelFS.NovelIO.BinaryParser
open FsCheck
open FsCheck.Xunit

type ``IO Integration Tests`` =

    [<Property>]
    static member ``Read All Bytes from file`` (bytes : byte[]) =
        let fnameStr = "readbytestest.tst"
        System.IO.File.WriteAllBytes(fnameStr, bytes)
        let fname = File.assumeValidFilename fnameStr
        match IO.run <| File.readAllBytes fname with
        |IOSuccess readBytes ->
            readBytes = bytes
        |IOError err -> failwith "error"

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
                return! lineSeq |> List.ofSeq |> IO.listM
            }
        match IO.run lineIO with
        |IOSuccess lines ->
            lines = lstStrs
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

