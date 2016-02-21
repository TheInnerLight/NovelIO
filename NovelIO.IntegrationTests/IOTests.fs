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

    

