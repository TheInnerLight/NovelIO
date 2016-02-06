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
