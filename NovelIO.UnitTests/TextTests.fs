namespace NovelFS.NovelIO.UnitTests

open NovelFS.NovelIO
open FsCheck
open FsCheck.Xunit

type ``Text IO Simple Read Tests`` =
    [<Property>]
    static member ``readLine from string`` (str : NonNull<string>) =
        let str = str.Get + System.Environment.NewLine
        let bytes = System.Text.Encoding.UTF8.GetBytes str
        use memoryStream = new System.IO.MemoryStream(bytes)
        use reader = new System.IO.StreamReader(memoryStream);
        let str = reader.ReadLine()
        let result = 
            match TextIO.run (MemoryBlockRead (bytes, TextIO.readLine)) with
            |IOSuccess (strR,_) -> strR
            |IOError (_) -> failwith "error"
        result = str
    [<Property>]
    static member ``readToEnd from string`` (str : NonNull<string>) =
        let str = str.Get
        let bytes = System.Text.Encoding.UTF8.GetBytes str
        let result = 
            match TextIO.run (MemoryBlockRead (bytes, TextIO.readToEnd)) with
            |IOSuccess (strR,_) -> strR
            |IOError (_) -> failwith "error"
        result = str

