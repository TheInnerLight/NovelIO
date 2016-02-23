namespace NovelFS.NovelIO.UnitTests

//open NovelFS.NovelIO
//open FsCheck
//open FsCheck.Xunit
//
//type ``Text IO Simple Read Tests`` =
//    [<Property>]
//    static member ``readLine from string`` (str : NonNull<string>) =
//        let str = str.Get + System.Environment.NewLine
//        let bytes = System.Text.Encoding.UTF8.GetBytes str
//        use memoryStream = new System.IO.MemoryStream(bytes)
//        use reader = new System.IO.StreamReader(memoryStream);
//        let str = reader.ReadLine()
//        let result = 
//            match TextIO.run (MemoryBlockRead (bytes, TextIO.readLine)) with
//            |IOSuccess (strR,_) -> strR
//            |IOError (_) -> failwith "error"
//        result = str
//    [<Property>]
//    static member ``readToEnd from string`` (str : NonNull<string>) =
//        let str = str.Get
//        let bytes = System.Text.Encoding.UTF8.GetBytes str
//        let result = 
//            match TextIO.run (MemoryBlockRead (bytes, TextIO.readToEnd)) with
//            |IOSuccess (strR,_) -> strR
//            |IOError (_) -> failwith "error"
//        result = str
//type ``Text IO Simple Write Tests`` =
//    [<Property>]
//    static member ``writeLine from string`` (str : NonNull<string>) =
//        let firstLine = str.Get
//        let mmapFID = System.IO.Path.GetRandomFileName()
//        match TextIO.run (MemoryMappedFileWrite (mmapFID, 1024L, TextIO.writeLine firstLine)) with
//        |IOSuccess (_) -> 
//            let mmapf = System.IO.MemoryMappedFiles.MemoryMappedFile.OpenExisting(mmapFID)
//            use reader = new System.IO.StreamReader(mmapf.CreateViewStream())
//            let mmapfText = reader.ReadLine()
//            mmapfText = firstLine.Split('\r','\n').[0]
//        |IOError (_) -> failwith "error"
//    [<Property>]
//    static member ``write from string`` (str : NonNull<string>) =
//        let text = str.Get
//        let mmapFID = System.IO.Path.GetRandomFileName()
//        match TextIO.run (MemoryMappedFileWrite (mmapFID, 1024L, TextIO.write text)) with
//        |IOSuccess (_) -> 
//            let mmapf = System.IO.MemoryMappedFiles.MemoryMappedFile.OpenExisting(mmapFID)
//            use reader = new System.IO.StreamReader(mmapf.CreateViewStream())
//            let mmapfText = new System.String(reader.ReadToEnd() |> Seq.take (text.Length) |> Array.ofSeq)
//            mmapfText = text
//        |IOError (_) -> failwith "error"
