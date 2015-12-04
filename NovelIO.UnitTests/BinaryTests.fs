namespace NovelFS.NovelIO.UnitTests

open NovelFS.NovelIO
open FsCheck
open FsCheck.Xunit

type ``Binary IO Simple Read Tests`` =
    [<Property>]
    static member ``readByte from array of one byte`` (byte : byte) =
        let bytes = [|byte|]
        let result = 
            match BinaryIO.run (MemoryBlockRead (bytes, BinaryIO.readByte)) with
            |IOSuccess (byte,_) -> byte
            |IOError (_) -> failwith "error"
        result = bytes.[0]
    [<Property>]
    static member ``readCharfrom array of one byte`` (character : char) =
        let bytes = System.BitConverter.GetBytes(character)
        let result = 
            match BinaryIO.run (MemoryBlockRead (bytes, BinaryIO.readByte)) with
            |IOSuccess (byte,_) -> character
            |IOError (_) -> failwith "error"
        result = character
    [<Property>]
    static member ``readDecimal from array of one decimal`` (dec : decimal) =
        let memoryStream = new System.IO.MemoryStream()
        let binaryWriter = new System.IO.BinaryWriter(memoryStream);
        binaryWriter.Write(dec)
        let bytes = memoryStream.ToArray()
        let result = 
            match BinaryIO.run (MemoryBlockRead (bytes, BinaryIO.readDecimal)) with
            |IOSuccess (dec,_) -> dec
            |IOError (_) -> failwith "error"
        result = dec
    [<Property>]
    static member ``readFloat from array of one float`` (flt : float) =
        let bytes = System.BitConverter.GetBytes flt
        let result = 
            match BinaryIO.run (MemoryBlockRead (bytes, BinaryIO.readFloat)) with
            |IOSuccess (flt,_) -> flt
            |IOError (_) -> failwith "error"
        match flt with
        |_ when System.Double.IsNaN flt -> System.Double.IsNaN result
        |_ -> result = flt
    [<Property>]
    static member ``readFloat32 from array of one float32`` (flt : float32) =
        let bytes = System.BitConverter.GetBytes flt
        let result = 
            match BinaryIO.run (MemoryBlockRead (bytes, BinaryIO.readFloat32)) with
            |IOSuccess (flt,_) -> flt
            |IOError (_) -> failwith "error"
        match flt with
        |_ when System.Single.IsNaN flt -> System.Single.IsNaN result
        |_ -> result = flt
    [<Property>]
    static member ``readInt16 from array of one int16`` (int : int16) =
        let bytes = System.BitConverter.GetBytes int
        let result = 
            match BinaryIO.run (MemoryBlockRead (bytes, BinaryIO.readInt16)) with
            |IOSuccess (int,_) -> int
            |IOError (_) -> failwith "error"
        result = int
    [<Property>]
    static member ``readInt32 from array of one int32`` (int : int32) =
        let bytes = System.BitConverter.GetBytes int
        let result = 
            match BinaryIO.run (MemoryBlockRead (bytes, BinaryIO.readInt32)) with
            |IOSuccess (int,_) -> int
            |IOError (_) -> failwith "error"
        result = int
    [<Property>]
    static member ``readInt64 from array of one int64`` (int : int64) =
        let bytes = System.BitConverter.GetBytes int
        let result = 
            match BinaryIO.run (MemoryBlockRead (bytes, BinaryIO.readInt64)) with
            |IOSuccess (int,_) -> int
            |IOError (_) -> failwith "error"
        result = int
    [<Property>]
    static member ``readString from array of one string`` (str : NonNull<string>) =
        let str = str.Get
        use memoryStream = new System.IO.MemoryStream()
        use binaryWriter = new System.IO.BinaryWriter(memoryStream);
        binaryWriter.Write(str)
        let bytes = memoryStream.ToArray()
        let result = 
            match BinaryIO.run (MemoryBlockRead (bytes, BinaryIO.readString)) with
            |IOSuccess (dec,_) -> str
            |IOError (_) -> failwith "error"
        result = str
    
        


