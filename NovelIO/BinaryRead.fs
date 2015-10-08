namespace NovelFS.NovelIO

type IBinaryRead =
    abstract member Skip : System.IO.BinaryReader -> unit

[<AbstractClass>]
type BinaryRead<'a>() =
    abstract member Read : System.IO.BinaryReader -> 'a
    abstract member MinByteCount : int
    
    interface IBinaryRead with
        member this.Skip br = this.Read br |> ignore

    static member Tuplify2(a : BinaryRead<'a>, b : BinaryRead<'b>) =
        BinaryReadDouble(a, b) :> BinaryRead<'a*'b>

    static member Tuplify3(a : BinaryRead<'a>, b : BinaryRead<'b>, c : BinaryRead<'c>) =
        BinaryReadTriple(a, b, c) :> BinaryRead<'a*'b*'c>

and BinaryReadSingle<'a> (readFunc, ?size : int) =
    inherit BinaryRead<'a>()
    let size = 
        match size with
        |Some sz -> sz
        |None -> sizeof<'a>

    override this.Read lst =
        readFunc lst

    override this.MinByteCount = size

and BinaryReadDouble<'a, 'b> (one : BinaryRead<'a>, two : BinaryRead<'b>) =
    inherit BinaryRead<'a*'b>()
    let size = one.MinByteCount + two.MinByteCount

    override this.Read br =
        let result1 = one.Read br
        let result2 = two.Read br
        (result1, result2)

    override this.MinByteCount = size

and BinaryReadTriple<'a,'b, 'c> (one : BinaryRead<'a>, two : BinaryRead<'b>, three : BinaryRead<'c>) =
    inherit BinaryRead<'a*'b*'c>()
    let size = one.MinByteCount + two.MinByteCount + three.MinByteCount

    override this.Read br =
        let result1 = one.Read br
        let result2 = two.Read br
        let result3 = three.Read br
        (result1, result2, result3)

    override this.MinByteCount = size
        



//and BinaryReadList<'a> (one : BinaryRead<'a>, length : int) =
//    inherit BinaryRead<'a>()
//
//        
//    override this.ByteCount = one.ByteCount * length

type BinaryFileState(fname : string, br : System.IO.BinaryReader, readCalls : IBinaryRead list) =
    let mutable reader = new System.IO.BinaryReader(System.IO.File.OpenRead(fname))
    let mutable valid = false
    member internal this.BinaryReader = 
        match valid with
        |true -> reader
        |false ->
            reader <- new System.IO.BinaryReader(System.IO.File.OpenRead(fname))
            readCalls |> List.iter (fun ibr -> ibr.Skip reader)
            reader
    member internal this.Filename = fname
    member internal this.ReadList = readCalls
    member internal this.Invalidate() = valid <- false
    interface IIOToken

module IO =
    let inline tuple2 (a:^a) (b:^b) =
        (^a : (static member Tuplify2: ^a * ^b -> ^c) (a, b))

    let inline tuple3 (a:^a) (b:^b) (c:^c)  =
        (^a : (static member Tuplify3: ^a * ^b * ^c -> ^d) (a, b, c))

module BinaryReading =
    let readByte =
        let readByteFunc (br : System.IO.BinaryReader) = br.ReadByte()
        BinaryReadSingle<byte>(readByteFunc) :> BinaryRead<_>
    let readChar =
        let readCharFunc (br : System.IO.BinaryReader) = br.ReadChar()
        BinaryReadSingle<char>(readCharFunc, 1) :> BinaryRead<_>
    let readInt =
        let readIntFunc (br : System.IO.BinaryReader) = br.ReadInt32()
        BinaryReadSingle<int>(readIntFunc) :> BinaryRead<_>
    let readFloat =
        let readFloatFunc (br : System.IO.BinaryReader) = br.ReadDouble()
        BinaryReadSingle<float>(readFloatFunc) :> BinaryRead<_>

    let createToken fName =
        BinaryFileState(fName, new System.IO.BinaryReader(System.IO.File.OpenRead(fName)), [])

    let read (br : BinaryRead<_>) (brt : BinaryFileState) =
        let result = br.Read brt.BinaryReader
        let newToken = BinaryFileState(brt.Filename, brt.BinaryReader, (br :> IBinaryRead) :: brt.ReadList)
        brt.Invalidate()
        IOSuccess(result, newToken)
        


    