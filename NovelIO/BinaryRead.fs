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

namespace NovelFS.NovelIO

/// A type representing a generic binary read format
type IBinaryReadFormat =
    /// Skip past this structure in the supplied binary reader
    abstract member Skip : System.IO.BinaryReader -> unit

/// Base class for binary read formats
[<AbstractClass>]
type BinaryReadFormat<'a>() =
    /// Read this format structure from a supplied binary reader
    abstract member Read : System.IO.BinaryReader -> 'a
    /// The minimum number of bytes that will be read when this format is read from a stream
    abstract member MinByteCount : int
    /// Skip past this structure in the supplied binary reader
    member this.Skip br = this.Read br |> ignore
    /// IBinaryReadFormat implementation
    interface IBinaryReadFormat with
        member this.Skip br = this.Skip br
    /// Transform two binary read formats of types <'a> and <'b> into a combined reader of type<'a*'b>
    static member Tuplify2(a : BinaryReadFormat<'a>, b : BinaryReadFormat<'b>) =
        BinaryReadFormatDouble(a, b) :> BinaryReadFormat<'a*'b>
    /// Transform three binary read formats of types <'a>, <'b> and <'c> into a combined reader of type<'a*'b'c>
    static member Tuplify3(a : BinaryReadFormat<'a>, b : BinaryReadFormat<'b>, c : BinaryReadFormat<'c>) =
        BinaryReadFormatTriple(a, b, c) :> BinaryReadFormat<'a*'b*'c>
    /// Transform a binary read format of type <'a> into a binary read format of type <'a list> with supplied length
    static member Listify(a : BinaryReadFormat<'a>, specLen) =
        BinaryReadFormatList(a, specLen) :> BinaryReadFormat<'a list>
    /// Transform a binary read format of type <'a> into a binary read format of type <'a[]> with supplied length
    static member Arrayify(a : BinaryReadFormat<'a>, specLen) =
        BinaryReadFormatArray(a, specLen) :> BinaryReadFormat<'a[]>

/// A binary read format of one type
and private BinaryReadFormatSingle<'a> (readFunc, ?size : int) =
    inherit BinaryReadFormat<'a>()
    let size = 
        match size with
        |Some sz -> sz
        |None -> sizeof<'a>
    /// Read this format structure from a supplied binary reader
    override this.Read lst =
        readFunc lst
    /// The minimum number of bytes that will be read when this format is read from a stream
    override this.MinByteCount = size

/// A binary read format of two combined types
and private BinaryReadFormatDouble<'a, 'b> (one : BinaryReadFormat<'a>, two : BinaryReadFormat<'b>) =
    inherit BinaryReadFormat<'a*'b>()
    let size = one.MinByteCount + two.MinByteCount
    /// Read this format structure from a supplied binary reader
    override this.Read br =
        let result1 = one.Read br
        let result2 = two.Read br
        (result1, result2)
    /// The minimum number of bytes that will be read when this format is read from a stream
    override this.MinByteCount = size

/// A binary read format of three combined types
and private BinaryReadFormatTriple<'a,'b, 'c> (one : BinaryReadFormat<'a>, two : BinaryReadFormat<'b>, three : BinaryReadFormat<'c>) =
    inherit BinaryReadFormat<'a*'b*'c>()
    let size = one.MinByteCount + two.MinByteCount + three.MinByteCount
    /// Read this format structure from a supplied binary reader
    override this.Read br =
        let result1 = one.Read br
        let result2 = two.Read br
        let result3 = three.Read br
        (result1, result2, result3)
    /// The minimum number of bytes that will be read when this format is read from a stream
    override this.MinByteCount = size

/// A binary read format of a list of types  
and private BinaryReadFormatList<'a> (one : BinaryReadFormat<'a>, rLen : ReadLength) =
    inherit BinaryReadFormat<'a list>()
    /// Read this format structure from a supplied binary reader
    override this.Read br =
        match rLen with
        |Specified len -> List.init len (fun i -> one.Read br)
        |Remainder -> Seq.initInfinite id |> Seq.takeWhile (fun i -> br.PeekChar() <> -1) |> Seq.map (fun i -> one.Read br) |> List.ofSeq
        
    /// The minimum number of bytes that will be read when this format is read from a stream
    override this.MinByteCount =
        match rLen with
        |Specified len -> one.MinByteCount*len
        |Remainder -> one.MinByteCount

/// A binary read format of an array of types
and private BinaryReadFormatArray<'a> (one : BinaryReadFormat<'a>, rLen : ReadLength) =
    inherit BinaryReadFormat<'a[]>()
    /// Read this format structure from a supplied binary reader
    override this.Read br =
        match rLen with
        |Specified len -> Array.init len (fun i -> one.Read br)
        |Remainder -> Seq.initInfinite id |> Seq.takeWhile (fun i -> br.PeekChar() <> -1) |> Seq.map (fun i -> one.Read br) |> Array.ofSeq
    /// The minimum number of bytes that will be read when this format is read from a stream
    override this.MinByteCount =
        match rLen with
        |Specified len -> one.MinByteCount*len
        |Remainder -> one.MinByteCount

/// Encapsulates the current state of binary file reading.  Reading from the same token will, except in exceptional circumstances, produce the same result.
type BinaryFileState(fname : string, br : System.IO.BinaryReader, readCalls : IBinaryReadFormat list) =
    let mutable reader = br
    let mutable valid = true
    /// Get the reader associated with this state.  If the state is valid, using the existing one, otherwise make a new one nand move to the correct position
    let getReader() =
        match valid with
        |true -> br
        |false ->
            reader <- new System.IO.BinaryReader(System.IO.File.OpenRead(fname))
            readCalls |> List.iter (fun ibr -> ibr.Skip reader)
            reader
    /// Disposes the stream associated with this binary file state and invalidates the token
    member internal this.Dispose() =
        valid <- false
        reader.Dispose()
    /// Read from the current binary file state using the supplied binary read format
    member internal this.ReadUsing (readFormat : BinaryReadFormat<_>) =
        let result = readFormat.Read <| getReader()
        let newToken = BinaryFileState(fname, getReader(), (readFormat :> IBinaryReadFormat) :: readCalls)
        valid <- false
        result, newToken
    interface IIOToken

/// Functions for creating binary reading formats
module BinaryReadFormatter =
    /// A binary read format for reading bytes
    let readByte = BinaryReadFormatSingle(fun br -> br.ReadByte()) :> BinaryReadFormat<_>
    /// A binary read format for reading chars
    let readChar = BinaryReadFormatSingle(fun br -> br.ReadChar()) :> BinaryReadFormat<_>
    /// A binary read format for reading a decimal number
    let readDecimal = BinaryReadFormatSingle(fun br -> br.ReadDecimal()) :> BinaryReadFormat<_>
    /// A binary read format for reading 16 bit ints
    let readInt16 = BinaryReadFormatSingle(fun br -> br.ReadInt16()) :> BinaryReadFormat<_>
    /// A binary read format for reading (32 bit) ints
    let readInt32 = BinaryReadFormatSingle(fun br -> br.ReadInt32()) :> BinaryReadFormat<_>
    /// A binary read format for reading 64 bit ints
    let readInt64 = BinaryReadFormatSingle(fun br -> br.ReadInt64()) :> BinaryReadFormat<_>
    /// A binary read format for reading (double-precision) floats
    let readFloat = BinaryReadFormatSingle(fun br -> br.ReadDouble()) :> BinaryReadFormat<_>
    /// A binary read format for reading single-precision floating point numbers
    let readFloat32 = BinaryReadFormatSingle(fun br -> br.ReadDouble()) :> BinaryReadFormat<_>
    /// A binary read format for reading a length prefixed string
    let readString = BinaryReadFormatSingle(fun br -> br.ReadString()) :> BinaryReadFormat<_>

/// Functions for performing binary IO operations
module BinaryIO =
    /// Create a binary read token for a supplied file name
    let createToken fName =
        BinaryFileState(fName, new System.IO.BinaryReader(System.IO.File.OpenRead(fName)), [])
    /// Create a binary read token for a supplied file name
    let destroyToken (token : BinaryFileState) =
        token.Dispose()
    /// Read from a supplied binary state using a supplied binary read format
    let read (br : BinaryReadFormat<_>) (brt : BinaryFileState) =
        FileIO.performFileIoWithExceptionCheck (fun () -> brt.ReadUsing br)