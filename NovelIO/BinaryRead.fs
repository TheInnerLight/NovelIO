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

/// A type representing a generic binary write format
type IBinaryWriteFormat = interface end
/// A type representing the state of binary IO
type IBinaryToken = 
    inherit IIO
    /// Destroys the token
    abstract member Destroy : unit -> unit
/// A type representing the state of binary reading IO
type IBinaryReaderState = inherit IBinaryToken
/// A type representing the state of binary writing IO
type IBinaryWriterState = inherit IBinaryToken

/// Base class for binary read formats
[<AbstractClass>]
type BinaryReadFormat<'a>() =
    /// Read this format structure from a supplied binary reader
    abstract member Read : System.IO.BinaryReader -> 'a
    /// Skip past this structure in the supplied binary reader
    member this.Skip br = this.Read br |> ignore
    /// IBinaryReadFormat implementation
    interface IBinaryReadFormat with
        member this.Skip br = this.Skip br

/// Base class for binary write formats
[<AbstractClass>]
type BinaryWriteFormat<'a>() =
    abstract member Write : System.IO.BinaryWriter -> 'a -> unit
    // IBinaryWriteFormat interface definition
    interface IBinaryWriteFormat

/// A type representing the state of text reading IO which can be mapped forward to a new state of type <'s> when we read from it
type IBinaryReaderState<'s when 's :> IBinaryReaderState> =
    inherit IBinaryReaderState
    /// Read from the state to get a value of type 'a and a new text reader state
    abstract member ReadUsing : BinaryReadFormat<'a> -> 'a * 's
/// A type representing the state of text writing IO which can be mapped forward to a new state of type <'s> when we write with it
type IBinaryWriterState<'s when 's :> IBinaryWriterState> =
    inherit IBinaryWriterState
    /// Write with the state, supplying a value and get a new text writer state
    abstract member WriteUsing : BinaryWriteFormat<'a> -> 'a -> unit * 's

// ***** IMPLEMENTATION *****

/// A binary read format of one type
type private BinaryReadFormatByFunc<'a> (readFunc, ?size : int) =
    inherit BinaryReadFormat<'a>()
    /// Read this format structure from a supplied binary reader
    override this.Read brf =
        readFunc brf

/// Class for binary writing based on a func
type private BinaryWriteFormatByFunc<'a> (writeFunc) =
    inherit BinaryWriteFormat<'a>()
    /// Write this format structure to a supplied binary writer
    override this.Write binRdr value =
        writeFunc binRdr value

/// Encapsulates the current state of binary  reading.
type BinaryReaderState(br : System.IO.BinaryReader) =
    let mutable valid = true
    /// Get the reader associated with this state.
    let getReader() =
        match valid with
        |true -> br
        |false -> raise <| System.InvalidOperationException "Attempted read from invalid state token"
    /// Disposes the stream associated with this binary file state and invalidates the token
    member internal this.Destroy() =
        valid <- false
        br.Dispose()
    /// Read from the current binary file state using the supplied binary read format
    member internal this.ReadUsing (readFormat : BinaryReadFormat<_>) =
        let result = readFormat.Read <| getReader()
        let newToken = BinaryReaderState(getReader())
        valid <- false
        result, newToken
    // IBinaryReaderState interface impl
    interface IBinaryReaderState<BinaryReaderState> with
        member this.ReadUsing readFormat = this.ReadUsing readFormat
        member this.Destroy() = this.Destroy()

/// Encapsulates the current state of binary file writing.
type BinaryWriterState internal(tr : System.IO.BinaryWriter) =
    let mutable valid = true
    /// Get the writer associated with this state.
    let getWriter() =
        match valid with
        |true -> tr
        |false -> raise <| System.InvalidOperationException "Attempted write with invalid state token"
    /// Disposes the stream associated with this text writer state and invalidates the token
    member internal this.Destroy() =
        valid <- false
        tr.Dispose()
    /// Write to the current binary state using the supplied binary write format and a value of type 'a
    member internal this.WriteUsing (writeFormat : BinaryWriteFormat<_>) (value : 'a) =
        writeFormat.Write (getWriter()) value
        let newToken = BinaryWriterState(getWriter())
        valid <- false
        (), newToken
    // IBinaryWriterState interface impl
    interface IBinaryWriterState<BinaryWriterState> with
        member this.WriteUsing writeFormat value = this.WriteUsing writeFormat value
        member this.Destroy() = this.Destroy()

/// Functions for performing binary IO operations
module BinaryIO =
    /// Create a binary read token for a supplied file name
    let private createFileReadToken fName =
        BinaryReaderState(new System.IO.BinaryReader(System.IO.File.OpenRead(fName)) )
    let private finaliseToken f =
        let destroyToken (token : IBinaryToken) = token.Destroy()
        match f with
        |IOSuccess (res, token) ->
            destroyToken token
            IOSuccess(res, ())
        |IOError e -> IOError e
    /// Create a binary read token for a supplied file name
    let private destroyToken (token : BinaryReaderState) =
        token.Destroy()
    /// Read from a supplied binary state using a supplied binary read format
    let run fName bfs =
        finaliseToken (bfs <| createFileReadToken fName)
    
    let private readBasic f (brt : IBinaryReaderState<_>) = 
        IO.performIoWithExceptionCheck (fun () -> brt.ReadUsing f)
    let private writeBasic f value (bwrt : IBinaryWriterState<_>) = 
        IO.performIoWithExceptionCheck (fun () -> bwrt.WriteUsing f value)
    /// A binary read format for reading bytes
    let readByte brt = readBasic (BinaryReadFormatByFunc(fun br -> br.ReadByte()) :> BinaryReadFormat<_>) brt
    /// A binary read format for reading chars
    let readChar brt = readBasic (BinaryReadFormatByFunc(fun br -> br.ReadChar()) :> BinaryReadFormat<_>) brt
    /// A binary read format for reading a decimal number
    let readDecimal brt = readBasic (BinaryReadFormatByFunc(fun br -> br.ReadDecimal()) :> BinaryReadFormat<_>) brt
    /// A binary read format for reading 16 bit ints
    let readInt16 brt = readBasic (BinaryReadFormatByFunc(fun br -> br.ReadInt16()) :> BinaryReadFormat<_>) brt
    /// A binary read format for reading (32 bit) ints
    let readInt32 brt = readBasic (BinaryReadFormatByFunc(fun br -> br.ReadInt32()) :> BinaryReadFormat<_>) brt
    /// A binary read format for reading 64 bit ints
    let readInt64 brt = readBasic (BinaryReadFormatByFunc(fun br -> br.ReadInt64()) :> BinaryReadFormat<_>) brt
    /// A binary read format for reading (double-precision) floats
    let readFloat brt = readBasic (BinaryReadFormatByFunc(fun br -> br.ReadDouble()) :> BinaryReadFormat<_>) brt
    /// A binary read format for reading single-precision floating point numbers
    let readFloat32 brt =  readBasic (BinaryReadFormatByFunc(fun br -> br.ReadDouble()) :> BinaryReadFormat<_>) brt
    /// A binary read format for reading a length prefixed string
    let readString brt = readBasic (BinaryReadFormatByFunc(fun br -> br.ReadString()) :> BinaryReadFormat<_>) brt


    /// A binary write format for writing bytes
    let writeByte value brt = writeBasic (BinaryWriteFormatByFunc(fun bwr (value : byte) -> bwr.Write(value)) :> BinaryWriteFormat<_>) value brt
    /// A binary write format for writing chars
    let writeChar value brt = writeBasic (BinaryWriteFormatByFunc(fun bwr (value : char) -> bwr.Write(value)) :> BinaryWriteFormat<_>) value brt
    /// A binary write format for writing a decimal number
    let writeDecimal value brt = writeBasic (BinaryWriteFormatByFunc(fun bwr (value : decimal) -> bwr.Write(value)) :> BinaryWriteFormat<_>) value brt
    /// A binary write format for writing 16 bit ints
    let writeInt16 value brt = writeBasic (BinaryWriteFormatByFunc(fun bwr (value : int16) -> bwr.Write(value)) :> BinaryWriteFormat<_>) value brt
    /// A binary write format for writing (32 bit) ints
    let writeInt32 value brt = writeBasic (BinaryWriteFormatByFunc(fun bwr (value : int32) -> bwr.Write(value)) :> BinaryWriteFormat<_>) value brt
    /// A binary write format for writing 64 bit ints
    let writeInt64 value brt = writeBasic (BinaryWriteFormatByFunc(fun bwr (value : int64) -> bwr.Write(value)) :> BinaryWriteFormat<_>) value brt
    /// A binary write format for writing (double-precision) floats
    let writeFloat value brt = writeBasic (BinaryWriteFormatByFunc(fun bwr (value : float) -> bwr.Write(value)) :> BinaryWriteFormat<_>) value brt
    /// A binary write format for writing single-precision floating point numbers
    let writeFloat32 value brt =  writeBasic (BinaryWriteFormatByFunc(fun bwr (value : float32) -> bwr.Write(value)) :> BinaryWriteFormat<_>) value brt
    /// A binary write format for writing a length prefixed string
    let writeString value brt = writeBasic (BinaryWriteFormatByFunc(fun bwr (value : string) -> bwr.Write(value)) :> BinaryWriteFormat<_>) value brt
