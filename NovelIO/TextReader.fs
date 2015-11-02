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

/// A type representing a generic text read format
type ITextReadFormat =
    /// Skip past this structure in the supplied text reader
    abstract member Skip : System.IO.TextReader -> unit

/// Base class for text read formats
[<AbstractClass>]
type TextReadFormat<'a>() =
    abstract member Read : System.IO.TextReader -> 'a
    abstract member Skip : System.IO.TextReader -> unit
    // ITextRead interface definition
    interface ITextReadFormat with
        member this.Skip txtRdr = this.Skip txtRdr

/// Class for reading a single line from a file
type TextReadFormatSingle() =
    inherit TextReadFormat<string>()
    /// Read this format structure from a supplied text reader
    override this.Read (txtRdr : System.IO.TextReader) =
        txtRdr.ReadLine()
    /// Skip past this structure in the supplied text reader
    override this.Skip txtRdr =
        this.Read txtRdr |> ignore

/// Class for reading a pair of combined textreadformats
type TextReadFormatDouble<'a, 'b>(one : TextReadFormat<'a>, two : TextReadFormat<'b>) =
    inherit TextReadFormat<'a*'b>()
    /// Read this format structure from a supplied text reader
    override this.Read (txtRdr : System.IO.TextReader) =
        (one.Read txtRdr, two.Read txtRdr)
    /// Skip past this structure in the supplied text reader
    override this.Skip txtRdr =
        this.Read txtRdr |> ignore

/// Class for reading a triple of combined textreadformats
type TextReadFormatTriple<'a, 'b, 'c>(one : TextReadFormat<'a>, two : TextReadFormat<'b>, three : TextReadFormat<'c>) =
    inherit TextReadFormat<'a*'b*'c>()
    /// Read this format structure from a supplied text reader
    override this.Read (txtRdr : System.IO.TextReader) =
        (one.Read txtRdr, two.Read txtRdr, three.Read txtRdr)
    /// Skip past this structure in the supplied text reader
    override this.Skip txtRdr =
        this.Read txtRdr |> ignore

/// Encapsulates the current state of text file reading.  Reading from the same token will, except in exceptional circumstances, produce the same result.
type TextFileState(fname : string, br : System.IO.TextReader, readCalls : ITextReadFormat list) =
    let mutable reader = br
    let mutable valid = true
    /// Get the reader associated with this state.  If the state is valid, using the existing one, otherwise make a new one nand move to the correct position
    let getReader() =
        match valid with
        |true -> br
        |false ->
            reader <- new System.IO.StreamReader(System.IO.File.OpenRead(fname))
            readCalls |> List.iter (fun ibr -> ibr.Skip reader)
            reader
    /// Disposes the stream associated with this text file state and invalidates the token
    member internal this.Dispose() =
        valid <- false
        reader.Dispose()
    /// Read from the current text file state using the supplied text read format
    member internal this.ReadUsing (readFormat : TextReadFormat<_>) =
        let result = readFormat.Read <| getReader()
        let newToken = TextFileState(fname, getReader(), (readFormat :> ITextReadFormat) :: readCalls)
        valid <- false
        result, newToken
    interface IIOToken

/// Functions for performing text IO operations
module TextIO =
    /// Create a binary read token for a supplied file name
    let createToken fName =
        TextFileState(fName, new System.IO.StreamReader(System.IO.File.OpenRead(fName)), [])
    /// Create a binary read token for a supplied file name
    let destroyToken (token : TextFileState) =
        token.Dispose()
    /// Read from a supplied binary state using a supplied binary read format
    let read (tr : TextReadFormat<_>) (trt : TextFileState) =
        IO.performIoWithExceptionCheck (fun () -> trt.ReadUsing tr)