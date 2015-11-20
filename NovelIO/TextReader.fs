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

/// Encapsulates the current state of text file reading.  Reading from the same token will, except in exceptional circumstances, produce the same result.
type TextFileState(fname : string, tr : System.IO.TextReader) =
    let mutable valid = true
    /// Get the reader associated with this state.  If the state is valid, using the existing one, otherwise make a new one nand move to the correct position
    let getReader() =
        match valid with
        |true -> tr
        |false -> raise <| System.InvalidOperationException "Attempted read from invalid state token"
    /// Disposes the stream associated with this text file state and invalidates the token
    member internal this.Dispose() =
        valid <- false
        tr.Dispose()
    /// Read from the current text file state using the supplied text read format
    member internal this.ReadUsing (readFormat : TextReadFormat<_>) =
        let result = readFormat.Read <| getReader()
        let newToken = TextFileState(fname, getReader())
        valid <- false
        result, newToken
    interface IIO

/// Functions for performing text IO operations
module TextIO =
    open IOExpressionFunctions
    /// Create a binary read token for a supplied file name
    let private createToken fName =
        TextFileState( fName, new System.IO.StreamReader(System.IO.File.OpenRead(fName)) )
    /// Create a binary read token for a supplied file name
    let private destroyToken (token : TextFileState) =
        token.Dispose()
    /// Read from a supplied binary state using a supplied binary read format
    let run fName tfs =
        match tfs <| createToken fName with
        |IOSuccess (res, token) ->
            destroyToken token
            IOSuccess(res, ())
        |IOError e -> IOError e
    /// Read from a supplied binary state using a supplied binary read format
    let read (tr : TextReadFormat<_>) (trt : TextFileState) =
        IO.performIoWithExceptionCheck (fun () -> trt.ReadUsing tr)