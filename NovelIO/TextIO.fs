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

/// A type representing a generic text write format
type ITextWriteFormat = interface end
/// A type representing the state of text IO
type ITextToken = 
    inherit IIO
    /// Destroys the token
    abstract member Destroy : unit -> unit
/// A type representing the state of text reading IO
type ITextReaderState = inherit ITextToken
/// A type representing the state of text writing IO
type ITextWriterState = inherit ITextToken

/// Base class for text read formats
[<AbstractClass>]
type TextReadFormat<'a>() =
    abstract member Read : System.IO.TextReader -> 'a
    /// Skip past this structure in the supplied text reader
    member this.Skip br = this.Read br |> ignore
    // ITextReadFormat interface definition
    interface ITextReadFormat with
        member this.Skip txtRdr = this.Skip txtRdr

/// Base class for text write formats
[<AbstractClass>]
type TextWriteFormat<'a>() =
    abstract member Write : System.IO.TextWriter -> 'a -> unit
    // ITextWriteFormat interface definition
    interface ITextWriteFormat

/// A type representing the state of text reading IO which can be mapped forward to a new state of type <'s> when we read from it
type ITextReaderState<'s when 's :> ITextReaderState> =
    inherit ITextReaderState
    /// Read from the state to get a value of type 'a and a new text reader state
    abstract member ReadUsing : TextReadFormat<'a> -> 'a * 's
/// A type representing the state of text writing IO which can be mapped forward to a new state of type <'s> when we write with it
type ITextWriterState<'s when 's :> ITextWriterState> =
    inherit ITextWriterState
    /// Write with the state, supplying a value and get a new text writer state
    abstract member WriteUsing : TextWriteFormat<'a> -> 'a -> unit * 's

// ***** IMPLEMENTATION *****

/// Class for reading a from a file based on a func
type private TextReadFormatByFunc (readFunc) =
    inherit TextReadFormat<string>()
    /// Read this format structure from a supplied text reader
    override this.Read txtRdr =
        readFunc txtRdr

/// Class for text writing based on a func
type private TextWriteFormatByFunc (writeFunc) =
    inherit TextWriteFormat<string>()
    /// Write this format structure to a supplied text writer
    override this.Write txtRdr value =
        writeFunc txtRdr value

/// Encapsulates the current state of text file reading.
type TextReaderState internal(tr : System.IO.TextReader) =
    let mutable valid = true
    /// Get the reader associated with this state.
    let getReader() =
        match valid with
        |true -> tr
        |false -> raise <| System.InvalidOperationException "Attempted read from invalid state token"
    /// Disposes the stream associated with this text read state and invalidates the token
    member internal this.Destroy() =
        valid <- false
        tr.Dispose()
    /// Read from the current text state using the supplied text read format
    member internal this.ReadUsing (readFormat : TextReadFormat<_>) =
        let result = readFormat.Read <| getReader()
        let newToken = TextReaderState(getReader())
        valid <- false
        result, newToken
    // ITextReaderState interface impl
    interface ITextReaderState<TextReaderState> with
        member this.ReadUsing readFormat = this.ReadUsing readFormat
        member this.Destroy() = this.Destroy()

/// Encapsulates the current state of text file writing.
type TextWriterState internal(tr : System.IO.TextWriter) =
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
    /// Write to the current text state using the supplied text write format and a value of type 'a
    member internal this.WriteUsing (writeFormat : TextWriteFormat<_>) (value : 'a) =
        writeFormat.Write (getWriter()) value
        let newToken = TextWriterState(getWriter())
        valid <- false
        (), newToken
    // ITextWriterState interface impl
    interface ITextWriterState<TextWriterState> with
        member this.WriteUsing writeFormat value = this.WriteUsing writeFormat value
        member this.Destroy() = this.Destroy()

/// Functions for performing text IO operations
module TextIO =
    open IOExpressionFunctions
    /// Create a text read token for a supplied file name
    let private createFileReadToken fName =
        TextReaderState(new System.IO.StreamReader(System.IO.File.OpenRead(fName)) )
    /// Create a text write token for a supplied file name
    let private createFileWriteToken fName =
        TextWriterState(new System.IO.StreamWriter(System.IO.File.OpenWrite(fName)) )
    /// Create a text read token for a supplied socket
    let private createTCPServerReadToken socket =
        TextReaderState (new System.IO.StreamReader(new System.Net.Sockets.NetworkStream(socket)))

    let private finaliseToken f =
        let destroyToken (token : ITextToken) = token.Destroy()
        match f with
        |IOSuccess (res, token) ->
            destroyToken token
            IOSuccess(res, ())
        |IOError e -> IOError e
            
    /// Read from a supplied binary state using a supplied binary read format
    let run rIOType =
        match rIOType with
        |FileReadIO (fName, f) ->
            finaliseToken (f <| createFileReadToken fName)
        |FileWriteIO (fName, f) ->
            finaliseToken (f <| createFileWriteToken fName)
        |TCPServerSocketReadIO (socket, f) ->
            finaliseToken (f <| createTCPServerReadToken socket)
            

    let private readBasic f (brt : ITextReaderState<_>) = 
        IO.performIoWithExceptionCheck (fun () -> brt.ReadUsing f)

    let private writeBasic f value (twrt : ITextWriterState<_>) = 
        IO.performIoWithExceptionCheck (fun () -> twrt.WriteUsing f value)

    /// Read a line from a supplied text state
    let readLine (trt : ITextReaderState<_>) =
        readBasic (TextReadFormatByFunc(fun tr -> tr.ReadLine()) :> TextReadFormat<_>) trt
    /// Read all of the remaining text from the supplied rext state
    let readToEnd (trt : ITextReaderState<_>) =
        readBasic (TextReadFormatByFunc(fun tr -> tr.ReadToEnd()) :> TextReadFormat<_>) trt

    /// Write a line to a supplied text writer state
    let writeLine value (twrt : ITextWriterState<_>) =
        writeBasic (TextWriteFormatByFunc(fun tw str -> tw.WriteLine(str)) :> TextWriteFormat<_>) value twrt