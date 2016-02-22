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

open System.IO
open System.Net

exception HandleDoesNotSupportWritingException
exception HandleDoesNotSupportReadingException

/// Represents the result of an IO operation
type IOResult<'a> =
    /// A successful IO operation
    |IOSuccess of 'a
    /// An IO operation which failed
    |IOError of IOErrorResult
/// A set of possible failure modes for IO Operations
and IOErrorResult =
    /// A generic IO failure
    |Other of IOException
    /// IO failure due to a directory not being found
    |DirectoryNotFound of DirectoryNotFoundException
    /// IO failure due to a drive not being found
    |DriveNotFound of DriveNotFoundException
    /// IO failure due to a file not being found
    |FileNotFound of FileNotFoundException
    /// IO failure due to a path being too long
    |PathTooLong of PathTooLongException
    /// IO failure due to unathourised access to a resource
    |UnauthourisedAccess of System.UnauthorizedAccessException
    /// IO failure due a stream being closed
    |StreamClosed of System.ObjectDisposedException
    /// IO failure due to the supplied handle not supporting reading
    |HandleDoesNotSupportReading
    /// IO failure due to the supplied handle not supporting writing
    |HandleDoesNotSupportWriting
    /// IO failure due to trying to read past the end of the stream
    |PastEndOfStream of EndOfStreamException
    /// Incorrect format
    |IncorrectFormat
    /// IO failure due to an action being attempted on a stream which does not support it
    |StreamStateUnsupported of string

/// Represents a filename of a valid format
type Filename =
    private |Filename of string

    /// The raw string representation of the filename
    member this.PathString = match this with Filename str -> str

    /// Attempts to create a valid filename from a string, returning Some Filename if successful or None otherwise
    static member TryCreateFromString (path : string) =
        match path.IndexOfAny(Path.GetInvalidFileNameChars()) = -1 with
        |true -> Some <| Filename(path)
        |false -> None

    /// Attempts to create a valid filename from a string, returning a Filename if successful or throwing an exception otherwise
    static member CreateFromString path =
        match Filename.TryCreateFromString path with
        |Some fname -> fname
        |None -> invalidArg "path" "Path Invalid"

[<AutoOpen>]
module PathDiscriminators =
    let (|ValidFilename|InvalidFilename|) (path : string) =
        match Filename.TryCreateFromString path with
        |Some fname -> ValidFilename fname
        |None -> InvalidFilename

/// General functions of wide applicability
[<AutoOpen>]
module General =
    let const' x _ = x
    let flip f a b = f b a

type Handle = private {TextReader : TextReader option; TextWriter : TextWriter option}
/// A TCP Server
type TCPServer = private {TCPListener : Sockets.TcpListener}
/// A connected TCP Socket
type TCPConnectedSocket = private {TCPConnectedSocket : System.Net.Sockets.Socket}

module internal IOResult =
    let withExceptionCheck f a =
        try 
            f a |> IOSuccess
        with
            | HandleDoesNotSupportReadingException -> HandleDoesNotSupportReading |> IOError
            | HandleDoesNotSupportWritingException -> HandleDoesNotSupportWriting |> IOError
            | :? EndOfStreamException as eose -> PastEndOfStream eose |> IOError
            | :? System.ObjectDisposedException as ode -> StreamClosed ode |> IOError
            | :? FileNotFoundException as fnfe -> FileNotFound fnfe |> IOError
            | :? PathTooLongException as ptle -> PathTooLong ptle |> IOError
            | :? System.UnauthorizedAccessException as uaex -> UnauthourisedAccess uaex |> IOError
            | :? IOException as ioex -> Other ioex |> IOError



