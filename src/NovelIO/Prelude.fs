(*
   Copyright 2015-2016 Philip Curzon

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

/// Exception that occurs when attempting to write to a Channel that does not support writing
exception ChannelDoesNotSupportWritingException

/// Exception that occurs when attempt to read from a Channel that does not support reading
exception ChannelDoesNotSupportReadingException

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
    /// IO failure due to the supplied channel not supporting reading
    |ChannelDoesNotSupportReading
    /// IO failure due to the supplied channel not supporting writing
    |ChannelDoesNotSupportWriting
    /// IO failure due to trying to read past the end of the stream
    |PastEndOfStream of EndOfStreamException
    /// Incorrect format
    |IncorrectFormat
    /// IO failure due to an action being attempted on a stream which does not support it
    |StreamStateUnsupported of string

/// Units of bytes
[<Measure>] type Bytes

/// Represents a filename in a valid format
type FilePath private (fName : string) =
    let fInfo = System.IO.FileInfo(fName)
    /// The raw string representation of the filename
    member this.PathString = fName

    /// Attempts to create a valid filename from a string, returning a Filename if successful or throwing an exception otherwise
    static member CreateFromString path =
        Some <| FilePath path

    /// Attempts to create a valid filename from a string, returning Some Filename if successful or None otherwise
    static member TryCreateFromString (path : string) =
        try
            FilePath.CreateFromString path
        with
            | :? System.Security.SecurityException -> None
            | :? System.ArgumentException -> None
            | :? System.UnauthorizedAccessException -> None
            | :? System.IO.PathTooLongException -> None
            | :? System.NotSupportedException -> None

/// Provides patterns for matching against valid and invalid file names
[<AutoOpen>]
module PathDiscriminators =
    let (|ValidFilePath|InvalidFilePath|) (path : string) =
        match FilePath.TryCreateFromString path with
        |Some fname -> ValidFilePath fname
        |None -> InvalidFilePath

/// General functions of wide applicability
[<AutoOpen>]
module General =
    /// Helper function that takes two arguments and throws away the second, returning the first
    let inline const' x _ = x
    /// When supplied with a function f, returns a new function that accepts the first and second arguments in the opposite order
    let inline flip f a b = f b a
    /// Curried function for prepending to list, equivalent to x :: ys
    let inline listCons x ys = x :: ys
    /// Converts an uncurried function to a curried function.
    let inline curry f a b = f(a, b)
    /// Converts a curried function to a function on pairs.
    let inline uncurry f (a, b) = f a b 
        
/// Specifies how the operating system should open a file
[<RequireQualifiedAccess>]
type FileMode =
    /// Specifies that a new file should be created.  If the file already exists, an IOException will be thrown.
    |CreateNew
    /// Specifies that a new file should be created.  If the file already exists, it will be overwritten.
    |Create
    /// Specifies that an existing file should be opened.
    |Open
    /// Specifies that an existing file should be opened.  If the file does not exist, it will be created.
    |OpenOrCreate
    /// Specifies that an existing file should be opened but that, once opened, it should be truncated to zero bytes.
    |Truncate
    /// Specifies that an existing file should be opened and the end of the file sought.  If the file does not exist, it will be created.
    |Append

/// Defines the type of access to a file
[<RequireQualifiedAccess>]
type FileAccess =
    /// Read access to a file
    |Read
    /// Write access to a file
    |Write
    /// Read and write access to a file
    |ReadWrite

type ChannelIOMode =
    |Synchronous
    |Optimise
    |Asynchronous

/// A channel that may support text being read from it and written to it
type TChannel = 
    private {
        TextReader : StreamReader option; 
        TextWriter : StreamWriter option;
        IOMode : ChannelIOMode
        }

/// A channel that may support binary data being read from it or written to it
type BChannel = 
    private {
        IOStream : System.IO.Stream; 
        IOMode : ChannelIOMode;
        mutable EOS :  bool
        }

/// A TCP Server
type TCPServer = private {TCPListener : Sockets.TcpListener}

/// A connected TCP Socket
type TCPConnectedSocket = private {TCPConnectedSocket : System.Net.Sockets.Socket}

/// Defines possible endianness options
type Endianness =
    /// Big Endian
    |BigEndian
    /// Little Endian
    |LittleEndian

/// Functions for handling byte order
module ByteOrder =
    /// Gets the endianness of the current platform
    let systemEndianness =
        match System.BitConverter.IsLittleEndian with
        |true -> LittleEndian
        |false -> BigEndian

    /// Returns true if the supplied endianness is big endian
    let isBigEndian endianness =
        match endianness with
        |BigEndian -> true
        |_ -> false


