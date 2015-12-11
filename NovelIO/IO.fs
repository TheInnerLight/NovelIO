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

/// Represents the result of an IO operation
type IO<'a> =
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
    /// IO failure due to trying to read past the end of the stream
    |PastEndOfStream of EndOfStreamException
    /// Incorrect format
    |IncorrectFormat

module IO =
    let return' a = IOSuccess a
    let bind x f =
        match x with
        |IOSuccess a -> f a
        |IOError err -> IOError err

    let withExceptionCheck f =
        try 
            f() |> IOSuccess
        with
            | :? EndOfStreamException as eose -> PastEndOfStream eose |> IOError
            | :? System.ObjectDisposedException as ode -> StreamClosed ode |> IOError
            | :? FileNotFoundException as fnfe -> FileNotFound fnfe |> IOError
            | :? PathTooLongException as ptle -> PathTooLong ptle |> IOError
            | :? System.UnauthorizedAccessException as uaex -> UnauthourisedAccess uaex |> IOError
            | :? IOException as ioex -> Other ioex |> IOError

type IOStream = interface end

type IOBuilder() =
    member this.Return a = IO.return' a
    member this.Bind (x, f) = IO.bind x f

[<AutoOpen>]
module IOBuilders =
    let io = IOBuilder()