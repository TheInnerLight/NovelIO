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
type IOResult<'a, 'b> =
    /// A successful IO operation
    |IOSuccess of 'a * 'b
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
    /// IO failure due to trying to read past the end of the stream
    |PastEndOfStream of EndOfStreamException
    /// IO failure due to a file not being found
    |FileNotFound of FileNotFoundException
    /// IO failure due to a path being too long
    |PathTooLong of PathTooLongException
    /// IO failure due a stream being closed
    |StreamClosed of System.ObjectDisposedException
    /// IO failure due to unathourised access to a resource
    |UnauthourisedAccess of System.UnauthorizedAccessException

/// A token used to represent the state of an IO operation
type IIO = interface end
    
type ReadLength =
    |Specified of int
    |Remainder
