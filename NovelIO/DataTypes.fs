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

type IOResult<'a, 'b> =
    |IOSuccess of 'a * 'b
    |IOError of IOErrorResult
and IOErrorResult =
    |Other of IOException
    |DirectoryNotFound of DirectoryNotFoundException
    |DriveNotFound of DriveNotFoundException
    |PastEndOfStream of EndOfStreamException
    |FileNotFound of FileNotFoundException
    |PathTooLong of PathTooLongException
    |StreamClosed of System.ObjectDisposedException
    |UnauthourisedAccess of System.UnauthorizedAccessException

type IIOToken = interface end

exception InvalidIOTokenError

type internal IOStatusValidity =
    |Valid
    |Invalid
    
