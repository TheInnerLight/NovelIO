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

/// Functions relating to FileIO
module internal FileIO =
    /// Perform some file IO and check for exceptions
    let performFileIoWithExceptionCheck f =
        try
            f() |> IOSuccess
        with
        | :? System.IO.DirectoryNotFoundException as dnfe -> DirectoryNotFound dnfe |> IOError
        | :? System.IO.DriveNotFoundException as dnfe -> DriveNotFound dnfe |> IOError
        | :? System.IO.EndOfStreamException as eose -> PastEndOfStream eose |> IOError
        | :? System.IO.FileNotFoundException as fnfe -> FileNotFound fnfe |> IOError
        | :? System.IO.PathTooLongException as ptle -> PathTooLong ptle |> IOError
        | :? System.ObjectDisposedException as ode -> StreamClosed ode |> IOError
        | :? System.UnauthorizedAccessException as unax -> UnauthourisedAccess unax |> IOError
        | :? System.IO.IOException as ioex -> Other ioex |> IOError

