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

module internal InternalIOHelper =
    /// Helper function to catch IO exceptions and structure the success/failure
    let withExceptionCheck f a =
        try 
            f a |> IOSuccess
        with
            | ChannelDoesNotSupportReadingException -> ChannelDoesNotSupportReading |> IOError
            | ChannelDoesNotSupportWritingException -> ChannelDoesNotSupportWriting |> IOError
            | :? EndOfStreamException as eose -> PastEndOfStream eose |> IOError
            | :? System.ObjectDisposedException as ode -> StreamClosed ode |> IOError
            | :? FileNotFoundException as fnfe -> FileNotFound fnfe |> IOError
            | :? PathTooLongException as ptle -> PathTooLong ptle |> IOError
            | :? System.UnauthorizedAccessException as uaex -> UnauthourisedAccess uaex |> IOError
            | :? IOException as ioex -> Other ioex |> IOError

    /// Converts a NovelIO file mode to a System.IO.FileMode
    let fileModeToSystemIOFileMode (fm : NovelFS.NovelIO.FileMode) =
        match fm with
        |NovelFS.NovelIO.FileMode.CreateNew -> System.IO.FileMode.CreateNew
        |NovelFS.NovelIO.FileMode.Create -> System.IO.FileMode.Create
        |NovelFS.NovelIO.FileMode.Open -> System.IO.FileMode.Open
        |NovelFS.NovelIO.FileMode.OpenOrCreate -> System.IO.FileMode.OpenOrCreate
        |NovelFS.NovelIO.FileMode.Truncate -> System.IO.FileMode.Truncate
        |NovelFS.NovelIO.FileMode.Append -> System.IO.FileMode.Append

    /// Converts a NovelIO file mode to a System.IO.FileMode
    let fileAccessToSystemIOFileAccess (fa : NovelFS.NovelIO.FileAccess) =
        match fa with
        |NovelFS.NovelIO.FileAccess.Read -> System.IO.FileAccess.Read
        |NovelFS.NovelIO.FileAccess.Write -> System.IO.FileAccess.Write
        |NovelFS.NovelIO.FileAccess.ReadWrite -> System.IO.FileAccess.ReadWrite

