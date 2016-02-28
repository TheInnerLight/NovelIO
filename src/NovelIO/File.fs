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

/// Side effecting File IO functions used to implement the pure versions
module private SideEffectingFileIO =
    /// Gets the bare string from a filename
    let toFileInfo (filename : Filename) = FileInfo(filename.PathString)

    /// Returns true if the file is readonly, false otherwise
    let isFileReadOnly file = (toFileInfo file).IsReadOnly

    /// Returns the filesize in bytes
    let fileSize (file) = 
        (toFileInfo file).Length
        |> LanguagePrimitives.Int64WithMeasure<Bytes>

/// Provides functions relating to the creating, copying, deleting, moving, opening and reading of files
module File =
    /// Turns a string into a filename by assuming the supplied string is a valid filename.  
    /// Throws an ArgumentException if the supplied string is, in fact, not valid.
    let assumeValidFilename path =
        match path with
        |ValidFilename fname -> fname
        |InvalidFilename -> invalidArg "path" "Assumption of valid path was not correct."

    /// Gets the bare string from a filename
    let getPathString (filename : Filename) = filename.PathString

    /// Copies an existing file to a location specified.  Overwriting is not allowed
    let copy sourceFile destFile =
        IO.fromEffectful (fun _ -> File.Copy(getPathString sourceFile, getPathString destFile))

    /// Determines the creation date / time of the specified file
    let creationTime filename = 
        IO.fromEffectful (fun _ -> File.GetCreationTime <| getPathString filename)

    /// Determines the UTC creation date / time of the specified file
    let creationTimeUTC filename = 
        IO.fromEffectful (fun _ -> File.GetCreationTimeUtc <| getPathString filename)

    /// Deletes the specified file
    let delete filename =
        IO.fromEffectful (fun _ -> File.Delete <| getPathString filename)

    /// Determines whether or not the specified file exists
    let exists filename = 
        IO.fromEffectful (fun _ -> File.Exists <| getPathString filename)

    /// Determines whether or not the specified file is readonly
    let isReadOnly filename =
        IO.fromEffectful (fun _ -> SideEffectingFileIO.isFileReadOnly filename)

    /// Determines the date / time at which the specified file was last accessed
    let lastAccessTime filename = 
        IO.fromEffectful (fun _ -> File.GetLastAccessTime <| getPathString filename)

    /// Determines the UTC date / time at which the specified file was last accessed
    let lastAccessTimeUTC filename = 
        IO.fromEffectful (fun _ -> File.GetLastAccessTimeUtc <| getPathString filename)

    /// Determines the date / time at which the specified file was last written
    let lastWriteTime filename = 
        IO.fromEffectful (fun _ -> File.GetLastWriteTime <| getPathString filename)

    /// Determines the UTC date / time at which the specified file was last written
    let lastWriteTimeUTC filename = 
        IO.fromEffectful (fun _ -> File.GetLastWriteTimeUtc <| getPathString filename)

    /// Moves an existing file to a location specified.  Overwriting is not allowed
    let move sourceFile destFile =
        IO.fromEffectful (fun _ -> File.Move(getPathString sourceFile, getPathString destFile))

    /// Opens a handle to the specified file using the supplied file mode
    let openFileHandle (mode : FileMode) (access : FileAccess) (fName : Filename) =
        IO.fromEffectful (fun _ -> SideEffectingIO.openFileHandle fName mode access)

    /// Reads all the bytes from a specified file as an array
    let readAllBytes filename = 
        IO.fromEffectful(fun _ -> File.ReadAllBytes <| getPathString filename)

    /// Reads all the lines from a file.
    let readAllLines filename = 
        IO.fromEffectful (fun _ -> List.ofArray << File.ReadAllLines <| getPathString filename)

    /// Reads the lines from a file where each line can be read lazily.
    let readLines filename = 
        IO.fromEffectful (fun _ -> Seq.map (IO.return') (File.ReadLines <| getPathString filename))

    /// Sets the date / time at which the specified file was created
    let setCreationTime datetime filename = 
        IO.fromEffectful (fun _ -> File.SetCreationTime(getPathString filename, datetime))

    /// Sets the UTC date / time at which the specified file was created
    let setCreationTimeUTC datetime filename = 
        IO.fromEffectful (fun _ -> File.SetCreationTimeUtc(getPathString filename, datetime))

    /// Sets the date / time at which the specified file was last accessed
    let setLastAccessTime datetime filename = 
        IO.fromEffectful (fun _ -> File.SetLastAccessTime(getPathString filename, datetime))

    /// Sets the UTC date / time at which the specified file was last accessed
    let setLastAccessTimeUTC datetime filename = 
        IO.fromEffectful (fun _ -> File.SetLastAccessTimeUtc(getPathString filename, datetime))

    /// Sets the date / time at which the specified file was last written
    let setLastWriteTime datetime filename = 
        IO.fromEffectful (fun _ -> File.SetLastWriteTime(getPathString filename, datetime))

    /// Sets the UTC date / time at which the specified file was last written
    let setLastWriteTimeUTC datetime filename = 
        IO.fromEffectful (fun _ -> File.SetLastWriteTimeUtc(getPathString filename, datetime))

    /// Determines the size of the specified file in bytes
    let size filename =
        IO.fromEffectful (fun _ -> SideEffectingFileIO.fileSize filename)