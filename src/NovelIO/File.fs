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

/// Side effecting File IO functions used to implement the pure versions
module private SideEffectingFileIO =
    /// Gets the bare string from a filename
    let toFileInfo (filename : FilePath) = FileInfo(filename.PathString)

    /// Returns true if the file is readonly, false otherwise
    let isFileReadOnly file = (toFileInfo file).IsReadOnly

    /// Returns the filesize in bytes
    let fileSize file = 
        (toFileInfo file).Length
        |> LanguagePrimitives.Int64WithMeasure<Bytes>

    /// Create a file channel for a supplied file name, file mode and file access
    let openTextFileChannel (fName : FilePath) mode access =
        let crTxtRdr (fStream : FileStream) = new StreamReader(fStream)
        let crTxtWrtr (fStream : FileStream) = new StreamWriter(fStream)
        let fStream = new FileStream(fName.PathString, InternalIOHelper.fileModeToSystemIOFileMode mode, InternalIOHelper.fileAccessToSystemIOFileAccess access, FileShare.Read, 4096, true)
        let (reader, writer) =
            match access with
            |NovelFS.NovelIO.FileAccess.Read -> Some <| crTxtRdr fStream, None
            |NovelFS.NovelIO.FileAccess.ReadWrite -> Some <| crTxtRdr fStream, Some <| crTxtWrtr fStream
            |NovelFS.NovelIO.FileAccess.Write -> None, Some <| crTxtWrtr fStream
        {TextReader = reader; TextWriter = writer; IOMode = ChannelIOMode.Synchronous}

    /// Create a binary file channel for a supplied file name, file mode and file access
    let openBinaryFileChannel (fName : FilePath) mode access =
        let fStream = new FileStream(fName.PathString, InternalIOHelper.fileModeToSystemIOFileMode mode, InternalIOHelper.fileAccessToSystemIOFileAccess access)
        {IOStream = fStream; IOMode = ChannelIOMode.Optimise; EOS = false}

/// Provides functions relating to the creating, copying, deleting, moving, opening and reading of files
module File =
    /// Turns a string into a filename by assuming the supplied string is a valid filename.  
    /// Throws an ArgumentException if the supplied string is, in fact, not valid.
    let assumeValidFilename path =
        match path with
        |ValidFilePath fname -> fname
        |InvalidFilePath -> invalidArg "path" "Assumption of valid path was not correct."

    /// Gets the bare string from a filename
    let getPathString (filename : FilePath) = filename.PathString

    /// Appends lines to a file, and then closes the file. If the specified file does not exist, this function creates a 
    /// file, writes the specified lines to the file and then closes the file.
    let appendLines (lines : seq<string>) filename =
        IO.fromEffectful (fun _ -> File.AppendAllLines(getPathString filename, lines))

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

    /// Opens a channel to the specified file using the supplied file mode
    let openBinaryChannel mode access (fName : FilePath) =
        IO.fromEffectful (fun _ -> SideEffectingFileIO.openBinaryFileChannel fName mode access)

    /// Opens a channel to the specified file using the supplied file mode and performs the supplied computation fChannel with the channel before cleaning it up.
    let withBinaryChannel mode access (fName : FilePath) fChannel =
        IO.bracket (openBinaryChannel mode access fName) (BinaryChannel.close) fChannel

    /// Opens a channel to the specified file using the supplied file mode
    let openTextChannel mode access (fName : FilePath) =
        IO.fromEffectful (fun _ -> SideEffectingFileIO.openTextFileChannel fName mode access)

    /// Opens a channel to the specified file using the supplied file mode and performs the supplied computation fChannel with the channel before cleaning it up.
    let withTextChannel mode access (fName : FilePath) fChannel =
        IO.bracket (openTextChannel mode access fName) (TextChannel.close) fChannel

    /// Reads all the bytes from a specified file as an array
    let readAllBytes filename = 
        IO.fromEffectful(fun _ -> File.ReadAllBytes <| getPathString filename)

    /// Reads all the lines from a file.
    let readAllLines filename = 
        IO.fromEffectful (fun _ -> List.ofArray << File.ReadAllLines <| getPathString filename)

    /// Reads all the lines from a file in the supplied encoding.
    let readAllLinesIn encoding filename = 
        IO.fromEffectful (fun _ -> List.ofArray <| File.ReadAllLines (getPathString filename, Encoding.createDotNetEncoding encoding))

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

    /// Creates a new file, writes the specified lines to the file and then closes the file. 
    let writeLines (lines : seq<string>) (filename : FilePath)  =
        IO.fromEffectful (fun _ -> File.WriteAllLines(filename.PathString, lines))