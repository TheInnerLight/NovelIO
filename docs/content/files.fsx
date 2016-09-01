(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "NovelIO/NovelIO.dll"
open NovelFS.NovelIO

(**
Working with Files
======================

Working with Files in NovelIO is a little different from standard .NET IO.  Perhaps the most significant change is the handling of file paths.

Standard .NET IO uses `string` to represent file paths.  NovelIO instead uses the 'Filename' type, this is effectively a string wrapper that cannot contain invalid file name characters.

### Creating Filepaths

The first method of creating `Filepath`s is to use active patterns on a string, for example:
*)

match "test.txt" with
|ValidFilePath fName -> Some fName // do something with the valid filename
|InvalidFilePath -> None // handle the invalid filename case

(**
If we know that a conversion to a `Filepath` is definitely going to be succesful, we can instead use `File.assumeValidFilename`
*)

let fName = File.Path.fromValid "test.txt"

(**
Should we be mistaken about the supplied string being a valid filename, an `ArgumentException` will be thrown.

### Using File functions

The `File` modules contains functions very similar to `System.IO.File` defined in standard .NET, these functions are presented using curried arguments and their returns are wrapped by the `IO<'a>` type.
*)

io {
   let! lines = File.readAllLines fName
   return lines
}

(**

## File Channels

If you need more fine-grained control over File IO, the way to achieve this is using Channels.  Text and Binary Channels (`TChannels` and `BChannels`) support explicit reading and writing of their corresponding datatype.

*)

let readFileUntilEnd path =
    File.withTextChannel File.Open.defaultRead path (fun channel ->
        IO.Loops.untilM (TextChannel.isEOF channel) (TextChannel.getLine channel))

(**

It is recommended that you use that the `withChannel` functions provided so that the channel will be automatically cleaned up after its use rather than explicitly opening and closing channels manually.

You can find more about channels on the [channels page](channels.html). 

*)