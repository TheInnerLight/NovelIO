// Warning: generated file; your changes could be lost when a new file is generated.
#I __SOURCE_DIRECTORY__
#load @"load-references.fsx"
#load @"..\Prelude.fs"
      @"..\Helper.fs"
      @"..\Encoding.fs"
      @"..\IO.fs"
      @"..\Actions.fs"
      @"..\Channels.fs"
      @"..\PicklerInfrastructure.fs"
      @"..\BinaryPickler.fs"
      @"..\File.fs"
      @"..\TCP.fs"
      @"..\Random.fs"
      @"..\MemoryBuffer.fs"

open NovelFS.NovelIO
open NovelFS.NovelIO.IO.Operators

let file = File.assumeValidFilename """D:\3DLM SM v3 analysis review 151207.pdf"""

let file2 = File.assumeValidFilename """D:\test.pdf""";;

File.withTextChannel (FileMode.Open) (FileAccess.Read) file (fun chan ->
    File.withTextChannel (FileMode.Create) (FileAccess.Write) file2 (fun chan2 ->
        IO.Loops.untilM (TextChannel.isEOF chan) (TextChannel.getLine chan >>= TextChannel.putStrLn chan2)))
