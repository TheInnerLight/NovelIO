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

/// Operations on text channels
module TextChannel =
    module private SideEffecting =
        /// Writes a a string to the text channel with a supplied function
        let putStrF f channel =
            match channel.TextWriter with
            |Some txtWrtr ->
                f txtWrtr
                txtWrtr.Flush()
            |None -> raise ChannelDoesNotSupportWritingException
        /// Close a text channel
        let close channel =
            match channel.TextReader with
            |Some txtRdr -> txtRdr.Close()
            |None -> ()
            match channel.TextWriter with
            |Some txtWtr -> txtWtr.Close()
            |None -> ()
        /// Gets a line from a text channel
        let getLine channel =
            match channel.TextReader with
            |Some txtRdr -> txtRdr.ReadLine()
            |None -> raise ChannelDoesNotSupportReadingException
        /// Gets a line from a text channel asynchronously
        let getLineAsync channel =
            match channel.TextReader with
            |Some txtRdr -> async { return! Async.AwaitTask <| txtRdr.ReadLineAsync()}
            |None -> raise ChannelDoesNotSupportReadingException
        /// Writes a string to a text channel
        let putStr (str : string) channel =
            putStrF (fun txtWrtr -> txtWrtr.Write str) channel
        /// Writes a string line to a text channel
        let putStrLn (str : string) channel =
            putStrF (fun txtWrtr -> txtWrtr.WriteLine str) channel
        /// Determines whether a supplied text channel is ready to be read from
        let isChannelReadyToRead channel = 
            match channel.TextReader with
            |Some txtRdr -> txtRdr.Peek() <> -1
            |None -> raise ChannelDoesNotSupportReadingException
        /// Determines whether a supplied text channel has reached the end of the stream
        let isChannelAtEndOfStream channel = 
            match channel.TextReader with
            |Some txtRdr -> txtRdr.EndOfStream
            |None -> raise ChannelDoesNotSupportReadingException

    /// An action that closes a text channel
    let close channel = IO.fromEffectful (fun _ -> SideEffecting.close channel)

    /// An action that reads a line from the text channel
    let getLine (channel : TChannel) = 
        match channel.IOMode with
        |Synchronous -> IO.fromEffectful (fun _ -> SideEffecting.getLine channel)
        |_ -> IO.liftAsync <| SideEffecting.getLineAsync channel

    /// An action that determines if the text channel is at the end of the stream.  This a synonym for isEOF
    let isEOS channel = IO.fromEffectful (fun _ -> SideEffecting.isChannelAtEndOfStream channel)

    /// An action that determines if the text channel is at the end of the file.  This a synonym for isEOS
    let isEOF channel = isEOS channel

    /// An action that determines if the text channel has data available
    let isReady channel = IO.fromEffectful (fun _ -> SideEffecting.isChannelReadyToRead channel)

    /// An action that writes a line to the text channel
    let putStrLn channel str = IO.fromEffectful (fun _ -> SideEffecting.putStrLn str channel)

/// Operations on binary channels
module BinaryChannel =
    module internal SideEffecting = 
        /// Close a binary channel
        let close bChannel = bChannel.IOStream.Close()
        /// Sets the absolute position of the binary channel
        let setAbsPosition pos bChannel = bChannel.IOStream.Position <- pos

        /// Reads from the binary channel
        let read bytes pos count bChannel = 
            match bChannel.IOStream.CanRead with
            |true -> 
                let read = bChannel.IOStream.Read(bytes, pos, count)
                if (read = 0) then bChannel.EOS <- true
                read
            |false -> raise ChannelDoesNotSupportReadingException

        /// Reads from the binary channel
        let readExactly count bChannel =  
            let bytes = Array.zeroCreate<byte> count
            let mutable readCount = 0
            while readCount < count do
                let justRead = read bytes readCount (count-readCount) bChannel
                if justRead = 0 then raise <| System.IO.EndOfStreamException()
                readCount <- readCount + justRead
            bytes

        /// Reads asynchronously from the binary channel
        let asyncRead bytes pos count bChannel = 
            async {
                match bChannel.IOStream.CanRead with
                |true -> 
                        let! read = bChannel.IOStream.AsyncRead(bytes, pos, count)
                        if (read = 0) then bChannel.EOS <- true
                        return read
                |false -> return! raise ChannelDoesNotSupportReadingException
            }

        /// Reads asynchronously from the binary channel
        let asyncReadExactly count bChannel = 
            async {
                match bChannel.IOStream.CanRead with
                |true -> return! bChannel.IOStream.AsyncRead(count)
                |false -> return! raise ChannelDoesNotSupportReadingException
            }

        /// Writes to the binary channel
        let write bytes bChannel =
            match bChannel.IOStream.CanWrite with
            |true -> bChannel.IOStream.Write(bytes, 0, bytes.Length)
            |false -> raise ChannelDoesNotSupportWritingException

        /// Writes asynchronously to the binary channel
        let asyncWrite bytes bChannel = 
            async {
                match bChannel.IOStream.CanWrite with
                |true -> return! bChannel.IOStream.AsyncWrite(bytes, 0, bytes.Length)
                |false -> return! raise ChannelDoesNotSupportWritingException
            }

    /// Higher order function for performing channel actions synchrously or asynchronously
    let private syncOrAsync syncFunc asyncA count channel =
        match channel.IOMode with
        |Synchronous | Optimise when count < 1024 -> IO.fromEffectful syncFunc
        |_ -> IO.liftAsync asyncA

    /// Provides a general approach for reading partial byte arrays from a channel
    let private readPartialByteArray channel count =
        // function to synchronously read a partial byte array
        let syncFunc () =
            let bytes = Array.zeroCreate<byte> count
            let count' = SideEffecting.read bytes 0 count channel
            Array.take count' bytes
        // async action to read partial byte array
        let asyncA =
            async {
                let bytes = Array.zeroCreate<byte> count
                let! count' = SideEffecting.asyncRead bytes 0 count channel
                return Array.take count' bytes
                }
        syncOrAsync syncFunc asyncA count channel

    /// Provides a general approach for reading complete byte arrays from a channel
    let private readCompleteByteArray channel count =
        let syncFunc() =  SideEffecting.readExactly count channel
        let asyncA = SideEffecting.asyncReadExactly count channel
        syncOrAsync syncFunc asyncA count channel

    /// An action that closes a binary channel  
    let close channel = IO.fromEffectful (fun _ -> SideEffecting.close channel)

    /// Determines if the end of the channel has been reached
    let isEOS channel = IO.fromEffectful (fun _ -> channel.EOS)

    /// Determines if the end of the channel has been reached
    let isEOF channel = isEOS channel

    /// An action that reads up to a specified number of bytes from a channel and returns the result as a byte array
    let read channel maxCount = readPartialByteArray channel maxCount

    /// An action that reads exactly count bytes from a channel and throws an exception if the end of the stream is reached
    let readExactly channel count = readCompleteByteArray channel count

    /// An action that sets the position of the binary channel to the supplied absolute position
    let setAbsPosition channel pos = IO.fromEffectful (fun _ -> SideEffecting.setAbsPosition pos channel)

    /// An action that writes a supplied array of bytes to the binary channel
    let writeBytes channel (bytes : byte[]) = 
        let sync() = SideEffecting.write bytes channel
        let asyncA = SideEffecting.asyncWrite bytes channel
        syncOrAsync sync asyncA (bytes.Length) channel