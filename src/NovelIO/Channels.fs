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
        /// Writes a string to a text channel
        let putStr (str : string) channel =
            putStrF (fun txtWrtr -> txtWrtr.Write str) channel
        /// Writes a string line to a text channel
        let putStrLn (str : string) channel =
            putStrF (fun txtWrtr -> txtWrtr.WriteLine str) channel
        /// Determines whether a supplied text channel is ready to be read from
        let isChannelReadyToRead channel = 
            match channel.TextReader with
            |Some txtRdr -> txtRdr.Peek() = -1
            |None -> raise ChannelDoesNotSupportReadingException

    /// An action that closes a text channel
    let close channel = IO.fromEffectful (fun _ -> SideEffecting.close channel)

    /// An action that reads a line from the text channel
    let getLine channel = IO.fromEffectful (fun _ -> SideEffecting.getLine channel)

    /// An action that determines if the text channel has data available
    let isReady channel = IO.fromEffectful (fun _ -> SideEffecting.isChannelReadyToRead channel)

    /// An action that writes a line to the text channel
    let putStrLn channel str = IO.fromEffectful (fun _ -> SideEffecting.putStrLn str channel)

/// Operations on binary channels
module BinaryChannel =
    module private SideEffecting = 
        /// Close a binary channel
        let close channel =
            match channel.BinaryReader with
            |Some binRdr -> binRdr.Close()
            |None -> ()
            match channel.BinaryReader with
            |Some binWtr -> binWtr.Close()
            |None -> ()
        /// Sets the absolute position of the binary channel
        let setAbsPosition pos bChannel =
            match bChannel.BinaryReader with
            |Some br -> br.BaseStream.Position <- pos
            |_ -> ()
            match bChannel.BinaryWriter with
            |Some bw -> bw.BaseStream.Position <- pos
            |_ -> ()
        /// Reads from the binary channel with a supplied function
        let read f bChannel =
            match bChannel.BinaryReader with
            |Some bRdr -> f bRdr
            |None -> raise ChannelDoesNotSupportReadingException
        /// Writes to the binary channel with a supplied function
        let write f bChannel =
            match bChannel.BinaryWriter with
            |Some bWrtr -> f bWrtr
            |None -> raise ChannelDoesNotSupportWritingException

    /// An action that closes a binary channel  
    let close channel = IO.fromEffectful (fun _ -> SideEffecting.close channel)

    /// An action that sets the position of the binary channel to the supplied absolute position
    let setAbsPosition channel pos = IO.fromEffectful (fun _ -> SideEffecting.setAbsPosition pos channel)

    /// Channel reading partial byte arrays in different ways
    let private readPartialByteArray channel count f =
        IO.fromEffectful (fun _ ->
            let bytes = Array.zeroCreate<byte> count
            let count' = SideEffecting.read (fun br -> br.Read(bytes, 0, count)) channel
            f count' bytes)

    /// An action that reads up to a specified number of bytes from a channel and returns the result as a byte array
    let readBytes channel maxCount = 
        readPartialByteArray channel maxCount (Array.take)

    /// An action that attempts to read a fixed number of bytes from a channel and returns Some(byte array) if it succeeds or None if it can't satisfy the request.
    let readFixedBytes channel count =
        readPartialByteArray channel count (fun count' bytes ->
            match count = count' with
            |true -> Some bytes
            |false -> None)

    /// An action that writes a supplied array of bytes to the binary channel
    let writeBytes channel (bytes : byte[]) = IO.fromEffectful (fun _ -> SideEffecting.write (fun bw -> bw.Write bytes) channel)