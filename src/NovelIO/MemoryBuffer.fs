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

type MemBuffer = private {MemStream : MemoryStream}

module MemoryBuffer =
    /// Creates an expandable memory buffer with zero initial size
    let createExpandable() = {MemStream = new MemoryStream()}

    /// Creates a non-expandable memory buffer from the supplied byte array
    let createFromByteArray (arr : byte array) = {MemStream = new MemoryStream(arr)}

    /// Create a handle from a memory buffer
    let bufferToHandle buffer =
        IO.return' 
            {TextReader = new StreamReader(buffer.MemStream) :> TextReader |> Some;
             TextWriter = new StreamWriter(buffer.MemStream) :> TextWriter |> Some}

     /// Create a binary handle from a memory buffer
    let bufferToBinaryHandle buffer =
        IO.return' 
            {BinaryReader = new BinaryReader(buffer.MemStream) |> Some;
             BinaryWriter = new BinaryWriter(buffer.MemStream) |> Some}
    

