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

/// A memory buffer that can be read from and written to
type MemBuffer = private {MemStream : MemoryStream}

/// Operations on memory buffers
module MemoryBuffer =
    /// Creates an expandable memory buffer with zero initial size
    let createExpandable() = {MemStream = new MemoryStream()}

    /// Creates a non-expandable memory buffer from the supplied byte array
    let createFromByteArray (arr : byte array) = {MemStream = new MemoryStream(arr)}

    /// Create a channel from a memory buffer
    let bufferToTextChannel buffer =
        IO.fromEffectful (fun _ ->
            {
                TextReader = new StreamReader(buffer.MemStream) |> Some;
                TextWriter = new StreamWriter(buffer.MemStream) |> Some;
            })

     /// Create a binary channel from a memory buffer
    let bufferToBinaryChannel buffer =
        IO.fromEffectful (fun _ ->
            {
                IOStream = buffer.MemStream;
                EOS = false
            })
    

