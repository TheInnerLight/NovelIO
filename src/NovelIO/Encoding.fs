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

module Encoding =
    type UTF8Encoding = {EmitIdentifier : bool}
    type UnicodeEncoding = {Endianness : Endianness; ByteOrderMark : bool}
    type UTF32Encoding = {Endianness : Endianness; ByteOrderMark : bool}

    type Encoding = 
        |Ascii
        |UTF7
        |UTF8 of UTF8Encoding
        |Unicode of UnicodeEncoding
        |UTF32 of UTF32Encoding

    /// Creates a .NET encoding for the supplied encoding, using the supplied endianness if it's not specified in the encoding
    let createDotNetEncoding enc =
        match enc with
        |Ascii -> System.Text.Encoding.ASCII
        |UTF7 -> System.Text.Encoding.UTF7
        |UTF8 {EmitIdentifier = ident} -> 
            System.Text.UTF8Encoding(ident) :> System.Text.Encoding
        |Unicode {Endianness = endianness; ByteOrderMark = mark} -> 
            System.Text.UnicodeEncoding(ByteOrder.isBigEndian  endianness, mark) :> System.Text.Encoding
        |UTF32 {Endianness = endianness; ByteOrderMark = mark} -> 
            System.Text.UTF32Encoding(ByteOrder.isBigEndian  endianness, mark) :> System.Text.Encoding

    /// Gets the length of the supplied string in the supplied encoding
    let byteLength encoding (str : string) =
        let enc = createDotNetEncoding  encoding
        enc.GetByteCount str

    /// Gets the length of the preamble in the supplied encoding
    let preambleLength encoding =
        Array.length <| (createDotNetEncoding encoding).GetPreamble()

