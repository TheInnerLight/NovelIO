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

namespace NovelFS.NovelIO.UnitTests

open NovelFS.NovelIO
open NovelFS.NovelIO.BinaryPickler
open FsCheck
open FsCheck.Xunit

type ``Binary Pickler Tests`` =
    [<Property>]
    static member ``Unpickle byte from array of one byte`` (byte : byte) =
        let bytes = [|byte|]
        let bytePickler = BinaryPickler.pickleByte
        let result = BinaryPickler.unpickle bytePickler bytes 
        result = byte

    [<Property>]
    static member ``Unpickle int16 from array of bytes`` (i16 : int16) =
        let bytes = System.BitConverter.GetBytes i16
        let int16Pickler = BinaryPickler.pickleInt16
        let result = BinaryPickler.unpickle int16Pickler bytes
        result = i16

    [<Property>]
    static member ``Unpickle int from array of bytes`` (i32 : int32) =
        let bytes = System.BitConverter.GetBytes i32
        let int32Pickler = BinaryPickler.pickleInt
        let result = BinaryPickler.unpickle int32Pickler bytes
        result = i32

    [<Property>]
    static member ``Unpickle int64 from array of bytes`` (i64 : int64) =
        let bytes = System.BitConverter.GetBytes i64
        let int64Pickler = BinaryPickler.pickleInt64
        let result = BinaryPickler.unpickle int64Pickler bytes
        result = i64

    [<Property>]
    static member ``Unpickle float64 from array of bytes`` (flt : float) =
        let bytes = System.BitConverter.GetBytes flt
        let floatPickler = BinaryPickler.pickleFloat
        let result = BinaryPickler.unpickle floatPickler bytes
        match result with
        |x when System.Double.IsNaN(x) -> System.Double.IsNaN(flt)
        |_ -> result = flt

    [<Property>]
    static member ``Unpickle float32 from array of bytes`` (flt : float32) =
        let bytes = System.BitConverter.GetBytes flt
        let float32Pickler = BinaryPickler.pickleFloat32
        let result = BinaryPickler.unpickle float32Pickler bytes
        match result with
        |x when System.Single.IsNaN(x) -> System.Single.IsNaN(flt)
        |_ -> result = flt

    [<Property>]
    static member ``Unpickle decimal from array of bytes`` (dec : decimal) =
        let bytes = 
            System.Decimal.GetBits dec
            |> Array.collect (System.BitConverter.GetBytes)
        let decPickler = BinaryPickler.pickleDecimal
        let result = BinaryPickler.unpickle decPickler bytes
        result = dec

    [<Property>]
    static member ``Unpickle Ascii string from array of bytes`` (nStr : NonEmptyString) =
        let str = nStr.Get 
        let bytesWOPrefix = System.Text.Encoding.ASCII.GetBytes str
        let bytes = 
            Array.concat 
                [System.BitConverter.GetBytes (Array.length bytesWOPrefix);
                 bytesWOPrefix]
        let stringPickler = BinaryPickler.pickleAscii
        let result = BinaryPickler.unpickle stringPickler bytes
        result = str

    [<Property>]
    static member ``Unpickle UTF-7 string from array of bytes`` (nStr : NonEmptyString) =
        let str = nStr.Get 
        let bytesWOPrefix = System.Text.Encoding.UTF7.GetBytes str
        let bytes = 
            Array.concat 
                [System.BitConverter.GetBytes (Array.length bytesWOPrefix);
                 bytesWOPrefix]
        let stringPickler = BinaryPickler.pickleUTF7
        let result = BinaryPickler.unpickle stringPickler bytes
        result = str

    [<Property>]
    static member ``Unpickle UTF-8 string from array of bytes`` (nStr : NonEmptyString) =
        let str = nStr.Get 
        let bytesWOPrefix = System.Text.Encoding.UTF8.GetBytes str
        let bytes = 
            Array.concat 
                [System.BitConverter.GetBytes (Array.length bytesWOPrefix);
                 bytesWOPrefix]
        let stringPickler = BinaryPickler.pickleUTF8
        let result = BinaryPickler.unpickle stringPickler bytes
        result = str

    [<Property>]
    static member ``Unpickle Little Endian UTF-16 string from array of bytes`` (nStr : NonEmptyString) =
        let str = nStr.Get 
        let preamble = System.Text.Encoding.Unicode.GetPreamble()
        let bytesWOPrefix = System.Text.Encoding.Unicode.GetBytes str
        let bytes = 
            Array.concat 
                [System.BitConverter.GetBytes (Array.length bytesWOPrefix);
                 bytesWOPrefix]
        let stringPickler = BinaryPickler.pickleUTF16LE
        let result = BinaryPickler.unpickle stringPickler bytes
        result = str

    [<Property>]
    static member ``Unpickle Big Endian UTF-16 string from array of bytes`` (nStr : NonEmptyString) =
        let str = nStr.Get 
        let preamble = System.Text.Encoding.BigEndianUnicode.GetPreamble()
        let bytesWOPrefix = System.Text.Encoding.BigEndianUnicode.GetBytes str
        let bytes = 
            Array.concat 
                [System.BitConverter.GetBytes (Array.length bytesWOPrefix);
                 bytesWOPrefix]
        let stringPickler = BinaryPickler.pickleUTF16BE
        let result = BinaryPickler.unpickle stringPickler bytes
        result = str

    [<Property>]
    static member ``Unpickle Little Endian UTF-32 string from array of bytes`` (nStr : NonEmptyString) =
        let str = nStr.Get 
        let bytesWOPrefix = System.Text.Encoding.UTF32.GetBytes str
        let bytes = 
            Array.concat 
                [System.BitConverter.GetBytes (Array.length bytesWOPrefix);
                 bytesWOPrefix]
        let stringPickler = BinaryPickler.pickleUtf32LE
        let result = BinaryPickler.unpickle stringPickler bytes
        result = str

    [<Property>]
    static member ``Unpickle Big Endian UTF-32 string from array of bytes`` (nStr : NonEmptyString) =
        let str = nStr.Get 
        let bytesWOPrefix = System.Text.UTF32Encoding(true, true).GetBytes str
        let bytes = 
            Array.concat 
                [System.BitConverter.GetBytes (Array.length bytesWOPrefix);
                 bytesWOPrefix]
        let stringPickler = BinaryPickler.pickleUtf32BE
        let result = BinaryPickler.unpickle stringPickler bytes
        result = str

    [<Property>]
    static member ``Pickle byte from one byte`` (byte : byte) =
        let bytes = [|byte|]
        let bytePickler = BinaryPickler.pickleByte
        let result = BinaryPickler.pickle bytePickler byte 
        result = bytes

    [<Property>]
    static member ``Pickle int16 from one int16`` (i16 : int16) =
        let int16Pickler = BinaryPickler.pickleInt16
        let bytes = BinaryPickler.pickle int16Pickler i16
        let result = BinaryPickler.unpickle int16Pickler bytes
        result = i16

    [<Property>]
    static member ``Pickle int from one int`` (i32 : int32) =
        let int32Pickler = BinaryPickler.pickleInt
        let bytes = BinaryPickler.pickle int32Pickler i32
        let result = BinaryPickler.unpickle int32Pickler bytes
        result = i32

    [<Property>]
    static member ``Pickle int64 from one int64`` (i64 : int64) =
        let int64Pickler = BinaryPickler.pickleInt64
        let bytes = BinaryPickler.pickle int64Pickler i64
        let result = BinaryPickler.unpickle int64Pickler bytes
        result = i64

    [<Property>]
    static member ``Pickle float from one float`` (f64 : float) =
        let floatPickler = BinaryPickler.pickleFloat
        let bytes = BinaryPickler.pickle floatPickler f64
        let result = BinaryPickler.unpickle floatPickler bytes
        match result with
        |x when System.Double.IsNaN(x) -> System.Double.IsNaN(f64)
        |_ -> result = f64

    [<Property>]
    static member ``Pickle float32 from one float32`` (f32 : float32) =
        let floatPickler = BinaryPickler.pickleFloat32
        let bytes = BinaryPickler.pickle floatPickler f32
        let result = BinaryPickler.unpickle floatPickler bytes
        match result with
        |x when System.Single.IsNaN(x) -> System.Single.IsNaN(f32)
        |_ -> result = f32

    [<Property>]
    static member ``Pickle decimal from one decimal`` (dec : decimal) =
        let decPickler = BinaryPickler.pickleDecimal
        let bytes = BinaryPickler.pickle decPickler dec
        let result = BinaryPickler.unpickle decPickler bytes
        result = dec

    [<Property>]
    static member ``Pickle Ascii from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let strPickler = BinaryPickler.pickleAscii
        let bytes = BinaryPickler.pickle strPickler str
        let result = BinaryPickler.unpickle strPickler bytes
        result = str

    [<Property>]
    static member ``Pickle UTF-7 from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let strPickler = BinaryPickler.pickleUTF7
        let bytes = BinaryPickler.pickle strPickler str
        let result = BinaryPickler.unpickle strPickler bytes
        result = str

    [<Property>]
    static member ``Pickle UTF-8 from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let strPickler = BinaryPickler.pickleUTF8
        let bytes = BinaryPickler.pickle strPickler str
        let result = BinaryPickler.unpickle strPickler bytes
        result = str

    [<Property>]
    static member ``Pickle UTF-8 with byte order mark`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let strPickler = BinaryPickler.pickleUTF8BOM
        let bytes = BinaryPickler.pickle strPickler str
        let result = BinaryPickler.unpickle strPickler bytes
        result = str

    [<Property>]
    static member ``Pickle Little Endian UTF-16 from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let strPickler = BinaryPickler.pickleUTF16LE
        let bytes = BinaryPickler.pickle strPickler str
        let result = BinaryPickler.unpickle strPickler bytes
        result = str

    [<Property>]
    static member ``Pickle Big Endian UTF-16 from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let strPickler = BinaryPickler.pickleUTF16BE
        let bytes = BinaryPickler.pickle strPickler str
        let result = BinaryPickler.unpickle strPickler bytes
        result = str

    [<Property>]
    static member ``Pickle UTF-16 with Endianness detection from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let strPickler = BinaryPickler.pickleUTF16
        let bytes = BinaryPickler.pickle strPickler str
        let result = BinaryPickler.unpickle strPickler bytes
        result = str

    [<Property>]
    static member ``Pickle Little Endian UTF-32 from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let strPickler = BinaryPickler.pickleUtf32LE
        let bytes = BinaryPickler.pickle strPickler str
        let result = BinaryPickler.unpickle strPickler bytes
        result = str

    [<Property>]
    static member ``Pickle Big Endian UTF-32 from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let strPickler = BinaryPickler.pickleUtf32BE
        let bytes = BinaryPickler.pickle strPickler str
        let result = BinaryPickler.unpickle strPickler bytes
        result = str

    [<Property>]
    static member ``Pickle UTF-32 with Endianness detection from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let strPickler = BinaryPickler.pickleUTF32
        let bytes = BinaryPickler.pickle strPickler str
        let result = BinaryPickler.unpickle strPickler bytes
        result = str

type ``Incremental Binary Pickler Tests`` =
    [<Property>]
    static member ``Unpickle byte from array of one byte`` (byte : byte) =
        let bytes = [|byte|]
        let buff = MemoryBuffer.createFromByteArray bytes
        let bytePickler = BinaryPickler.pickleByte
        io {
            let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
            return! BinaryPickler.unpickleIncr bytePickler bHandle
        } |> IO.run = byte

    [<Property>]
    static member ``Unpickle int16 from array of bytes`` (i16 : int16) =
        let bytes = System.BitConverter.GetBytes i16
        let buff = MemoryBuffer.createFromByteArray bytes
        let int16Pickler = BinaryPickler.pickleInt16
        io {
            let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
            return! BinaryPickler.unpickleIncr int16Pickler bHandle
        } |> IO.run = i16

    [<Property>]
    static member ``Unpickle int from array of bytes`` (i32 : int32) =
        let bytes = System.BitConverter.GetBytes i32
        let buff = MemoryBuffer.createFromByteArray bytes
        let int32Pickler = BinaryPickler.pickleInt
        io {
            let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
            return! BinaryPickler.unpickleIncr int32Pickler bHandle
        } |> IO.run = i32

    [<Property>]
    static member ``Unpickle int64 from array of bytes`` (i64 : int64) =
        let bytes = System.BitConverter.GetBytes i64
        let buff = MemoryBuffer.createFromByteArray bytes
        let int32Pickler = BinaryPickler.pickleInt64
        io {
            let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
            return! BinaryPickler.unpickleIncr int32Pickler bHandle
        } |> IO.run = i64
        
    [<Property>]
    static member ``Unpickle float32 from array of bytes`` (f32 : float32) =
        let bytes = System.BitConverter.GetBytes f32
        let buff = MemoryBuffer.createFromByteArray bytes
        let float32Pickler = BinaryPickler.pickleFloat32
        let result = 
            io {
                let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
                return! BinaryPickler.unpickleIncr float32Pickler bHandle
            } |> IO.run 
        match result with
        |x when System.Single.IsNaN(x) -> System.Single.IsNaN(f32)
        |_ -> result = f32

    [<Property>]
    static member ``Unpickle float from array of bytes`` (f64 : float) =
        let bytes = System.BitConverter.GetBytes f64
        let buff = MemoryBuffer.createFromByteArray bytes
        let float64Pickler = BinaryPickler.pickleFloat
        let result = 
            io {
                let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
                return! BinaryPickler.unpickleIncr float64Pickler bHandle
            } |> IO.run
        match result with
        |x when System.Double.IsNaN(x) -> System.Double.IsNaN(f64)
        |_ -> result = f64

    [<Property>]
    static member ``Unpickle decimal from array of bytes`` (dec : decimal) =
        let bytes = 
            System.Decimal.GetBits dec
            |> Array.collect (System.BitConverter.GetBytes)
        let buff = MemoryBuffer.createFromByteArray bytes
        let decPickler = BinaryPickler.pickleDecimal
        io {
            let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
            return! BinaryPickler.unpickleIncr decPickler bHandle
        } |> IO.run = dec

    [<Property>]
    static member ``Unpickle Ascii string from array of bytes`` (nStr : NonEmptyString) =
        let str = nStr.Get 
        let bytesWOPrefix = System.Text.Encoding.ASCII.GetBytes str
        let bytes = 
            Array.concat 
                [System.BitConverter.GetBytes (Array.length bytesWOPrefix);
                 bytesWOPrefix]
        let buff = MemoryBuffer.createFromByteArray bytes
        let stringPickler = BinaryPickler.pickleAscii
        io {
            let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
            return! BinaryPickler.unpickleIncr stringPickler bHandle
        } |> IO.run = str

    [<Property>]
    static member ``Unpickle UTF-7 string from array of bytes`` (nStr : NonEmptyString) =
        let str = nStr.Get 
        let bytesWOPrefix = System.Text.Encoding.UTF7.GetBytes str
        let bytes = 
            Array.concat 
                [System.BitConverter.GetBytes (Array.length bytesWOPrefix);
                 bytesWOPrefix]
        let buff = MemoryBuffer.createFromByteArray bytes
        let stringPickler = BinaryPickler.pickleUTF7
        io {
            let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
            return! BinaryPickler.unpickleIncr stringPickler bHandle
        } |> IO.run = str

    [<Property>]
    static member ``Unpickle UTF-8 string from array of bytes`` (nStr : NonEmptyString) =
        let str = nStr.Get 
        let bytesWOPrefix = System.Text.Encoding.UTF8.GetBytes str
        let bytes = 
            Array.concat 
                [System.BitConverter.GetBytes (Array.length bytesWOPrefix);
                 bytesWOPrefix]
        let buff = MemoryBuffer.createFromByteArray bytes
        let stringPickler = BinaryPickler.pickleUTF8
        io {
            let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
            return! BinaryPickler.unpickleIncr stringPickler bHandle
        } |> IO.run = str

    [<Property>]
    static member ``Unpickle Little Endian UTF-16 string from array of bytes`` (nStr : NonEmptyString) =
        let str = nStr.Get 
        let preamble = System.Text.Encoding.Unicode.GetPreamble()
        let bytesWOPrefix = System.Text.Encoding.Unicode.GetBytes str
        let bytes = 
            Array.concat 
                [System.BitConverter.GetBytes (Array.length bytesWOPrefix);
                 bytesWOPrefix]
        let buff = MemoryBuffer.createFromByteArray bytes
        let stringPickler = BinaryPickler.pickleUTF16LE
        io {
            let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
            return! BinaryPickler.unpickleIncr stringPickler bHandle
        } |> IO.run = str

    [<Property>]
    static member ``Unpickle Big Endian UTF-16 string from array of bytes`` (nStr : NonEmptyString) =
        let str = nStr.Get 
        let preamble = System.Text.Encoding.BigEndianUnicode.GetPreamble()
        let bytesWOPrefix = System.Text.Encoding.BigEndianUnicode.GetBytes str
        let bytes = 
            Array.concat 
                [System.BitConverter.GetBytes (Array.length bytesWOPrefix);
                 bytesWOPrefix]
        let buff = MemoryBuffer.createFromByteArray bytes
        let stringPickler = BinaryPickler.pickleUTF16BE
        io {
            let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
            return! BinaryPickler.unpickleIncr stringPickler bHandle
        } |> IO.run = str

    [<Property>]
    static member ``Unpickle Little Endian UTF-32 string from array of bytes`` (nStr : NonEmptyString) =
        let str = nStr.Get 
        let bytesWOPrefix = System.Text.Encoding.UTF32.GetBytes str
        let bytes = 
            Array.concat 
                [System.BitConverter.GetBytes (Array.length bytesWOPrefix);
                 bytesWOPrefix]
        let buff = MemoryBuffer.createFromByteArray bytes
        let stringPickler = BinaryPickler.pickleUtf32LE
        io {
            let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
            return! BinaryPickler.unpickleIncr stringPickler bHandle
        } |> IO.run = str

    [<Property>]
    static member ``Unpickle Big Endian UTF-32 string from array of bytes`` (nStr : NonEmptyString) =
        let str = nStr.Get 
        let bytesWOPrefix = System.Text.UTF32Encoding(true, true).GetBytes str
        let bytes = 
            Array.concat 
                [System.BitConverter.GetBytes (Array.length bytesWOPrefix);
                 bytesWOPrefix]
        let buff = MemoryBuffer.createFromByteArray bytes
        let stringPickler = BinaryPickler.pickleUtf32BE
        io {
            let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
            return! BinaryPickler.unpickleIncr stringPickler bHandle
        } |> IO.run = str


    [<Property>]
    static member ``Pickle byte from one byte`` (byte : byte) =
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
            do! BinaryPickler.pickleIncr (BinaryPickler.pickleByte) bHandle byte
            do! IO.bhSetAbsPosition bHandle 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.pickleByte) bHandle
        } |> IO.run = byte

    [<Property>]
    static member ``Pickle int16 from one int16`` (i16 : int16) =
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
            do! BinaryPickler.pickleIncr (BinaryPickler.pickleInt16) bHandle i16
            do! IO.bhSetAbsPosition bHandle 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.pickleInt16) bHandle
        } |> IO.run = i16

    [<Property>]
    static member ``Pickle int from one int`` (i32 : int32) =
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
            do! BinaryPickler.pickleIncr (BinaryPickler.pickleInt) bHandle i32
            do! IO.bhSetAbsPosition bHandle 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.pickleInt) bHandle
        } |> IO.run = i32

    [<Property>]
    static member ``Pickle int64 from one int64`` (i64 : int64) =
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
            do! BinaryPickler.pickleIncr (BinaryPickler.pickleInt64) bHandle i64
            do! IO.bhSetAbsPosition bHandle 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.pickleInt64) bHandle
        } |> IO.run = i64

    [<Property>]
    static member ``Pickle float from one float`` (f64 : float) =
        let buff = MemoryBuffer.createExpandable()
        let result = 
            io {
                let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
                do! BinaryPickler.pickleIncr (BinaryPickler.pickleFloat) bHandle f64
                do! IO.bhSetAbsPosition bHandle 0L
                return! BinaryPickler.unpickleIncr (BinaryPickler.pickleFloat) bHandle
            } |> IO.run
        match result with
        |x when System.Double.IsNaN(x) -> System.Double.IsNaN(f64)
        |_ -> result = f64

    [<Property>]
    static member ``Pickle float32 from one float32`` (f32 : float32) =
        let buff = MemoryBuffer.createExpandable()
        let result = 
            io {
                let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
                do! BinaryPickler.pickleIncr (BinaryPickler.pickleFloat32) bHandle f32
                do! IO.bhSetAbsPosition bHandle 0L
                return! BinaryPickler.unpickleIncr (BinaryPickler.pickleFloat32) bHandle
            } |> IO.run
        match result with
        |x when System.Single.IsNaN(x) -> System.Single.IsNaN(f32)
        |_ -> result = f32

    [<Property>]
    static member ``Pickle decimal from one decimal`` (dec : decimal) =
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
            do! BinaryPickler.pickleIncr (BinaryPickler.pickleDecimal) bHandle dec
            do! IO.bhSetAbsPosition bHandle 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.pickleDecimal) bHandle
        } |> IO.run = dec

    [<Property>]
    static member ``Pickle Ascii from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
            do! BinaryPickler.pickleIncr (BinaryPickler.pickleAscii) bHandle str
            do! IO.bhSetAbsPosition bHandle 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.pickleAscii) bHandle
        } |> IO.run = str

    [<Property>]
    static member ``Pickle UTF-7 from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
            do! BinaryPickler.pickleIncr (BinaryPickler.pickleUTF7) bHandle str
            do! IO.bhSetAbsPosition bHandle 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.pickleUTF7) bHandle
        } |> IO.run = str

    [<Property>]
    static member ``Pickle UTF-8 from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
            do! BinaryPickler.pickleIncr (BinaryPickler.pickleUTF8) bHandle str
            do! IO.bhSetAbsPosition bHandle 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.pickleUTF8) bHandle
        } |> IO.run = str

    [<Property>]
    static member ``Pickle UTF-8 with byte order mark`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
            do! BinaryPickler.pickleIncr (BinaryPickler.pickleUTF8BOM) bHandle str
            do! IO.bhSetAbsPosition bHandle 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.pickleUTF8BOM) bHandle
        } |> IO.run = str

    [<Property>]
    static member ``Pickle Little Endian UTF-16 from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
            do! BinaryPickler.pickleIncr (BinaryPickler.pickleUTF16LE) bHandle str
            do! IO.bhSetAbsPosition bHandle 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.pickleUTF16LE) bHandle
        } |> IO.run = str

    [<Property>]
    static member ``Pickle Big Endian UTF-16 from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
            do! BinaryPickler.pickleIncr (BinaryPickler.pickleUTF16BE) bHandle str
            do! IO.bhSetAbsPosition bHandle 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.pickleUTF16BE) bHandle
        } |> IO.run = str

    [<Property>]
    static member ``Pickle UTF-16 with Endianness detection from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
            do! BinaryPickler.pickleIncr (BinaryPickler.pickleUTF16) bHandle str
            do! IO.bhSetAbsPosition bHandle 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.pickleUTF16) bHandle
        } |> IO.run = str

    [<Property>]
    static member ``Pickle Little Endian UTF-32 from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
            do! BinaryPickler.pickleIncr (BinaryPickler.pickleUtf32LE) bHandle str
            do! IO.bhSetAbsPosition bHandle 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.pickleUtf32LE) bHandle
        } |> IO.run = str

    [<Property>]
    static member ``Pickle Big Endian UTF-32 from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bHandle = MemoryBuffer.bufferToBinaryHandle buff
            do! BinaryPickler.pickleIncr (BinaryPickler.pickleUtf32BE) bHandle str
            do! IO.bhSetAbsPosition bHandle 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.pickleUtf32BE) bHandle
        } |> IO.run = str



    
        


