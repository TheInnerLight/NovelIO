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

namespace NovelFS.NovelIO.UnitTests

open NovelFS.NovelIO
open NovelFS.NovelIO.BinaryPickler
open FsCheck
open FsCheck.Xunit

type ``Binary Pickler Tests`` =
    [<Property>]
    static member ``Unpickle byte from array of one byte`` (byte : byte) =
        let bytes = [|byte|]
        let bytePickler = BinaryPickler.bytePU
        let result = BinaryPickler.unpickle bytePickler bytes 
        result = byte

    [<Property>]
    static member ``Unpickle int16 from array of bytes`` (i16 : int16) =
        let bytes = System.BitConverter.GetBytes i16
        let int16Pickler = BinaryPickler.int16PU
        let result = BinaryPickler.unpickle int16Pickler bytes
        result = i16

    [<Property>]
    static member ``Unpickle int16 (little endian) from array of bytes`` (i16 : int16) =
        let bytes = EndianHelper.convertToEndianness LittleEndian <| System.BitConverter.GetBytes i16
        let result = BinaryPickler.unpickle BinaryPickler.LittleEndian.int16PU bytes
        result = i16

    [<Property>]
    static member ``Unpickle int16 (big endian) from array of bytes`` (i16 : int16) =
        let bytes = EndianHelper.convertToEndianness BigEndian <| System.BitConverter.GetBytes i16
        let result = BinaryPickler.unpickle BinaryPickler.BigEndian.int16PU bytes
        result = i16

    [<Property>]
    static member ``Unpickle int from array of bytes`` (i32 : int32) =
        let bytes = System.BitConverter.GetBytes i32
        let int32Pickler = BinaryPickler.intPU
        let result = BinaryPickler.unpickle int32Pickler bytes
        result = i32

    [<Property>]
    static member ``Unpickle int32 (little endian) from array of bytes`` (i32 : int32) =
        let bytes = EndianHelper.convertToEndianness LittleEndian <| System.BitConverter.GetBytes i32
        let result = BinaryPickler.unpickle BinaryPickler.LittleEndian.intPU bytes
        result = i32

    [<Property>]
    static member ``Unpickle int32 (big endian) from array of bytes`` (i32 : int32) =
        let bytes = EndianHelper.convertToEndianness BigEndian <| System.BitConverter.GetBytes i32
        let result = BinaryPickler.unpickle BinaryPickler.BigEndian.intPU bytes
        result = i32

    [<Property>]
    static member ``Unpickle int64 from array of bytes`` (i64 : int64) =
        let bytes = System.BitConverter.GetBytes i64
        let int64Pickler = BinaryPickler.int64PU
        let result = BinaryPickler.unpickle int64Pickler bytes
        result = i64

    [<Property>]
    static member ``Unpickle int64 (little endian) from array of bytes`` (i64 : int64) =
        let bytes = EndianHelper.convertToEndianness LittleEndian <| System.BitConverter.GetBytes i64
        let result = BinaryPickler.unpickle BinaryPickler.LittleEndian.int64PU bytes
        result = i64

    [<Property>]
    static member ``Unpickle int64 (big endian) from array of bytes`` (i64 : int64) =
        let bytes = EndianHelper.convertToEndianness BigEndian <| System.BitConverter.GetBytes i64
        let result = BinaryPickler.unpickle BinaryPickler.BigEndian.int64PU bytes
        result = i64

    [<Property>]
    static member ``Unpickle float64 from array of bytes`` (flt : float) =
        let bytes = System.BitConverter.GetBytes flt
        let floatPickler = BinaryPickler.floatPU
        let result = BinaryPickler.unpickle floatPickler bytes
        match result with
        |x when System.Double.IsNaN(x) -> System.Double.IsNaN(flt)
        |_ -> result = flt

    [<Property>]
    static member ``Unpickle float64 (little endian) from array of bytes`` (flt : float) =
        let bytes = EndianHelper.convertToEndianness LittleEndian <| System.BitConverter.GetBytes flt
        let result = BinaryPickler.unpickle BinaryPickler.LittleEndian.floatPU bytes
        match result with
        |x when System.Double.IsNaN(x) -> System.Double.IsNaN(flt)
        |_ -> result = flt

    [<Property>]
    static member ``Unpickle float64 (big endian) from array of bytes`` (flt : float) =
        let bytes = EndianHelper.convertToEndianness BigEndian <| System.BitConverter.GetBytes flt
        let result = BinaryPickler.unpickle BinaryPickler.BigEndian.floatPU bytes
        match result with
        |x when System.Double.IsNaN(x) -> System.Double.IsNaN(flt)
        |_ -> result = flt

    [<Property>]
    static member ``Unpickle float32 from array of bytes`` (flt : float32) =
        let bytes = System.BitConverter.GetBytes flt
        let float32Pickler = BinaryPickler.float32PU
        let result = BinaryPickler.unpickle float32Pickler bytes
        match result with
        |x when System.Single.IsNaN(x) -> System.Single.IsNaN(flt)
        |_ -> result = flt

    [<Property>]
    static member ``Unpickle float32 (little endian) from array of bytes`` (flt : float32) =
        let bytes = EndianHelper.convertToEndianness LittleEndian <| System.BitConverter.GetBytes flt
        let result = BinaryPickler.unpickle BinaryPickler.LittleEndian.float32PU bytes
        match result with
        |x when System.Single.IsNaN(x) -> System.Single.IsNaN(flt)
        |_ -> result = flt

    [<Property>]
    static member ``Unpickle float32 (big endian) from array of bytes`` (flt : float32) =
        let bytes = EndianHelper.convertToEndianness BigEndian <| System.BitConverter.GetBytes flt
        let result = BinaryPickler.unpickle BinaryPickler.BigEndian.float32PU bytes
        match result with
        |x when System.Single.IsNaN(x) -> System.Single.IsNaN(flt)
        |_ -> result = flt

    [<Property>]
    static member ``Unpickle decimal from array of bytes`` (dec : decimal) =
        let bytes = 
            System.Decimal.GetBits dec
            |> Array.collect (System.BitConverter.GetBytes)
        let decPickler = BinaryPickler.decimalPU
        let result = BinaryPickler.unpickle decPickler bytes
        result = dec

    [<Property>]
    static member ``Unpickle length prefixed Ascii string from array of bytes`` (nStr : NonEmptyString) =
        let str = nStr.Get 
        let bytesWOPrefix = System.Text.Encoding.ASCII.GetBytes str
        let bytes = 
            Array.concat 
                [System.BitConverter.GetBytes (Array.length bytesWOPrefix);
                 bytesWOPrefix]
        let stringPickler = BinaryPickler.asciiPU
        let result = BinaryPickler.unpickle stringPickler bytes
        result = str

    [<Property>]
    static member ``Unpickle null terminated Ascii string from array of bytes`` (nStr : NonEmptyString) =
        let str = (nStr.Get |> String.filter((<>) '\000')) 
        let bytesWOPrefix = System.Text.Encoding.ASCII.GetBytes (str + string '\000')
        let stringPickler = BinaryPickler.nullTerminated BinaryPickler.asciiCharPU
        let result = BinaryPickler.unpickle stringPickler bytesWOPrefix
        result = str

    [<Property>]
    static member ``Unpickle UTF-7 string from array of bytes`` (nStr : NonEmptyString) =
        let str = nStr.Get 
        let bytesWOPrefix = System.Text.Encoding.UTF7.GetBytes str
        let bytes = 
            Array.concat 
                [System.BitConverter.GetBytes (Array.length bytesWOPrefix);
                 bytesWOPrefix]
        let stringPickler = BinaryPickler.utf7PU
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
        let stringPickler = BinaryPickler.utf8PU
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
        let stringPickler = BinaryPickler.LittleEndian.utf16PU
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
        let stringPickler = BinaryPickler.BigEndian.utf16PU
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
        let stringPickler = BinaryPickler.LittleEndian.utf32PU
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
        let stringPickler = BinaryPickler.BigEndian.utf32PU
        let result = BinaryPickler.unpickle stringPickler bytes
        result = str

    [<Property>]
    static member ``Pickle byte from one byte`` (byte : byte) =
        let bytes = [|byte|]
        let bytePickler = BinaryPickler.bytePU
        let result = BinaryPickler.pickle bytePickler byte 
        result = bytes

    [<Property>]
    static member ``Pickle int16 from one int16`` (i16 : int16) =
        let int16Pickler = BinaryPickler.int16PU
        let bytes = BinaryPickler.pickle int16Pickler i16
        let result = BinaryPickler.unpickle int16Pickler bytes
        result = i16

    [<Property>]
    static member ``Pickle int from one int`` (i32 : int32) =
        let int32Pickler = BinaryPickler.intPU
        let bytes = BinaryPickler.pickle int32Pickler i32
        let result = BinaryPickler.unpickle int32Pickler bytes
        result = i32

    [<Property>]
    static member ``Pickle int64 from one int64`` (i64 : int64) =
        let int64Pickler = BinaryPickler.int64PU
        let bytes = BinaryPickler.pickle int64Pickler i64
        let result = BinaryPickler.unpickle int64Pickler bytes
        result = i64

    [<Property>]
    static member ``Pickle float from one float`` (f64 : float) =
        let floatPickler = BinaryPickler.floatPU
        let bytes = BinaryPickler.pickle floatPickler f64
        let result = BinaryPickler.unpickle floatPickler bytes
        match result with
        |x when System.Double.IsNaN(x) -> System.Double.IsNaN(f64)
        |_ -> result = f64

    [<Property>]
    static member ``Pickle float32 from one float32`` (f32 : float32) =
        let floatPickler = BinaryPickler.float32PU
        let bytes = BinaryPickler.pickle floatPickler f32
        let result = BinaryPickler.unpickle floatPickler bytes
        match result with
        |x when System.Single.IsNaN(x) -> System.Single.IsNaN(f32)
        |_ -> result = f32

    [<Property>]
    static member ``Pickle decimal from one decimal`` (dec : decimal) =
        let decPickler = BinaryPickler.decimalPU
        let bytes = BinaryPickler.pickle decPickler dec
        let result = BinaryPickler.unpickle decPickler bytes
        result = dec

    [<Property>]
    static member ``Pickle Ascii from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let strPickler = BinaryPickler.asciiPU
        let bytes = BinaryPickler.pickle strPickler str
        let result = BinaryPickler.unpickle strPickler bytes
        result = str

    [<Property>]
    static member ``Pickle UTF-7 from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let strPickler = BinaryPickler.utf7PU
        let bytes = BinaryPickler.pickle strPickler str
        let result = BinaryPickler.unpickle strPickler bytes
        result = str

    [<Property>]
    static member ``Pickle UTF-8 from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let strPickler = BinaryPickler.utf8PU
        let bytes = BinaryPickler.pickle strPickler str
        let result = BinaryPickler.unpickle strPickler bytes
        result = str

    [<Property>]
    static member ``Pickle UTF-8 with byte order mark`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let strPickler = BinaryPickler.utf8BomPU
        let bytes = BinaryPickler.pickle strPickler str
        let result = BinaryPickler.unpickle strPickler bytes
        result = str

    [<Property>]
    static member ``Pickle Little Endian UTF-16 from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let strPickler = BinaryPickler.LittleEndian.utf16PU
        let bytes = BinaryPickler.pickle strPickler str
        let result = BinaryPickler.unpickle strPickler bytes
        result = str

    [<Property>]
    static member ``Pickle Big Endian UTF-16 from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let strPickler = BinaryPickler.BigEndian.utf16PU
        let bytes = BinaryPickler.pickle strPickler str
        let result = BinaryPickler.unpickle strPickler bytes
        result = str

    [<Property>]
    static member ``Pickle UTF-16 with Endianness detection from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let strPickler = BinaryPickler.utf16PU
        let bytes = BinaryPickler.pickle strPickler str
        let result = BinaryPickler.unpickle strPickler bytes
        result = str

    [<Property>]
    static member ``Pickle Little Endian UTF-32 from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let strPickler = BinaryPickler.LittleEndian.utf32PU
        let bytes = BinaryPickler.pickle strPickler str
        let result = BinaryPickler.unpickle strPickler bytes
        result = str

    [<Property>]
    static member ``Pickle Big Endian UTF-32 from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let strPickler = BinaryPickler.BigEndian.utf32PU
        let bytes = BinaryPickler.pickle strPickler str
        let result = BinaryPickler.unpickle strPickler bytes
        result = str

    [<Property>]
    static member ``Pickle UTF-32 with Endianness detection from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let strPickler = BinaryPickler.utf32PU
        let bytes = BinaryPickler.pickle strPickler str
        let result = BinaryPickler.unpickle strPickler bytes
        result = str

// ********************************************************
// ************** INCREMENTAL PICKLING TESTS **************
// ********************************************************

type ``Incremental Binary Pickler Tests`` =
    [<Property>]
    static member ``Unpickle byte from array of one byte`` (byte : byte) =
        let bytes = [|byte|]
        let buff = MemoryBuffer.createFromByteArray bytes
        let bytePickler = BinaryPickler.bytePU
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            return! BinaryPickler.unpickleIncr bytePickler bChannel
        } |> IO.run = byte

    [<Property>]
    static member ``Unpickle int16 from array of bytes`` (i16 : int16) =
        let bytes = System.BitConverter.GetBytes i16
        let buff = MemoryBuffer.createFromByteArray bytes
        let int16Pickler = BinaryPickler.int16PU
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            return! BinaryPickler.unpickleIncr int16Pickler bChannel
        } |> IO.run = i16

    [<Property>]
    static member ``Unpickle int from array of bytes`` (i32 : int32) =
        let bytes = System.BitConverter.GetBytes i32
        let buff = MemoryBuffer.createFromByteArray bytes
        let int32Pickler = BinaryPickler.intPU
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            return! BinaryPickler.unpickleIncr int32Pickler bChannel
        } |> IO.run = i32

    [<Property>]
    static member ``Unpickle int64 from array of bytes`` (i64 : int64) =
        let bytes = System.BitConverter.GetBytes i64
        let buff = MemoryBuffer.createFromByteArray bytes
        let int32Pickler = BinaryPickler.int64PU
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            return! BinaryPickler.unpickleIncr int32Pickler bChannel
        } |> IO.run = i64
        
    [<Property>]
    static member ``Unpickle float32 from array of bytes`` (f32 : float32) =
        let bytes = System.BitConverter.GetBytes f32
        let buff = MemoryBuffer.createFromByteArray bytes
        let float32Pickler = BinaryPickler.float32PU
        let result = 
            io {
                let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
                return! BinaryPickler.unpickleIncr float32Pickler bChannel
            } |> IO.run 
        match result with
        |x when System.Single.IsNaN(x) -> System.Single.IsNaN(f32)
        |_ -> result = f32

    [<Property>]
    static member ``Unpickle float from array of bytes`` (f64 : float) =
        let bytes = System.BitConverter.GetBytes f64
        let buff = MemoryBuffer.createFromByteArray bytes
        let float64Pickler = BinaryPickler.floatPU
        let result = 
            io {
                let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
                return! BinaryPickler.unpickleIncr float64Pickler bChannel
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
        let decPickler = BinaryPickler.decimalPU
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            return! BinaryPickler.unpickleIncr decPickler bChannel
        } |> IO.run = dec

    [<Property>]
    static member ``Unpickle length prefixed Ascii string from array of bytes`` (nStr : NonEmptyString) =
        let str = nStr.Get 
        let bytesWOPrefix = System.Text.Encoding.ASCII.GetBytes str
        let bytes = 
            Array.concat 
                [System.BitConverter.GetBytes (Array.length bytesWOPrefix);
                 bytesWOPrefix]
        let buff = MemoryBuffer.createFromByteArray bytes
        let stringPickler = BinaryPickler.asciiPU
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            return! BinaryPickler.unpickleIncr stringPickler bChannel
        } |> IO.run = str

    [<Property>]
    static member ``Unpickle null terminated Ascii string from array of bytes`` (nStr : NonEmptyString) =
        let str = (nStr.Get |> String.filter((<>) '\000')) 
        let bytes = System.Text.Encoding.ASCII.GetBytes (str + string '\000')
        let stringPickler = BinaryPickler.nullTerminated BinaryPickler.asciiCharPU
        let buff = MemoryBuffer.createFromByteArray bytes
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            return! BinaryPickler.unpickleIncr stringPickler bChannel
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
        let stringPickler = BinaryPickler.utf7PU
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            return! BinaryPickler.unpickleIncr stringPickler bChannel
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
        let stringPickler = BinaryPickler.utf8PU
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            return! BinaryPickler.unpickleIncr stringPickler bChannel
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
        let stringPickler = BinaryPickler.LittleEndian.utf16PU
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            return! BinaryPickler.unpickleIncr stringPickler bChannel
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
        let stringPickler = BinaryPickler.BigEndian.utf16PU
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            return! BinaryPickler.unpickleIncr stringPickler bChannel
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
        let stringPickler = BinaryPickler.LittleEndian.utf32PU
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            return! BinaryPickler.unpickleIncr stringPickler bChannel
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
        let stringPickler = BinaryPickler.BigEndian.utf32PU
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            return! BinaryPickler.unpickleIncr stringPickler bChannel
        } |> IO.run = str


    [<Property>]
    static member ``Pickle byte from one byte`` (byte : byte) =
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            do! BinaryPickler.pickleIncr (BinaryPickler.bytePU) bChannel byte
            do! BinaryChannel.setAbsPosition bChannel 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.bytePU) bChannel
        } |> IO.run = byte

    [<Property>]
    static member ``Pickle int16 from one int16`` (i16 : int16) =
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            do! BinaryPickler.pickleIncr (BinaryPickler.int16PU) bChannel i16
            do! BinaryChannel.setAbsPosition bChannel 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.int16PU) bChannel
        } |> IO.run = i16

    [<Property>]
    static member ``Pickle int from one int`` (i32 : int32) =
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            do! BinaryPickler.pickleIncr (BinaryPickler.intPU) bChannel i32
            do! BinaryChannel.setAbsPosition bChannel 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.intPU) bChannel
        } |> IO.run = i32

    [<Property>]
    static member ``Pickle int64 from one int64`` (i64 : int64) =
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            do! BinaryPickler.pickleIncr (BinaryPickler.int64PU) bChannel i64
            do! BinaryChannel.setAbsPosition bChannel 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.int64PU) bChannel
        } |> IO.run = i64

    [<Property>]
    static member ``Pickle float from one float`` (f64 : float) =
        let buff = MemoryBuffer.createExpandable()
        let result = 
            io {
                let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
                do! BinaryPickler.pickleIncr (BinaryPickler.floatPU) bChannel f64
                do! BinaryChannel.setAbsPosition bChannel 0L
                return! BinaryPickler.unpickleIncr (BinaryPickler.floatPU) bChannel
            } |> IO.run
        match result with
        |x when System.Double.IsNaN(x) -> System.Double.IsNaN(f64)
        |_ -> result = f64

    [<Property>]
    static member ``Pickle float32 from one float32`` (f32 : float32) =
        let buff = MemoryBuffer.createExpandable()
        let result = 
            io {
                let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
                do! BinaryPickler.pickleIncr (BinaryPickler.float32PU) bChannel f32
                do! BinaryChannel.setAbsPosition bChannel 0L
                return! BinaryPickler.unpickleIncr (BinaryPickler.float32PU) bChannel
            } |> IO.run
        match result with
        |x when System.Single.IsNaN(x) -> System.Single.IsNaN(f32)
        |_ -> result = f32

    [<Property>]
    static member ``Pickle decimal from one decimal`` (dec : decimal) =
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            do! BinaryPickler.pickleIncr (BinaryPickler.decimalPU) bChannel dec
            do! BinaryChannel.setAbsPosition bChannel 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.decimalPU) bChannel
        } |> IO.run = dec

    [<Property>]
    static member ``Pickle Ascii from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            do! BinaryPickler.pickleIncr (BinaryPickler.asciiPU) bChannel str
            do! BinaryChannel.setAbsPosition bChannel 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.asciiPU) bChannel
        } |> IO.run = str

    [<Property>]
    static member ``Pickle UTF-7 from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            do! BinaryPickler.pickleIncr (BinaryPickler.utf7PU) bChannel str
            do! BinaryChannel.setAbsPosition bChannel 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.utf7PU) bChannel
        } |> IO.run = str

    [<Property>]
    static member ``Pickle UTF-8 from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            do! BinaryPickler.pickleIncr (BinaryPickler.utf8PU) bChannel str
            do! BinaryChannel.setAbsPosition bChannel 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.utf8PU) bChannel
        } |> IO.run = str

    [<Property>]
    static member ``Pickle UTF-8 with byte order mark`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            do! BinaryPickler.pickleIncr (BinaryPickler.utf8BomPU) bChannel str
            do! BinaryChannel.setAbsPosition bChannel 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.utf8BomPU) bChannel
        } |> IO.run = str

    [<Property>]
    static member ``Pickle Little Endian UTF-16 from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            do! BinaryPickler.pickleIncr (BinaryPickler.LittleEndian.utf16PU) bChannel str
            do! BinaryChannel.setAbsPosition bChannel 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.LittleEndian.utf16PU) bChannel
        } |> IO.run = str

    [<Property>]
    static member ``Pickle Big Endian UTF-16 from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            do! BinaryPickler.pickleIncr (BinaryPickler.BigEndian.utf16PU) bChannel str
            do! BinaryChannel.setAbsPosition bChannel 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.BigEndian.utf16PU) bChannel
        } |> IO.run = str

    [<Property>]
    static member ``Pickle UTF-16 with Endianness detection from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            do! BinaryPickler.pickleIncr (BinaryPickler.utf16PU) bChannel str
            do! BinaryChannel.setAbsPosition bChannel 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.utf16PU) bChannel
        } |> IO.run = str

    [<Property>]
    static member ``Pickle Little Endian UTF-32 from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            do! BinaryPickler.pickleIncr (BinaryPickler.LittleEndian.utf32PU) bChannel str
            do! BinaryChannel.setAbsPosition bChannel 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.LittleEndian.utf32PU) bChannel
        } |> IO.run = str

    [<Property>]
    static member ``Pickle Big Endian UTF-32 from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            do! BinaryPickler.pickleIncr (BinaryPickler.BigEndian.utf32PU) bChannel str
            do! BinaryChannel.setAbsPosition bChannel 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.BigEndian.utf32PU) bChannel
        } |> IO.run = str

    [<Property>]
    static member ``Pickle UTF-32 with Endianness detection from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let buff = MemoryBuffer.createExpandable()
        io {
            let! bChannel = MemoryBuffer.bufferToBinaryChannel buff
            do! BinaryPickler.pickleIncr (BinaryPickler.utf32PU) bChannel str
            do! BinaryChannel.setAbsPosition bChannel 0L
            return! BinaryPickler.unpickleIncr (BinaryPickler.utf32PU) bChannel
        } |> IO.run = str



    
        


