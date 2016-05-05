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
open NovelFS.NovelIO.BinaryParser
open FsCheck
open FsCheck.Xunit

type ``Binary Parser Tests`` =
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
    static member ``Unpickle int32 from array of bytes`` (i32 : int32) =
        let bytes = System.BitConverter.GetBytes i32
        let int32Pickler = BinaryPickler.pickleInt32
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
    static member ``Unpickle UTF7 string from array of bytes`` (nStr : NonEmptyString) =
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
    static member ``Unpickle UTF8 string from array of bytes`` (nStr : NonEmptyString) =
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
    static member ``Unpickle UTF32 string from array of bytes`` (nStr : NonEmptyString) =
        let str = nStr.Get 
        let bytesWOPrefix = System.Text.Encoding.UTF32.GetBytes str
        let bytes = 
            Array.concat 
                [System.BitConverter.GetBytes (Array.length bytesWOPrefix);
                 bytesWOPrefix]
        let stringPickler = BinaryPickler.pickleUTF32
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
    static member ``Pickle int32 from one int32`` (i32 : int32) =
        let int32Pickler = BinaryPickler.pickleInt32
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
    static member ``Pickle UTF7 from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let strPickler = BinaryPickler.pickleUTF7
        let bytes = BinaryPickler.pickle strPickler str
        let result = BinaryPickler.unpickle strPickler bytes
        result = str

    [<Property>]
    static member ``Pickle UTF8 from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let strPickler = BinaryPickler.pickleUTF8
        let bytes = BinaryPickler.pickle strPickler str
        let result = BinaryPickler.unpickle strPickler bytes
        result = str

    [<Property>]
    static member ``Pickle UTF32 from string`` (nStr : NonEmptyString) =
        let str = nStr.Get
        let strPickler = BinaryPickler.pickleUTF32
        let bytes = BinaryPickler.pickle strPickler str
        let result = BinaryPickler.unpickle strPickler bytes
        result = str

    
        


