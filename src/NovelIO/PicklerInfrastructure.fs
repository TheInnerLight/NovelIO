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

namespace NovelFS.NovelIO.BinaryPickler

open NovelFS.NovelIO

/// The attempted binary pickling exceeded the length of the supplied array
exception PicklingExceededArrayLengthException of int * int

type private BinaryUnpicklerState = {Raw : byte array; Position : int; Endianness : Endianness}
type private BinaryPicklerState = {Raw : byte list; Endianness : Endianness}
type private IncrBinaryUnpicklerState = {Reader : System.IO.BinaryReader}
type private IncrBinaryPicklerState = {Writer : System.IO.BinaryWriter}

type private BUnpickleState =
    |UnpickleComplete of BinaryUnpicklerState
    |UnpickleIncremental of IncrBinaryUnpicklerState

type private BPickleState =
    |PickleComplete of BinaryPicklerState
    |PickleIncremental of IncrBinaryPicklerState

/// Conversions functions
module internal PickleConvertors =
    /// perform some type conversion with exception checks on the array bounds
    let private checkConversionException f pos array =
        try
            f array
        with
            | :? System.ArgumentOutOfRangeException as aoex -> raise <| PicklingExceededArrayLengthException(pos, Array.length array)
            | :? System.ArgumentException as aex -> raise <| PicklingExceededArrayLengthException(pos, Array.length array)

    let private flipForEndianness endianness arr =
        match System.BitConverter.IsLittleEndian, endianness with
        |true, BigEndian |false, LittleEndian -> Array.rev arr
        |_ -> arr

    /// Convert a chunk of a byte array into an bool with exception checking
    let convToBool pos array =
        let unchecked pos arr = System.BitConverter.ToBoolean(arr, pos)
        checkConversionException (unchecked pos) pos array

    /// Convert a chunk of a byte array into an int16 with exception checking
    let convToInt16 endianness pos array = 
        let unchecked pos arr = System.BitConverter.ToInt16(arr, pos)
        checkConversionException (unchecked pos << flipForEndianness endianness) pos array

    /// Convert a chunk of a byte array into an int32 with exception checking
    let convToInt32 endianness pos array = 
        let unchecked pos arr = System.BitConverter.ToInt32(arr, pos)
        checkConversionException (unchecked pos << flipForEndianness endianness) pos array

    /// Convert a chunk of a byte array into an int64 with exception checking
    let convToInt64 endianness pos array = 
        let unchecked pos arr = System.BitConverter.ToInt64(arr, pos)
        checkConversionException (unchecked pos << flipForEndianness endianness) pos array

    /// Convert a chunk of a byte array into an float32 with exception checking
    let convToFloat32 endianness pos array = 
        let unchecked pos arr = System.BitConverter.ToSingle(arr, pos)
        checkConversionException (unchecked pos << flipForEndianness endianness) pos array

    /// Convert a chunk of a byte array into an float64 with exception checking
    let convToFloat64 endianness pos array = 
        let unchecked pos arr = System.BitConverter.ToDouble(arr, pos)
        checkConversionException (unchecked pos << flipForEndianness endianness) pos array

    /// Flips an array to produce a list in the opposite order
    let arrayFlipToList a =  Array.fold(fun lst it -> it :: lst) [] a

    /// Converts a bool to a byte list in reverse order
    let convFromBool (b : bool) = 
        System.BitConverter.GetBytes b 

    /// Converts an int16 to a byte list in reverse order
    let convFromInt16 endianness (i16 : int16) = 
        flipForEndianness endianness <| System.BitConverter.GetBytes i16

    /// Converts an int32 to a byte list in reverse order
    let convFromInt32 endianness (i32 : int32) = 
        flipForEndianness endianness <| System.BitConverter.GetBytes i32

    /// Converts an int64 to a byte list in reverse order
    let convFromInt64 endianness (i64 : int64) = 
        flipForEndianness endianness <| System.BitConverter.GetBytes i64

    /// Converts an float32 to a byte list in reverse order
    let convFromFloat32 endianness (f32 : float32) = 
        flipForEndianness endianness <| System.BitConverter.GetBytes f32

    /// Converts an float64 to a byte list in reverse order  
    let convFromFloat64 endianness (f64 : float) = 
        flipForEndianness endianness <| System.BitConverter.GetBytes f64

    /// Encoding conversion functions
    module Encodings =
        /// Convert a chunk of a byte array into a string with exception checking using the supplied .NET encoding
        let private convToStringWithDotNetEncoding pos byteCount (encoding : System.Text.Encoding) array =
            let len = Array.length <| encoding.GetPreamble()
            let unchecked pos arr = encoding.GetString (arr, pos + len, byteCount)
            checkConversionException (unchecked pos) pos array

        /// Convert a string into a byte list in reverse order using the supplied .NET encoding
        let private convFromStringWithDotNetEncoding (encoding : System.Text.Encoding) (str : string) =
            Array.concat [encoding.GetPreamble(); encoding.GetBytes str]

        /// Convert a chunk of a byte array into a string with exception checking using the supplied encoding
        let convToEncoding encoding byteCount pos array =
            convToStringWithDotNetEncoding pos byteCount (Encoding.createDotNetEncoding encoding) array

        /// Convert a string into a byte list in reverse order using the supplied encoding
        let convFromEncoding encoding str = 
            convFromStringWithDotNetEncoding (Encoding.createDotNetEncoding encoding) str

