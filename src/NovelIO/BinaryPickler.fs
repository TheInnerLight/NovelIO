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

/// A pickler/unpickler pair for type 'a
type BinaryPU<'a> = private {Pickle : 'a * BPickleState -> BPickleState; Unpickle : BUnpickleState -> 'a * BUnpickleState}

/// Conversions functions
module private PickleConvertors =
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

/// Provides functions for pickling binary data
module BinaryPickler =
    let private runUnpickle state x =
        match x with
        |{Unpickle = g; Pickle = _} -> g state

    let private runPickle (a, st) x =
        match x with
        |{Unpickle = _; Pickle = g} -> g (a, st)

    /// Helper function that chooses between complete or incremental pickling
    let private pickleHelper f b st =
        match st with
        |PickleComplete ps -> PickleComplete {ps with Raw = (PickleConvertors.arrayFlipToList << f <| b) @ ps.Raw}
        |PickleIncremental ips -> 
            ips.Writer.Write (f b)
            st
    /// Helper function that chooses between complete or incremental unpickling and accepts an arbitrary data-size
    let private unpickleHelperSized size f st =
        match st with
        |UnpickleComplete ps -> 
            let pos = ps.Position
            let result = f pos (ps.Raw)
            result, UnpickleComplete {ps with Position = pos + size}
        |UnpickleIncremental ips ->
            let result = f 0 (ips.Reader.ReadBytes size)
            result, st

    /// Helper function that chooses between complete or incremental unpickling and gets the size from the size of the data type
    let private unpickleHelper (f : int -> byte array -> 'a) st =
        unpickleHelperSized (sizeof<'a>) f st
            

    /// Given a value of x, returns a pickler of x
    let lift x = {Pickle = (fun (_,st) -> st); Unpickle = (fun s -> x, s)}

    /// Creates a sequential combination of picklers 
    let sequ (f : 'b -> 'a) (pa : BinaryPU<'a>) (k : 'a -> BinaryPU<'b>) : BinaryPU<'b> =
        match pa with
        |{Unpickle = unPck; Pickle = pck} ->
            // unpickling is sequenced like bind in the reader monad
            let unPck' s =
                let a, s' = runUnpickle s pa
                runUnpickle s' (k a)
            // pickling requires the extra projection function: f
            let pck' (b, s) =
                let a = f b
                let pb = k a
                runPickle (b, runPickle (a, s) pa) pb
            {Unpickle = unPck'; Pickle = pck'}

    /// Combines two picklers into a pickler that pickles a tuple-2
    let tuple2 pa pb =
        sequ fst pa (fun a ->
            sequ snd pb  (fun b -> lift (a, b)))

    /// Combines three picklers into a pickler that pickles a tuple-3
    let tuple3 pa pb pc =
        sequ (fun (a,_,_) -> a) pa (fun a ->
            sequ (fun (_,b,_) -> b) pb  (fun b ->
                sequ (fun (_,_,c) -> c) pc  (fun c ->
                    lift (a, b, c))))

    /// Combines four picklers into a pickler that pickles a tuple-4
    let tuple4 pa pb pc pd =
        sequ (fun (a,_,_,_) -> a) pa (fun a ->
            sequ (fun (_,b,_,_) -> b) pb  (fun b ->
                sequ (fun (_,_,c,_) -> c) pc  (fun c ->
                    sequ (fun (_,_,_,d) -> d) pd  (fun d ->
                        lift (a, b, c, d)))))

    /// When supplied with a method of transforming between two types reversably and a pickler of the first of those types, returns a 
    /// pickler of the second type.
    let wrap (fab, fba) pa = sequ fba pa (lift << fab)

    /// Repeats a pickler n times to create a list pickler
    let rec repeat pa n =
        match n with
        |0 -> lift []
        |_ ->
            let pb = tuple2 pa (repeat pa (n-1))
            wrap ((fun (a, b) -> (a::b)),(fun (a::b) -> (a,b))) pb

    /// Repeats a pickler n times to create an array pickler
    let repeatA pa n =
        wrap (Array.ofList, List.ofArray) (repeat pa n)

    /// A pickler/unpickler pair for bools
    let boolPU =
        {
        Pickle = fun (b, s) -> pickleHelper (PickleConvertors.convFromBool) b s
        Unpickle = fun st -> unpickleHelper (PickleConvertors.convToBool) st
        }

    /// A pickler/unpickler pair for bytes
    let bytePU =
        {
        Pickle = fun (b, s) -> pickleHelper (Array.singleton) b s
        Unpickle = fun st -> unpickleHelper (Array.item) st
        }

    /// A pickler/unpickler pair for int16s of the supplied endianness
    let private int16PUE endianness =
        {
        Pickle = fun (i16, s) -> pickleHelper (PickleConvertors.convFromInt16 endianness) i16 s
        Unpickle = fun st -> unpickleHelper (PickleConvertors.convToInt16 endianness) st
        }

    /// A pickler/unpickler pair for int32s of the supplied endianness
    let private int32PUE endianness =
        {
        Pickle = fun (i32, s) -> pickleHelper (PickleConvertors.convFromInt32 endianness) i32 s
        Unpickle = fun st -> unpickleHelper (PickleConvertors.convToInt32 endianness) st
        }

    /// A pickler/unpickler pair for int64s of the supplied endianness
    let private int64PUE endianness =
        {
        Pickle = fun (i64, s) -> pickleHelper (PickleConvertors.convFromInt64 endianness) i64 s
        Unpickle = fun st -> unpickleHelper (PickleConvertors.convToInt64 endianness) st
        }

    /// A pickler/unpickler pair for float32s of the supplied endianness
    let private float32PUE endianness =
        {
        Pickle = fun (f32, s) -> pickleHelper (PickleConvertors.convFromFloat32 endianness) f32 s
        Unpickle = fun st -> unpickleHelper (PickleConvertors.convToFloat32 endianness) st
        }

    /// A pickler/unpickler pair for floats of the supplied endianness
    let private floatPUE endianness =
        {
        Pickle = fun (f64, s) -> pickleHelper (PickleConvertors.convFromFloat64 endianness) f64 s
        Unpickle = fun st -> unpickleHelper (PickleConvertors.convToFloat64 endianness) st
        }

    /// A pickler/unpickler pair for decimals of the supplied endianness
    let private decimalPUE endianness =
        let intAToDecimal (a : int[]) = System.Decimal a
        wrap (intAToDecimal, System.Decimal.GetBits) (repeatA (int32PUE endianness) 4)

    /// A pickler/unpickler pair for int16s in the Endianness of the current platform
    let int16PU = int16PUE (ByteOrder.systemEndianness)

    /// A pickler/unpickler pair for int16s in Little Endian byte order
    let int16LittleEPU = int16PUE LittleEndian

    /// A pickler/unpickler pair for int16s in Big Endian byte order
    let int16BigEPU = int16PUE BigEndian

    /// A pickler/unpickler pair for ints in the Endianness of the current platform
    let intPU = int32PUE (ByteOrder.systemEndianness)

    /// A pickler/unpickler pair for ints in Little Endian byte order
    let intLittleEPU = int32PUE LittleEndian

    /// A pickler/unpickler pair for ints in Big Endian byte order
    let intBigEPU = int32PUE BigEndian

    /// A pickler/unpickler pair for int64s in the Endianness of the current platform
    let int64PU = int64PUE (ByteOrder.systemEndianness)

    /// A pickler/unpickler pair for int64s in Little Endian byte order
    let int64LittleEPU = int64PUE LittleEndian

    /// A pickler/unpickler pair for int64s in Big Endian byte order
    let int64BigEPU = int64PUE BigEndian

    /// A pickler/unpickler pair for float32s in the Endianness of the current platform
    let float32PU = float32PUE (ByteOrder.systemEndianness)

    /// A pickler/unpickler pair for float32s in Little Endian byte order
    let float32LittleEPU = float32PUE LittleEndian

    /// A pickler/unpickler pair for float32s in Big Endian byte order
    let float32BigEPU = float32PUE BigEndian

    /// A pickler/unpickler pair for floats in the Endianness of the current platform
    let floatPU = floatPUE (ByteOrder.systemEndianness)

    /// A pickler/unpickler pair for floats in Little Endian byte order
    let floatLittleEPU = floatPUE LittleEndian

    /// A pickler/unpickler pair for floats in Big Endian byte order
    let floatBigEPU = floatPUE BigEndian

    /// A pickler/unpickler pair for decimals in the Endianness of the current platform
    let decimalPU = decimalPUE (ByteOrder.systemEndianness)

    /// A pickler/unpickler pair for decimals in Little Endian byte order
    let decimalLittleEPU = decimalPUE LittleEndian

    /// A pickler/unpickler pair for decimals in Big Endian byte order
    let decimalBigEPU = decimalPUE BigEndian

    /// Accepts a tagging function that partitions the type to be pickled into two sets, then accepts a pickler for each set
    let alt tag ps = sequ tag intPU (flip Map.find <| ps)

    /// A pickler/unpickler pair for lists
    let list pa = sequ (List.length) intPU << repeat <| pa

    /// A pickler/unpickler pair for arrays
    let array pa = sequ (Array.length) intPU << repeatA <| pa

    /// A pickler/unpickler pair for option types
    let optionPU pa = 
        let tag = function
            |Some _ -> 1
            |None -> 0
        let map = Map.ofList [(0, lift None); (1, wrap (Some, Option.get) pa)]
        alt tag map

    /// A pickler/unpickler pair for ASCII strings
    let asciiPU =
        wrap (System.Text.Encoding.ASCII.GetString, System.Text.Encoding.ASCII.GetBytes) (array bytePU)

    /// A pickler/unpickler pair for UTF-7 strings
    let utf7PU =
        wrap (System.Text.Encoding.UTF7.GetString, System.Text.Encoding.UTF7.GetBytes) (array bytePU)

    /// A pickler/unpickler pair for UTF-32 strings
    let encodingPU encoding =
        let pickleEncodingS byteCount = 
            {
            Pickle = fun (str, s) -> pickleHelper (PickleConvertors.Encodings.convFromEncoding encoding) str s
            Unpickle = fun st -> unpickleHelperSized byteCount (PickleConvertors.Encodings.convToEncoding encoding byteCount) st
            }
        sequ (Encoding.byteLength encoding) intPU pickleEncodingS

    /// A pickler/unpickler pair for UTF-8 strings
    let utf8PU =
        encodingPU (Encoding.UTF8 {EmitIdentifier = false})

    /// A pickler/unpickler pair for unicode strings in little endian byte order.  No byte order mark is encoded.
    let utf16LittleEPU =
        encodingPU (Encoding.UTF16 {Endianness = LittleEndian; ByteOrderMark = false})

    /// A pickler/unpickler pair for unicode strings in big endian byte order.  No byte order mark is encoded.
    let utf16BigEPU =
        encodingPU (Encoding.UTF16 {Endianness = BigEndian; ByteOrderMark = false})

    /// A pickler/unpickler pair for UTF-32 strings in little endian byte order.  No byte order mark is encoded.
    let utf32LittleEPU =
        encodingPU (Encoding.UTF32 {Endianness = LittleEndian; ByteOrderMark = false})

    /// A pickler/unpickler pair for UTF-32 strings in big endian byte order.  No byte order mark is encoded.
    let utf32BigEPU =
        encodingPU (Encoding.UTF32 {Endianness = BigEndian; ByteOrderMark = false})

    /// A pickler/unpickler pair for unicode strings which grabs a byte order mark to indicate endianness when unpickling.
    let private pickleUTFXWithEndiannessDetect defaultEnc matchingEndianPickler nonMatchingEndianPickler =
        let preamble = Encoding.preamble defaultEnc
        sequ (fun _ -> preamble) (repeatA bytePU (Array.length preamble)) (fun bytes ->
            match Array.forall2 (=) bytes (preamble) with
            |true -> matchingEndianPickler
            |false -> nonMatchingEndianPickler)

    /// A pickler/unpickler pair for UTF-8 strings with byte order mark.  This pickler is not sensitive to endianness but the byte order mark does serve as an identifier that the
    /// subsequent data is in UTF-8.
    let utf8BomPU = pickleUTFXWithEndiannessDetect (Encoding.UTF8 {EmitIdentifier = true}) utf8PU utf8PU
        
    /// A pickler/unpickler pair for unicode strings which uses a byte order mark to indicate endianness when unpickling.  During pickling, little endian is used and a byte order
    /// mark to indicate this is prepended.
    let utf16PU = pickleUTFXWithEndiannessDetect (Encoding.UTF16 {Endianness = LittleEndian; ByteOrderMark = true}) utf16LittleEPU utf16BigEPU

    /// A pickler/unpickler pair for UTF-32 strings which uses a byte order mark to indicate endianness when unpickling.  During pickling, little endian is used and a byte order
    /// mark to indicate this is prepended.
    let utf32PU = pickleUTFXWithEndiannessDetect (Encoding.UTF32 {Endianness = LittleEndian; ByteOrderMark = true}) utf32LittleEPU utf32BigEPU

    /// Uses the supplied pickler/unpickler pair to unpickle the supplied byte array into some type 'a 
    let unpickle pu array =
        fst <| runUnpickle (UnpickleComplete {Raw = array; Position = 0; Endianness = ByteOrder.systemEndianness}) pu

    /// Uses the supplied pickler/unpickler pair to pickle the supplied value into a byte array
    let pickle pu value =
        let st = PickleComplete {Raw = []; Endianness = ByteOrder.systemEndianness}
        match (runPickle (value, st) pu) with 
        |PickleComplete ps -> ps.Raw |> Seq.rev |> Array.ofSeq
        |_ -> invalidOp "A non-complete binary pickler state was returned from an initially complete pickler"

    /// Uses the supplied pickler/unpickler pair to unpickle from the supplied binary handle incrementally
    let unpickleIncr pu binaryHandle =
        match binaryHandle.BinaryReader with
        |Some binReader -> 
            let incrUnpickler = UnpickleIncremental {Reader = binReader}
            IO.fromEffectful (fun _ -> fst <| runUnpickle (incrUnpickler) pu)
        |None -> raise HandleDoesNotSupportReadingException

    /// Uses the supplied pickler/unpickler pair to pickle the supplied data to the supplied binary handle incrementally
    let pickleIncr pu binaryHandle value =
        match binaryHandle.BinaryWriter with
        |Some binWriter -> 
            let incrPickler = PickleIncremental {Writer = binWriter}
            IO.fromEffectful (fun _ -> 
                match (runPickle (value, incrPickler) pu) with 
                |PickleIncremental ps -> binWriter.Flush()
                |_ -> invalidOp "A non-incremental binary pickler state was returned from an initially incremental pickler")
        |None -> raise HandleDoesNotSupportReadingException
        

