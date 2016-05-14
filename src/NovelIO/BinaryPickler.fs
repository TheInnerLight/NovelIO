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

namespace NovelFS.NovelIO.BinaryPickler

open NovelFS.NovelIO

/// The attempted binary pickling exceeded the length of the supplied array
exception PicklingExceededArrayLengthException of int * int

type private BinaryUnpicklerState = {Raw : byte array; Position : int; Endianness : Endianness}
type private BinaryPicklerState = {Raw : byte list; Endianness : Endianness}

/// A pickler/unpickler pair for type 'a
type BinaryPU<'a> = private {Pickle : 'a * BinaryPicklerState -> BinaryPicklerState; Unpickle : BinaryUnpicklerState -> 'a * BinaryUnpicklerState}

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
    let convToInt16 pos endianness array = 
        let unchecked pos arr = System.BitConverter.ToInt16(arr, pos)
        checkConversionException (unchecked pos << flipForEndianness endianness) pos array
    /// Convert a chunk of a byte array into an int32 with exception checking
    let convToInt32 pos endianness array = 
        let unchecked pos arr = System.BitConverter.ToInt32(arr, pos)
        checkConversionException (unchecked pos << flipForEndianness endianness) pos array
    /// Convert a chunk of a byte array into an int64 with exception checking
    let convToInt64 pos endianness array = 
        let unchecked pos arr = System.BitConverter.ToInt64(arr, pos)
        checkConversionException (unchecked pos << flipForEndianness endianness) pos array
    /// Convert a chunk of a byte array into an float32 with exception checking
    let convToFloat32 pos endianness array = 
        let unchecked pos arr = System.BitConverter.ToSingle(arr, pos)
        checkConversionException (unchecked pos << flipForEndianness endianness) pos array
    /// Convert a chunk of a byte array into an float64 with exception checking
    let convToFloat64 pos endianness array = 
        let unchecked pos arr = System.BitConverter.ToDouble(arr, pos)
        checkConversionException (unchecked pos << flipForEndianness endianness) pos array
    
    /// Flips an array to produce a list in the opposite order
    let arrayFlipToList a =  Array.fold(fun lst it -> it :: lst) [] a

    /// Converts a bool to a byte list in reverse order
    let convFromBool (b : bool) = 
        System.BitConverter.GetBytes(b) |> arrayFlipToList
    /// Converts an int16 to a byte list in reverse order
    let convFromInt16 endianness (i16 : int16) = 
        arrayFlipToList << flipForEndianness endianness <| System.BitConverter.GetBytes i16
    /// Converts an int32 to a byte list in reverse order
    let convFromInt32 endianness (i32 : int32) = 
        arrayFlipToList << flipForEndianness endianness <| System.BitConverter.GetBytes i32
    /// Converts an int64 to a byte list in reverse order
    let convFromInt64 endianness (i64 : int64) = 
        arrayFlipToList << flipForEndianness endianness <| System.BitConverter.GetBytes i64
    /// Converts an float32 to a byte list in reverse order
    let convFromFloat32 endianness (f32 : float32) = 
        arrayFlipToList << flipForEndianness endianness <| System.BitConverter.GetBytes f32
    /// Converts an float64 to a byte list in reverse order  
    let convFromFloat64 endianness (f64 : float) = 
        arrayFlipToList << flipForEndianness endianness <| System.BitConverter.GetBytes f64  

/// Provides functions for pickling binary data
module BinaryPickler =
    let private runUnpickle state x =
        match x with
        |{Unpickle = g; Pickle = _} -> g state

    let private runPickle (a, st) x =
        match x with
        |{Unpickle = _; Pickle = g} -> g (a, st)

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
    let pickleBool =
        {
        Pickle = fun (b, s) -> {s with Raw = (PickleConvertors.convFromBool b) @ s.Raw}
        Unpickle = fun st ->
            let pos = st.Position
            let result = PickleConvertors.convToBool pos (st.Raw)
            result, {st with Position = pos + sizeof<bool>}
        }

    /// A pickler/unpickler pair for bytes
    let pickleByte =
        {
        Pickle = fun (b, s) -> {s with Raw = b :: s.Raw}
        Unpickle = fun st ->
            let pos = st.Position
            let result = Array.item (pos) st.Raw
            result, {st with Position = pos + sizeof<byte>}
        }

    /// A pickler/unpickler pair for int16s
    let pickleInt16 =
        {
        Pickle = fun (i16, s) -> {s with Raw = (PickleConvertors.convFromInt16 (s.Endianness) i16) @ s.Raw}
        Unpickle = fun st ->
            let pos = st.Position
            let result = PickleConvertors.convToInt16 pos (st.Endianness) (st.Raw)
            result, {st with Position = pos + sizeof<int16>}
        }

    /// A pickler/unpickler pair for int32s
    let pickleInt32 =
        {
        Pickle = fun (i32, s) -> {s with Raw = (PickleConvertors.convFromInt32 (s.Endianness) i32) @ s.Raw}
        Unpickle = fun st ->
            let pos = st.Position
            let result = PickleConvertors.convToInt32 pos (st.Endianness) (st.Raw)
            result, {st with Position = pos + sizeof<int32>}
        }

    /// A pickler/unpickler pair for int64s
    let pickleInt64 =
        {
        Pickle = fun (i64, s) -> {s with Raw = (PickleConvertors.convFromInt64 (s.Endianness) i64) @ s.Raw}
        Unpickle = fun st ->
            let pos = st.Position
            let result = PickleConvertors.convToInt64 pos (st.Endianness) (st.Raw)
            result, {st with Position = pos + sizeof<int64>}
        }

    /// A pickler/unpickler pair for float32s
    let pickleFloat32 =
        {
        Pickle = fun (f32, s) -> {s with Raw = (PickleConvertors.convFromFloat32 (s.Endianness) f32) @ s.Raw}
        Unpickle = fun st ->
            let pos = st.Position
            let result = PickleConvertors.convToFloat32 pos (st.Endianness) (st.Raw)
            result, {st with Position = pos + sizeof<float32>}
        }

    /// A pickler/unpickler pair for floats
    let pickleFloat =
        {
        Pickle = fun (f64, s) -> {s with Raw = (PickleConvertors.convFromFloat64 (s.Endianness) f64) @ s.Raw}
        Unpickle = fun st ->
            let pos = st.Position
            let result = PickleConvertors.convToFloat64 pos (st.Endianness) (st.Raw)
            result, {st with Position = pos + sizeof<float>}
        }

    /// Accepts a tagging function that partitions the type to be pickled into two sets, then accepts a pickler for each set
    let alt tag ps = sequ tag pickleInt32 (fun i -> Map.find i ps)

    /// A pickler/unpickler pair for lists
    let list pa = sequ (List.length) pickleInt32 << repeat <| pa

    /// A pickler/unpickler pair for arrays
    let array pa = sequ (Array.length) pickleInt32 << repeatA <| pa

    /// A pickler/unpickler pair for option types
    let pickleOption pa = 
        let tag = function
            |Some _ -> 1
            |None -> 0
        let map = Map.ofList [(0, lift None); (1, wrap (Some, Option.get) pa)]
        alt tag map

    /// A pickler/unpickler pair for ASCII strings
    let pickleAscii =
        wrap (System.Text.Encoding.ASCII.GetString, System.Text.Encoding.ASCII.GetBytes) (array pickleByte)

    /// A pickler/unpickler pair for UTF-7 strings
    let pickleUTF7 =
        wrap (System.Text.Encoding.UTF7.GetString, System.Text.Encoding.UTF7.GetBytes) (array pickleByte)

    /// A pickler/unpickler pair for UTF-8 strings
    let pickleUTF8 =
        wrap (System.Text.Encoding.UTF8.GetString, System.Text.Encoding.UTF8.GetBytes) (array pickleByte)

    /// A pickler/unpickler pair for UTF-32 strings
    let pickleUTF32 =
        wrap (System.Text.Encoding.UTF32.GetString, System.Text.Encoding.UTF32.GetBytes) (array pickleByte)

    /// A pickler/unpickler pair for decimals
    let pickleDecimal =
        let intAToDecimal (a : int[]) = System.Decimal a
        wrap (intAToDecimal, System.Decimal.GetBits) (repeatA pickleInt32 4)

    /// Uses the supplied pickler to unpickle the supplied byte array into some type 'a 
    let unpickle pickler array =
        fst <| runUnpickle {Raw = array; Position = 0; Endianness = SystemEndian} pickler

    /// Uses the supplied pickler to pickle the supplied value into a byte array
    let pickle pickler value =
        (runPickle (value, {Raw = []; Endianness = SystemEndian}) pickler).Raw 
        |> Seq.rev
        |> Array.ofSeq
