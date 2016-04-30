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

namespace NovelFS.NovelIO.BinaryParser

/// The attempted binary pickling exceeded the length of the supplied array
exception PicklingExceededArrayLengthException of int * int

type BinaryUnpicklerState = {Raw : byte array; Position : int}
type BinaryPicklerState = {Raw : byte list}

type BinaryPU<'a> = {Pickle : 'a * BinaryPicklerState -> BinaryPicklerState; Unpickle : BinaryUnpicklerState -> 'a * BinaryUnpicklerState}

/// Conversions functions
module private PickleConvertors =
    /// perform some type conversion with exception checks on the array bounds
    let private checkConversionException f pos array =
        try
            f array
        with
            | :? System.ArgumentOutOfRangeException as aoex -> raise <| PicklingExceededArrayLengthException(pos, Array.length array)
            | :? System.ArgumentException as aex -> raise <| PicklingExceededArrayLengthException(pos, Array.length array)

    /// Convert a chunk of a byte array into an int16 with exception checking
    let convInt16 pos array = checkConversionException (fun arr -> System.BitConverter.ToInt16(arr, pos)) pos array
    /// Convert a chunk of a byte array into an int32 with exception checking
    let convInt32 pos array = checkConversionException (fun arr -> System.BitConverter.ToInt32(arr, pos)) pos array
    /// Convert a chunk of a byte array into an int64 with exception checking
    let convInt64 pos array = checkConversionException (fun arr -> System.BitConverter.ToInt64(arr, pos)) pos array
    /// Convert a chunk of a byte array into an float32 with exception checking
    let convFloat32 pos array = checkConversionException (fun arr -> System.BitConverter.ToSingle(arr, pos)) pos array
    /// Convert a chunk of a byte array into an float64 with exception checking
    let convFloat64 pos array = checkConversionException (fun arr -> System.BitConverter.ToDouble(arr, pos)) pos array
    
    /// Flips an array to produce a list in the opposite order
    let arrayFlipToList a =  Array.fold(fun lst it -> it :: lst) [] a

    /// Converts an int16 to a byte list in reverse order
    let convFromInt16 (i16 : int16) = 
        System.BitConverter.GetBytes(i16) |> arrayFlipToList
    /// Converts an int32 to a byte list in reverse order
    let convFromInt32 (i32 : int32) = 
        System.BitConverter.GetBytes(i32) |> arrayFlipToList
    /// Converts an int64 to a byte list in reverse order
    let convFromInt64 (i64 : int64) = 
        System.BitConverter.GetBytes(i64) |> arrayFlipToList
    /// Converts an float32 to a byte list in reverse order
    let convFromFloat32 (f32 : float32) = 
        System.BitConverter.GetBytes(f32) |> arrayFlipToList
    /// Converts an float64 to a byte list in reverse order  
    let convFromFloat64 (f64 : float) = 
        System.BitConverter.GetBytes(f64) |> arrayFlipToList   

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
                runPickle (a, runPickle (b, s) pb) pa
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

    /// Pickles a byte
    let pickleByte =
        {
        Pickle = fun (b, s) -> {Raw = b :: s.Raw}
        Unpickle = fun st ->
            let pos = st.Position
            let result = Array.item (pos) st.Raw
            result, {st with Position = pos + sizeof<byte>}
        }

    /// Pickles an int16
    let pickleInt16 =
        {
        Pickle = fun (i16, s) -> {Raw = (PickleConvertors.convFromInt16 i16) @ s.Raw}
        Unpickle = fun st ->
            let pos = st.Position
            let result = PickleConvertors.convInt16 pos (st.Raw)
            result, {st with Position = pos + sizeof<int16>}
        }

    /// Pickles an int32
    let pickleInt32 =
        {
        Pickle = fun (i32, s) -> {Raw = (PickleConvertors.convFromInt32 i32) @ s.Raw}
        Unpickle = fun st ->
            let pos = st.Position
            let result = PickleConvertors.convInt32 pos (st.Raw)
            result, {st with Position = pos + sizeof<int32>}
        }

    /// Pickles an int64
    let pickleInt64 =
        {
        Pickle = fun (i64, s) -> {Raw = (PickleConvertors.convFromInt64 i64) @ s.Raw}
        Unpickle = fun st ->
            let pos = st.Position
            let result = PickleConvertors.convInt64 pos (st.Raw)
            result, {st with Position = pos + sizeof<int64>}
        }

    /// Pickles a float32
    let pickleFloat32 =
        {
        Pickle = fun (f32, s) -> {Raw = (PickleConvertors.convFromFloat32 f32) @ s.Raw}
        Unpickle = fun st ->
            let pos = st.Position
            let result = PickleConvertors.convFloat32 pos (st.Raw)
            result, {st with Position = pos + sizeof<float32>}
        }

    /// Pickles a float
    let pickleFloat =
        {
        Pickle = fun (f64, s) -> {Raw = (PickleConvertors.convFromFloat64 f64) @ s.Raw}
        Unpickle = fun st ->
            let pos = st.Position
            let result = PickleConvertors.convFloat64 pos (st.Raw)
            result, {st with Position = pos + sizeof<float>}
        }

    let alt tag ps = sequ tag pickleInt32 (fun i -> List.item i ps)

    /// Pickles a general list by prefixing with the length of the list
    let list pa = sequ (List.length) pickleInt32 << repeat <| pa

    /// Pickles a general array by prefixing with the length of the array
    let array pa = sequ (Array.length) pickleInt32 << repeatA <| pa

    /// Pickles an option type
    let pickleOption pa = 
        let tag = function
            |Some _ -> 1
            |None -> 0
        alt tag [lift None; wrap (Some, Option.get) pa]

    /// Pickles an ASCII string
    let pickleAscii =
        wrap (System.Text.Encoding.ASCII.GetString, System.Text.Encoding.ASCII.GetBytes) (array pickleByte)

    /// Pickles a UTF-7 string
    let pickleUTF7 =
        wrap (System.Text.Encoding.UTF7.GetString, System.Text.Encoding.UTF7.GetBytes) (array pickleByte)

    /// Pickles a UTF-8 string
    let pickleUTF8 =
        wrap (System.Text.Encoding.UTF8.GetString, System.Text.Encoding.UTF8.GetBytes) (array pickleByte)

    /// Pickles a UTF-32 string
    let pickleUTF32 =
        wrap (System.Text.Encoding.UTF32.GetString, System.Text.Encoding.UTF32.GetBytes) (array pickleByte)

    /// Pickles a decimal
    let pickleDecimal =
        let intAToDecimal (a : int[]) = System.Decimal a
        wrap (intAToDecimal, System.Decimal.GetBits) (repeatA pickleInt32 4)
