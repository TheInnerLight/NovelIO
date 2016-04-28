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

type BinaryPU<'a> = {Pickle : unit -> 'a * BinaryPicklerState; Unpickle : BinaryUnpicklerState -> 'a * BinaryUnpicklerState}

module private PickleConvertors =
    let private checkConversionException f pos array =
        try
            f array
        with
            | :? System.ArgumentOutOfRangeException as aoex -> raise <| PicklingExceededArrayLengthException(pos, Array.length array)
            | :? System.ArgumentException as aex -> raise <| PicklingExceededArrayLengthException(pos, Array.length array)

    let convInt16 pos array = checkConversionException (fun arr -> System.BitConverter.ToInt16(arr, pos)) pos array
    let convInt32 pos array = checkConversionException (fun arr -> System.BitConverter.ToInt32(arr, pos)) pos array
    let convInt64 pos array = checkConversionException (fun arr -> System.BitConverter.ToInt64(arr, pos)) pos array
    let convFloat32 pos array = checkConversionException (fun arr -> System.BitConverter.ToSingle(arr, pos)) pos array
    let convFloat64 pos array = checkConversionException (fun arr -> System.BitConverter.ToDouble(arr, pos)) pos array

    let arrayFlipToList a =  a |> Array.fold(fun lst it -> it :: lst) []

    let convFromInt16 (i16 : int16) = 
        System.BitConverter.GetBytes(i16) |> arrayFlipToList
    let convFromInt32 (i32 : int32) = 
        System.BitConverter.GetBytes(i32) |> arrayFlipToList
    let convFromInt64 (i64 : int64) = 
        System.BitConverter.GetBytes(i64) |> arrayFlipToList
    let convFromFloat32 (f32 : float32) = 
        System.BitConverter.GetBytes(f32) |> arrayFlipToList   
    let convFromFloat64 (f64 : float) = 
        System.BitConverter.GetBytes(f64) |> arrayFlipToList   

module BinaryPickler =
    let private runUnpickle state x =
        match x with
        |{Unpickle = g; Pickle = _} -> g state

    let private runPickle  x =
        match x with
        |{Unpickle = _; Pickle = g} -> g  ()

    let return' x = {Pickle = (fun _ -> x, {Raw = []}); Unpickle = (fun s -> x, s)}

    let bind (x : BinaryPU<'a>) (f : 'a -> BinaryPU<'b>) = 
        match x with
        |{Unpickle = u; Pickle = p} ->
            let unpickler2 = 
                fun s ->
                    let (a, s') = runUnpickle s x 
                    runUnpickle s' (f a)
            let pickler2 =
                fun _ ->
                    let (a', s') = runPickle x
                    runPickle (f a')
            {Pickle = pickler2; Unpickle = unpickler2}

    let pickleByte b =
        {
        Pickle = fun _ -> b, {Raw = [b]};
        Unpickle = fun st ->
            let pos = st.Position
            let result = Array.item (pos) st.Raw
            result, {st with Position = pos + sizeof<byte>}
        }

    let pickleInt16 (i16 : int16) =
        {
        Pickle = fun _ -> i16, {Raw = (PickleConvertors.convFromInt16 i16)};
        Unpickle = fun st ->
            let pos = st.Position
            let result = PickleConvertors.convInt16 pos (st.Raw)
            result, {st with Position = pos + sizeof<int16>}
        }

    let pickleInt32 (i32 : int32) =
        {
        Pickle = fun _ -> i32, {Raw = (PickleConvertors.convFromInt32 i32)};
        Unpickle = fun st ->
            let pos = st.Position
            let result = PickleConvertors.convInt32 pos (st.Raw)
            result, {st with Position = pos + sizeof<int32>}
        }

    let pickleInt64 (i64 : int64) =
        {
        Pickle = fun _ -> i64, {Raw = (PickleConvertors.convFromInt64 i64)};
        Unpickle = fun st ->
            let pos = st.Position
            let result = PickleConvertors.convInt64 pos (st.Raw)
            result, {st with Position = pos + sizeof<int64>}
        }

    let pickleFloat32 (f32 : float32) =
        {
        Pickle = fun _ -> f32, {Raw = (PickleConvertors.convFromFloat32 f32)};
        Unpickle = fun st ->
            let pos = st.Position
            let result = PickleConvertors.convFloat32 pos (st.Raw)
            result, {st with Position = pos + sizeof<float32>}
        }

    let pickleFloat64 (f64 : float) =
        {
        Pickle = fun _ -> f64, {Raw = (PickleConvertors.convFromFloat64 f64)};
        Unpickle = fun st ->
            let pos = st.Position
            let result = PickleConvertors.convFloat64 pos (st.Raw)
            result, {st with Position = pos + sizeof<float>}
        }




