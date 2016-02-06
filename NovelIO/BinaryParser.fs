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

type BinaryParser<'a> =
    private
    |Return of 'a
    |ReadByte of (byte -> BinaryParser<'a>)
    |ReadInt16 of (int16 -> BinaryParser<'a>)
    |ReadInt32 of (int -> BinaryParser<'a>)
    |ReadInt64 of (int64 -> BinaryParser<'a>)
    |ReadFloat32 of (float32 -> BinaryParser<'a>)
    |ReadFloat64 of (float -> BinaryParser<'a>)

exception ParseExceededArrayLengthException

type BinaryParseResult<'a> =
    |ParseSuccess of 'a
    |ParseFailure of ParseError
and ParseError =
    |ArrayLengthInsufficient

module BinaryParser =
    /// return for Binary Parsers
    let return' x = Return x
    /// monadic bind for Binary Parsers
    let rec bind x f =
        match x with
        |Return a -> f a
        |ReadByte (g) -> ReadByte (fun a -> bind (g a) f)
        |ReadInt16 (g) -> ReadInt16 (fun a -> bind (g a) f)
        |ReadInt32 (g) -> ReadInt32 (fun a -> bind (g a) f)
        |ReadInt64 (g) -> ReadInt64 (fun a -> bind (g a) f)
        |ReadFloat32 (g) -> ReadFloat32 (fun a -> bind (g a) f)
        |ReadFloat64 (g) -> ReadFloat64 (fun a -> bind (g a) f)
    /// Monadic bind operator for Binary Parsers
    let (>>=) x f = bind x f
    /// Map function for Binary Parsers
    let map f x = x >>= (return' << f)
    /// Map operator for Binary Parsers
    let (<!>) f x = map f x
    /// Map each element of a list to a monadic action, evaluate these actions from left to right and collect the results.
    let mapM mFunc list =
        let folder head tail = 
            mFunc head >>= (fun h -> 
                tail >>= (fun t ->
                    return' (h::t) ))
        List.foldBack (folder) list (return' [])
    /// As mapM but ignores the result.
    let mapM_ mFunc list =
        mapM mFunc list >>= (fun _ -> return' ())
    /// Evaluate each action in the list from left to right, and and collect the results.
    let sequence list =
        mapM (id) list
    /// Performs the action mFunc n times, gathering the results.
    let replicateM mFunc n =
        sequence (List.init n (fun _ -> mFunc))
    /// As replicateM but ignores the results
    let replicateM_ mFunc n  =
        replicateM mFunc n >>= (fun _ -> return' ())
    /// Applicative for Binary Parsers
    let apply f x =
        f >>= (fun fe -> map fe x)
    /// Apply operator for Binary Parsers 
    let (<*>) f x = apply f x
    /// Combines two Binary Parsers using a specified function
    let lift2 f x1 x2 =
        f <!> x1 <*> x2
    /// Combines three Binary Parsers using a specified function
    let lift3 f x1 x2 x3 =
        f <!> x1 <*> x2 <*> x3
    /// Combines four Binary Parsers using a specified function
    let lift4 f x1 x2 x3 x4 =
        f <!> x1 <*> x2 <*> x3 <*> x4
    /// Zip two Binary Parsers to produce a 2-tuple parser
    let zip2 x1 x2 =
        lift2 (fun a b -> a, b) x1 x2
    /// Zip three Binary Parsers to produce a 3-tuple parser
    let zip3 x1 x2 x3 =
        lift3 (fun a b c -> a, b, c) x1 x2 x3
    /// Zip four Binary Parsers to produce a 4-tuple parser
    let zip4 x1 x2 x3 x4 =
        lift4 (fun a b c d -> a, b, c, d) x1 x2 x3 x4

    let private checkConversionException f array =
        try
            f array
        with
            | :? System.ArgumentOutOfRangeException as aoex -> raise <| ParseExceededArrayLengthException
            | :? System.ArgumentException as aex -> raise <| ParseExceededArrayLengthException

    let private convInt16 pos array = checkConversionException (fun arr -> System.BitConverter.ToInt16(arr, pos)) array
    let private convInt32 pos array = checkConversionException (fun arr -> System.BitConverter.ToInt32(arr, pos)) array
    let private convInt64 pos array = checkConversionException (fun arr -> System.BitConverter.ToInt64(arr, pos)) array
    let private convFloat32 pos array = checkConversionException (fun arr -> System.BitConverter.ToSingle(arr, pos)) array
    let private convFloat64 pos array = checkConversionException (fun arr -> System.BitConverter.ToDouble(arr, pos)) array

    /// Run binary parser
    let run array x =
        let rec runRec (array : byte[]) pos x =
            match x with
            |Return b -> b
            |ReadByte (g) -> runRec array (pos+sizeof<byte>) (g array.[pos]) 
            |ReadInt16 (g) -> runRec array (pos+sizeof<int16>) (g <| convInt16 pos array)
            |ReadInt32 (g) -> runRec array (pos+sizeof<int32>) (g <| convInt32 pos array)
            |ReadInt64 (g) -> runRec array (pos+sizeof<int64>) (g <| convInt64 pos array)
            |ReadFloat32 (g) -> runRec array (pos+sizeof<float32>) (g <| convFloat32 pos array)
            |ReadFloat64 (g) -> runRec array (pos+sizeof<float>) (g <| convFloat64 pos array)
        try
            ParseSuccess <| runRec array 0 x
        with 
            | ParseExceededArrayLengthException -> ParseFailure <| ArrayLengthInsufficient
            

    // Binary Parsers

    /// Binary Parser for a byte
    let parseByte = ReadByte (return')
    /// Binary Parser for an int 16
    let parseInt16 = ReadInt16 (return')
    /// Binary Parser for an int 32
    let parseInt32 = ReadInt32 (return')
    /// Binary Parser for an int 64
    let parseInt64 = ReadInt64 (return')
    /// Binary Parser for a float 64
    let parseFloat64 = ReadFloat64 (return')
    /// Binary Parser for a float 32
    let parseFloat32 = ReadFloat32 (return')

open BinaryParser

type BinaryParserBuilder() =
    member this.Return x = return' x
    member this.ReturnFrom x = x
    member this.Bind (x,f) = x >>= f
        