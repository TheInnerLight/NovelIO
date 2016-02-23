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

/// A binary parse result representing success or failure of the parsing operation
type BinaryParseResult<'a> =
    /// Binary parsing was successful with a result of type 'a
    |ParseSuccess of 'a
    /// Binary parsing failed with an error
    |ParseFailure of ParseError
/// Represents failure modes of the binary parser
and ParseError =
    /// The length of the array was insufficient for the requested parse operations
    |ArrayLengthInsufficient of int * int

/// A binary parser that parses some result 'a
type BinaryParser<'s,'a> =
    private
    |RunState of ('s -> 'a * 's)

/// The attempted binary parsing exceeded the length of the supplied array
exception ParseExceededArrayLengthException of int * int

/// Represents the current state of a binary parser of an in memory array
type BinaryParserState = {Raw : byte array; Position : int}

/// Binary Parsing functions
module BinaryParser =
    let private runState (s : 'state) x =
        match x with
        |RunState g -> g s
    /// Monadic return for Binary Parsers
    let return' x : BinaryParser<'state,'a> = RunState (fun s -> x, s)
    /// Monadic bind for Binary Parsers
    let bind (x : BinaryParser<'state,'a>) (f : 'a -> BinaryParser<'state,'b>) = 
        match x with
        |RunState (g) -> RunState (fun s -> 
            let a, s' = runState s x
            runState s' (f a) )
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

    let private checkConversionException f pos array =
        try
            f array
        with
            | :? System.ArgumentOutOfRangeException as aoex -> raise <| ParseExceededArrayLengthException(pos, Array.length array)
            | :? System.ArgumentException as aex -> raise <| ParseExceededArrayLengthException(pos, Array.length array)

    let private convInt16 pos array = checkConversionException (fun arr -> System.BitConverter.ToInt16(arr, pos)) pos array
    let private convInt32 pos array = checkConversionException (fun arr -> System.BitConverter.ToInt32(arr, pos)) pos array
    let private convInt64 pos array = checkConversionException (fun arr -> System.BitConverter.ToInt64(arr, pos)) pos array
    let private convFloat32 pos array = checkConversionException (fun arr -> System.BitConverter.ToSingle(arr, pos)) pos array
    let private convFloat64 pos array = checkConversionException (fun arr -> System.BitConverter.ToDouble(arr, pos)) pos array

    /// Run binary parser
    let run array x =
        try
            let initialState = {Raw = array; Position = 0}
            ParseSuccess << fst <| runState initialState x
        with 
            | ParseExceededArrayLengthException (pos, arrayLen) -> ParseFailure <| ArrayLengthInsufficient(pos, arrayLen)
            

    // Binary Parsers

    /// Binary Parser for a byte
    let parseByte = RunState (fun state ->
        let pos = state.Position
        let result = Array.item (pos) state.Raw
        result, {state with Position = pos + sizeof<byte>})
    /// Binary Parser for an int 16
    let parseInt16 = RunState (fun state ->
        let pos = state.Position
        let result = convInt16 pos (state.Raw)
        result, {state with Position = pos + sizeof<int16>})
    /// Binary Parser for an int 32
    let parseInt32 = RunState (fun state ->
        let pos = state.Position
        let result = convInt32 pos (state.Raw)
        result, {state with Position = pos + sizeof<int32>})
    /// Binary Parser for an int 64
    let parseInt64 = RunState (fun state ->
        let pos = state.Position
        let result = convInt64 pos (state.Raw)
        result, {state with Position = pos + sizeof<int64>})
    /// Binary Parser for a float 64
    let parseFloat64 = RunState (fun state ->
        let pos = state.Position
        let result = convFloat64 pos (state.Raw)
        result, {state with Position = pos + sizeof<float>})
    /// Binary Parser for a float 32
    let parseFloat32 = RunState (fun state ->
        let pos = state.Position
        let result = convFloat32 pos (state.Raw)
        result, {state with Position = pos + sizeof<float32>})

open BinaryParser

/// A builder for binary parsing computation expressions
type BinaryParserBuilder() =
    /// Monadic return for binary parsers
    member this.Return x : BinaryParser<'state,'a> = BinaryParser.return' x
    /// Bare return for binary parsers
    member this.ReturnFrom x : BinaryParser<'state,'a> = x
    /// Monadic bind for binary parsers
    member this.Bind (x : BinaryParser<'state,'a> , f : 'a -> BinaryParser<'state,'b>) = x >>= f
        