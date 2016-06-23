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

/// A pickler/unpickler pair for type 'a
type BinaryPU<'a> = private {Pickle : 'a * BPickleState -> BPickleState; Unpickle : BUnpickleState -> 'a * BUnpickleState}

/// Provides functions for pickling binary data
module BinaryPickler =
    let private runUnpickle state x =
        match x with
        |{Unpickle = g; Pickle = _} -> g state

    let private runPickle (a, st) x =
        match x with
        |{Unpickle = _; Pickle = g} -> g (a, st)

    /// Uses the supplied pickler/unpickler pair (PU) to unpickle the supplied byte array into some type 'a 
    let unpickle pu array =
        fst <| runUnpickle (UnpickleComplete {Raw = array; Position = 0; Endianness = ByteOrder.systemEndianness}) pu

    /// Uses the supplied pickler/unpickler pair (PU) to pickle the supplied value into a byte array
    let pickle pu value =
        let st = PickleComplete {Raw = []; Endianness = ByteOrder.systemEndianness}
        match (runPickle (value, st) pu) with 
        |PickleComplete ps -> ps.Raw |> Seq.rev |> Array.ofSeq
        |_ -> invalidOp "A non-complete binary pickler state was returned from an initially complete pickler"

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
            

    /// Given a value of x, returns a PU of x without affecting the underlying read/write states
    let lift x = {Pickle = (fun (_,st) -> st); Unpickle = (fun s -> x, s)}

    /// Creates a sequential combination of PU 
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

    /// Combines two PU into a PU that pickles a tuple-2
    let tuple2 pa pb =
        sequ fst pa (fun a ->
            sequ snd pb  (fun b -> lift (a, b)))

    /// Combines three PU into a PU that pickles a tuple-3
    let tuple3 pa pb pc =
        sequ (fun (a,_,_) -> a) pa (fun a ->
            sequ (fun (_,b,_) -> b) pb  (fun b ->
                sequ (fun (_,_,c) -> c) pc  (fun c ->
                    lift (a, b, c))))

    /// Combines four PU into a PU that pickles a tuple-4
    let tuple4 pa pb pc pd =
        sequ (fun (a,_,_,_) -> a) pa (fun a ->
            sequ (fun (_,b,_,_) -> b) pb  (fun b ->
                sequ (fun (_,_,c,_) -> c) pc  (fun c ->
                    sequ (fun (_,_,_,d) -> d) pd  (fun d ->
                        lift (a, b, c, d)))))

    /// When supplied with a method of transforming between two types reversably and a pickler of the first of those types, returns a 
    /// PU of the second type.
    let wrap (fab, fba) pa = sequ fba pa (lift << fab)

    /// Repeats a PU n times to create a list PU
    let rec repeat pa n =
        match n with
        |0 -> lift []
        |_ ->
            let pb = tuple2 pa (repeat pa (n-1))
            wrap ((fun (a, b) -> (a::b)),(fun (a::b) -> (a,b))) pb

    /// Repeats a PU until a condition is met to create a list PU
    let rec repeatUntil cond pa =
        let mapper = function 
            |[] -> Unchecked.defaultof<'b>
            |a::b -> a
        let binder = function 
            |x when cond(x) -> lift []
            |x ->  
                let pb = tuple2 (lift x) (repeatUntil cond pa)
                wrap ((fun (a, b) -> (a::b)), (fun (a::b) -> (a,b))) pb
        sequ (mapper) pa (binder)

    /// Repeats a PU until a condition is met to create a list PU
    let repeatWhile cond pa =
        repeatUntil (not << cond) pa

    /// Repeats a PU n times to create an array pickler
    let repeatA pa n = wrap (Array.ofList, List.ofArray) (repeat pa n)

    /// A pickler/unpickler pair (PU) for bools
    let boolPU =
        {
        Pickle = fun (b, s) -> pickleHelper (PickleConvertors.convFromBool) b s
        Unpickle = fun st -> unpickleHelper (PickleConvertors.convToBool) st
        }

    /// A pickler/unpickler pair (PU) for bytes
    let bytePU =
        {
        Pickle = fun (b, s) -> pickleHelper (Array.singleton) b s
        Unpickle = fun st -> unpickleHelper (Array.item) st
        }

    /// A pickler/unpickler pair (PU) for int16s of the supplied endianness
    let private int16PUE endianness =
        {
        Pickle = fun (i16, s) -> pickleHelper (PickleConvertors.convFromInt16 endianness) i16 s
        Unpickle = fun st -> unpickleHelper (PickleConvertors.convToInt16 endianness) st
        }

    /// A pickler/unpickler pair (PU) for int32s of the supplied endianness
    let private int32PUE endianness =
        {
        Pickle = fun (i32, s) -> pickleHelper (PickleConvertors.convFromInt32 endianness) i32 s
        Unpickle = fun st -> unpickleHelper (PickleConvertors.convToInt32 endianness) st
        }

    /// A pickler/unpickler pair (PU) for int64s of the supplied endianness
    let private int64PUE endianness =
        {
        Pickle = fun (i64, s) -> pickleHelper (PickleConvertors.convFromInt64 endianness) i64 s
        Unpickle = fun st -> unpickleHelper (PickleConvertors.convToInt64 endianness) st
        }

    /// A pickler/unpickler pair (PU) for float32s of the supplied endianness
    let private float32PUE endianness =
        {
        Pickle = fun (f32, s) -> pickleHelper (PickleConvertors.convFromFloat32 endianness) f32 s
        Unpickle = fun st -> unpickleHelper (PickleConvertors.convToFloat32 endianness) st
        }

    /// A pickler/unpickler pair (PU) for floats of the supplied endianness
    let private floatPUE endianness =
        {
        Pickle = fun (f64, s) -> pickleHelper (PickleConvertors.convFromFloat64 endianness) f64 s
        Unpickle = fun st -> unpickleHelper (PickleConvertors.convToFloat64 endianness) st
        }

    /// A pickler/unpickler pair (PU) for decimals of the supplied endianness
    let private decimalPUE endianness =
        let intAToDecimal (a : int[]) = System.Decimal a
        wrap (intAToDecimal, System.Decimal.GetBits) (repeatA (int32PUE endianness) 4)

    /// A pickler/unpickler pair (PU) that prefixes the byte length of the structure in the supplied endianness
    let private byteLengthPrefixE endianness pu =
        {
        Pickle = fun (v, s) -> 
            pickleHelper (fun v' -> 
                let arr = pickle pu v'
                let byteLen = PickleConvertors.convFromInt32 endianness (Array.length arr)
                Array.concat [byteLen; arr]) v s
        Unpickle = fun st -> unpickleHelper (fun _ b -> unpickle pu (Array.skip 4 b)) st
        }

    /// A pickler/unpickler pair (PU) for int16s in the Endianness of the current platform
    let int16PU = int16PUE (ByteOrder.systemEndianness)

    /// A pickler/unpickler pair (PU) for ints in the Endianness of the current platform
    let intPU = int32PUE (ByteOrder.systemEndianness)

    /// A pickler/unpickler pair (PU) for int64s in the Endianness of the current platform
    let int64PU = int64PUE (ByteOrder.systemEndianness)

    /// A pickler/unpickler pair (PU) for float32s in the Endianness of the current platform
    let float32PU = float32PUE (ByteOrder.systemEndianness)

    /// A pickler/unpickler pair (PU) for floats in the Endianness of the current platform
    let floatPU = floatPUE (ByteOrder.systemEndianness)

    /// A pickler/unpickler pair (PU) for decimals in the Endianness of the current platform
    let decimalPU = decimalPUE (ByteOrder.systemEndianness)

    /// Accepts a tagging function that partitions the type to be pickled/unpickled into two sets, then accepts a PU for each set.  This permits
    /// creating PUs that might pickle one of several alternatives
    let altE endianness tag ps = sequ tag (int32PUE endianness) (flip Map.find <| ps)

    /// A pickler/unpickler pair (PU) for lists which prefixes the length using the Endianness of the current platform
    let list pa = sequ (List.length) intPU << repeat <| pa

    /// A pickler/unpickler pair (PU) for arrays which prefixes the length using the Endianness of the current platform
    let array pa = sequ (Array.length) intPU << repeatA <| pa

    /// A pickler/unpickler pair (PU) that prefixes the byte length of the structure  using the Endianness of the current platform
    let byteLengthPrefixed pu = byteLengthPrefixE (ByteOrder.systemEndianness) pu

    /// A pickler/unpickler pair (PU) for creating length prefixed strings from a char PU.  The length is prefixed in the Endianness of the current platform
    let lengthPrefixed (pa : BinaryPU<char>) : BinaryPU<string> =
        let pArr = array pa
        pArr |> wrap ((fun chrs -> System.String(chrs)), fun str -> str.ToCharArray()) 

    /// A pickler/unpickler pair (PU) for creating null terminated strings from a char PU.
    let nullTerminated (pa : BinaryPU<char>) : BinaryPU<string> =
        let pNullTerm = repeatUntil ((=) '\000') pa
        pNullTerm |> wrap (Array.ofList >> System.String, List.ofSeq) 
        
    /// A pickler/unpickler pair (PU) for option types in the supplied endianness
    let private optionalPUE endianness pa = 
        let tag = function
            |Some _ -> 1
            |None -> 0
        let map = Map.ofList [(0, lift None); (1, wrap (Some, Option.get) pa)]
        altE endianness tag map

    /// A pickler/unpickler pair (PU) for option types in the Endianness of the current platform
    let optional pa = optionalPUE (ByteOrder.systemEndianness) pa

    /// A pickler/unpickler pair (PU) for ASCII chars
    let asciiCharPU =
        let getByte (chr : char) = System.Text.Encoding.ASCII.GetBytes([|chr|]) |> Array.exactlyOne
        let getChar (b : byte) = System.Text.Encoding.ASCII.GetChars([|b|]) |> Array.exactlyOne
        wrap (getChar, getByte) (bytePU)

    /// A pickler/unpickler pair (PU) for ASCII strings
    let asciiPU = lengthPrefixed asciiCharPU

    /// A pickler/unpickler pair (PU) for UTF-7 strings
    let utf7PU = wrap (System.Text.Encoding.UTF7.GetString, System.Text.Encoding.UTF7.GetBytes) (array bytePU)

    /// Creates a pickler/unpickler pair (PU) for strings using the supplied encoding
    let encodingPU encoding =
        let pickleEncodingS byteCount = 
            {
            Pickle = fun (str, s) -> pickleHelper (PickleConvertors.Encodings.convFromEncoding encoding) str s
            Unpickle = fun st -> unpickleHelperSized byteCount (PickleConvertors.Encodings.convToEncoding encoding byteCount) st
            }
        sequ (Encoding.byteLength encoding) intPU pickleEncodingS

    /// A pickler/unpickler pair (PU) for UTF-8 strings
    let utf8PU = encodingPU (Encoding.UTF8 {EmitIdentifier = false})

    /// A pickler/unpickler pair (PU) for unicode strings which grabs a byte order mark to indicate endianness when unpickling.
    let private pickleUTFXWithEndiannessDetect defaultEnc matchingEndianPickler nonMatchingEndianPickler =
        let preamble = Encoding.preamble defaultEnc
        sequ (fun _ -> preamble) (repeatA bytePU (Array.length preamble)) (fun bytes ->
            match Array.forall2 (=) bytes (preamble) with
            |true -> matchingEndianPickler
            |false -> nonMatchingEndianPickler)

    /// A pickler/unpickler pair (PU) for UTF-8 strings with byte order mark.  This pickler is not sensitive to endianness but the byte order mark does serve as an identifier that the
    /// subsequent data is in UTF-8.
    let utf8BomPU = pickleUTFXWithEndiannessDetect (Encoding.UTF8 {EmitIdentifier = true}) utf8PU utf8PU
        
    /// Primitive and combinator Pickler/Unpickler pairs that use Little Endian byte order
    module LittleEndian =
        /// A pickler/unpickler pair (PU) for int16s in Little Endian byte order
        let int16PU = int16PUE LittleEndian
        
        /// A pickler/unpickler pair (PU) for ints in Little Endian byte order
        let intPU = int32PUE LittleEndian

        /// A pickler/unpickler pair (PU) for int64s in Little Endian byte order
        let int64PU = int64PUE LittleEndian

        /// A pickler/unpickler pair (PU) for float32s in Little Endian byte order
        let float32PU = float32PUE LittleEndian

        /// A pickler/unpickler pair (PU) for floats in Little Endian byte order
        let floatPU = floatPUE LittleEndian

        /// A pickler/unpickler pair (PU) for decimals in Little Endian byte order
        let decimalLittleEPU = decimalPUE LittleEndian

        /// A pickler/unpickler pair (PU) for UTF-16 strings in little endian byte order.  No byte order mark is encoded.
        let utf16PU = encodingPU (Encoding.UTF16 {Endianness = LittleEndian; ByteOrderMark = false})

        /// A pickler/unpickler pair (PU) for UTF-32 strings in little endian byte order.  No byte order mark is encoded.
        let utf32PU = encodingPU (Encoding.UTF32 {Endianness = LittleEndian; ByteOrderMark = false})

        /// A pickler/unpickler pair (PU) for lists which prefixes the length in Little Endian byte order.
        let list pa = sequ (List.length) intPU << repeat <| pa

        /// A pickler/unpickler pair (PU) for arrays which prefixes the length in Little Endian byte order.
        let array pa = sequ (Array.length) intPU << repeatA <| pa

        /// A pickler/unpickler pair (PU) that prefixes the byte length of the structure in Little Endian byte order
        let byteLengthPrefixed pu = byteLengthPrefixE LittleEndian pu

        /// A pickler/unpickler pair (PU) for option types in the Endianness in Little Endian byte order
        let optional pa = optionalPUE LittleEndian pa

    /// Primitive and combinator Pickler/Unpickler pairs that use Big Endian byte order
    module BigEndian =
        /// A pickler/unpickler pair (PU) for int16s in Big Endian byte order
        let int16PU = int16PUE BigEndian
        
        /// A pickler/unpickler pair (PU) for ints in Big Endian byte order
        let intPU = int32PUE BigEndian

        /// A pickler/unpickler pair (PU) for int64s in Big Endian byte order
        let int64PU = int64PUE BigEndian

        /// A pickler/unpickler pair (PU) for floats in Big Endian byte order
        let floatPU = floatPUE BigEndian

        /// A pickler/unpickler pair (PU) for float32s in Big Endian byte order
        let float32PU = float32PUE BigEndian

        /// A pickler/unpickler pair (PU) for decimals in Big Endian byte order
        let decimalPU = decimalPUE BigEndian

        /// A pickler/unpickler pair (PU) for unicode strings in big endian byte order.  No byte order mark is encoded.
        let utf16PU = encodingPU (Encoding.UTF16 {Endianness = BigEndian; ByteOrderMark = false})

        /// A pickler/unpickler pair (PU) for UTF-32 strings in big endian byte order.  No byte order mark is encoded.
        let utf32PU = encodingPU (Encoding.UTF32 {Endianness = BigEndian; ByteOrderMark = false})

        /// A pickler/unpickler pair (PU) for lists which prefixes the length using in Big Endian byte order.
        let list pa = sequ (List.length) intPU << repeat <| pa

        /// A pickler/unpickler pair (PU) for arrays which prefixes the length using in Big Endian byte order.
        let array pa = sequ (Array.length) intPU << repeatA <| pa

        /// A pickler/unpickler pair (PU) that prefixes the byte length of the structure in Big Endian byte order
        let byteLengthPrefixed pu = byteLengthPrefixE BigEndian pu

        /// A pickler/unpickler pair (PU) for option types in the Endianness in Big Endian byte order
        let optional pa = optionalPUE BigEndian pa

    /// A pickler/unpickler pair (PU) for UTF-16 strings which uses a byte order mark to indicate endianness when unpickling.  During pickling, little endian is used and a byte order
    /// mark to indicate this is prepended.
    let utf16PU = pickleUTFXWithEndiannessDetect (Encoding.UTF16 {Endianness = LittleEndian; ByteOrderMark = true}) LittleEndian.utf16PU BigEndian.utf16PU

    /// A pickler/unpickler pair (PU) for UTF-32 strings which uses a byte order mark to indicate endianness when unpickling.  During pickling, little endian is used and a byte order
    /// mark to indicate this is prepended.
    let utf32PU = pickleUTFXWithEndiannessDetect (Encoding.UTF32 {Endianness = LittleEndian; ByteOrderMark = true}) LittleEndian.utf32PU BigEndian.utf16PU

    /// Uses the supplied pickler/unpickler pair (PU) to unpickle from the supplied binary handle incrementally
    let unpickleIncr pu binaryHandle =
        match binaryHandle.BinaryReader with
        |Some binReader -> 
            let incrUnpickler = UnpickleIncremental {Reader = binReader}
            IO.fromEffectful (fun _ -> fst <| runUnpickle (incrUnpickler) pu)
        |None -> raise HandleDoesNotSupportReadingException

    /// Uses the supplied pickler/unpickler pair (PU) to pickle the supplied data to the supplied binary handle incrementally
    let pickleIncr pu binaryHandle value =
        match binaryHandle.BinaryWriter with
        |Some binWriter -> 
            let incrPickler = PickleIncremental {Writer = binWriter}
            IO.fromEffectful (fun _ -> 
                match (runPickle (value, incrPickler) pu) with 
                |PickleIncremental ps -> binWriter.Flush()
                |_ -> invalidOp "A non-incremental binary pickler state was returned from an initially incremental pickler")
        |None -> raise HandleDoesNotSupportReadingException


    

    
