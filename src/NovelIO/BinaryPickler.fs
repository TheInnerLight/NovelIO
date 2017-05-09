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

type BasicBinaryPU<'a> = private {Pickle : 'a * BPickleState -> BPickleState; Unpickle : BinaryUnpicklerState -> 'a * BinaryUnpicklerState}

/// A pickler/unpickler pair for type 'a
type BinaryPU<'a> = 
    /// A PU containing a BasicBinaryPU which is resolvable immediately
    |PU of BasicBinaryPU<'a> 
    /// A recursive PU containing a PU generating expression which is resolved when the PU is run 
    |RecursivePU of (unit -> BinaryPU<'a>)

/// Provides functions for pickling binary data
module BinaryPickler =
    let rec private runUnpickle state x =
        match x with
        |PU {Unpickle = g; Pickle = _} -> g state
        |RecursivePU y -> runUnpickle state (y())

    let rec private runPickle (a, st) x =
        match x with
        |PU {Unpickle = _; Pickle = g} -> g (a, st)
        |RecursivePU y -> runPickle (a,st) (y())

    /// Uses the supplied pickler/unpickler pair (PU) to unpickle the supplied byte array into some type 'a 
    let unpickle pu array =
        fst <| runUnpickle ({Raw = array; Position = 0; Reader = None}) pu

    /// Uses the supplied pickler/unpickler pair (PU) to pickle the supplied value into a byte array
    let pickle pu value =
        let st = PickleComplete {Raw = []; Endianness = ByteOrder.systemEndianness}
        match (runPickle (value, st) pu) with 
        |PickleComplete ps -> ps.Raw |> Seq.rev |> Array.ofSeq
        |_ -> invalidOp "A non-complete binary pickler state was returned from an initially complete pickler"

    let private writeIfExhausted bytes (st : ABinaryPicklerState) =
        match st.Position + Array.length bytes >= Array.length st.Raw, st.Writer with
        |(true, Some writer) ->
            let write, rest = Array.splitAt st.Position st.Raw
            IO.run <| BinaryChannel.writeBytes writer write
            {Raw = rest; Position = Array.length rest; Writer = Some writer}
        |(true, None) ->
            {st with Raw = Array.append st.Raw (Array.zeroCreate <| Array.length st.Raw)}
        |_ -> 
            st

    /// Helper function that chooses between complete or incremental pickling
    let private pickleHelper f b st =
        match st with
        |PickleComplete ps -> PickleComplete {ps with Raw = (PickleConvertors.arrayFlipToList << f <| b) @ ps.Raw}
        |PickleIncremental ips -> 
            BinaryChannel.SideEffecting.write (f b) ips.Writer
            st

    let private readIfExhausted size st =
        match st.Position + size >= Array.length st.Raw, st.Reader with
        |(true, Some reader) ->
            let bytes = IO.run <| BinaryChannel.read reader size
            let array' = Array.skip st.Position st.Raw
            {Raw = Array.append array' bytes; Position = 0; Reader = Some reader}
        |_ -> 
            st

    /// Helper function that chooses between complete or incremental unpickling and accepts an arbitrary data-size
    let private unpickleHelperSized size f st =
        let ps = readIfExhausted size st
        let pos = ps.Position
        let result = f pos (ps.Raw)
        result, {ps with Position = pos + size}

    /// Helper function that chooses between complete or incremental unpickling and gets the size from the size of the data type
    let private unpickleHelper (f : int -> byte array -> 'a) st =
        unpickleHelperSized (sizeof<'a>) f st

    /// Given a value of x, returns a PU of x without affecting the underlying read/write states
    let lift x = PU {Pickle = (fun (_,st) -> st); Unpickle = (fun s -> x, s)}

    /// Creates a sequential combination of PU 
    let rec sequ (f : 'b -> 'a) (pa : BinaryPU<'a>) (k : 'a -> BinaryPU<'b>) : BinaryPU<'b> =
        match pa with
        |PU{Unpickle = unPck; Pickle = pck} ->
            // unpickling is sequenced like bind in the state monad
            let unPck' s =
                let a, s' = runUnpickle s pa
                runUnpickle s' (k a)
            // pickling requires the extra projection function: f
            let pck' (b, s) =
                let a = f b
                let pb = k a
                runPickle (b, runPickle (a, s) pa) pb
            PU{Unpickle = unPck'; Pickle = pck'}
        |RecursivePU y ->
            let lazySequ = fun () -> sequ f (y()) k
            RecursivePU lazySequ

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

    /// Repeats a PU n times to create an array pickler
    let repeatA pa n = wrap (Array.ofList, List.ofArray) (repeat pa n)

    /// Repeats a PU until a condition is met to create a list PU
    let rec untilCond cond last pa =
        let mapper = function 
            |[] -> last
            |a::b -> a
        let binder = function 
            |x when cond(x) -> lift []
            |x ->  
                let pb = tuple2 (lift x) (untilCond cond last pa)
                wrap ((fun (a, b) -> (a::b)), (fun (a::b) -> (a,b))) pb
        sequ (mapper) pa (binder)


    let rec untilCondIncl cond last pa =
        let mapper = function 
            |[] -> last
            |a::b -> a
        let binder = function 
            |x when cond(x) -> lift [x]
            |x ->  
                let pb = tuple2 (lift x) (untilCondIncl cond last pa)
                wrap ((fun (a, b) -> (a::b)), (fun (a::b) -> (a,b))) pb
        sequ (mapper) pa (binder)

    /// Repeats a PU until a specific value is reached
    let until value pa = untilCond ((=) value) value pa

    /// Repeats a PU until a condition is met to create a list PU
    let rec repeatUntilIncl cond last pa =
        let mapper = function 
            |[] -> last
            |a::b -> a
        let binder = function 
            |x when cond(x) -> lift [x]
            |x ->  
                let pb = tuple2 (lift x) (repeatUntilIncl cond last pa)
                wrap ((fun (a, b) -> (a::b)), (fun (a::b) -> (a,b))) pb
        sequ (mapper) pa (binder)

    /// Repeats a PU until a condition is met to create a list PU
    let repeatWhile cond pa =
        untilCond (not << cond) pa

    /// A pickler/unpickler pair (PU) for the unit type
    let unitPU = lift ()

    /// A pickler/unpickler pair (PU) for bools
    let boolPU =
        PU
            {
            Pickle = fun (b, s) -> pickleHelper (PickleConvertors.convFromBool) b s
            Unpickle = fun st -> unpickleHelper (PickleConvertors.convToBool) st
            }

    /// A pickler/unpickler pair (PU) for bytes
    let bytePU =
        PU
            {
            Pickle = fun (b, s) -> pickleHelper (Array.singleton) b s
            Unpickle = fun st -> unpickleHelper (Array.item) st
            }

    /// A pickler/unpickler pair (PU) for int16s of the supplied endianness
    let private int16PUE endianness =
        PU
            {
            Pickle = fun (i16, s) -> pickleHelper (PickleConvertors.convFromInt16 endianness) i16 s
            Unpickle = fun st -> unpickleHelper (PickleConvertors.convToInt16 endianness) st
            }

    /// A pickler/unpickler pair (PU) for uint16s of the supplied endianness
    let private uint16PUE endianness =
        PU
            {
            Pickle = fun (i16, s) -> pickleHelper (PickleConvertors.convFromUInt16 endianness) i16 s
            Unpickle = fun st -> unpickleHelper (PickleConvertors.convToUInt16 endianness) st
            }

    /// A pickler/unpickler pair (PU) for int32s of the supplied endianness
    let private int32PUE endianness =
        PU
            {
            Pickle = fun (i32, s) -> pickleHelper (PickleConvertors.convFromInt32 endianness) i32 s
            Unpickle = fun st -> unpickleHelper (PickleConvertors.convToInt32 endianness) st
            }

    /// A pickler/unpickler pair (PU) for uint32s of the supplied endianness
    let private uint32PUE endianness =
        PU
            {
            Pickle = fun (i32, s) -> pickleHelper (PickleConvertors.convFromUInt32 endianness) i32 s
            Unpickle = fun st -> unpickleHelper (PickleConvertors.convToUInt32 endianness) st
            }

    /// A pickler/unpickler pair (PU) for int64s of the supplied endianness
    let private int64PUE endianness =
        PU
            {
            Pickle = fun (i64, s) -> pickleHelper (PickleConvertors.convFromInt64 endianness) i64 s
            Unpickle = fun st -> unpickleHelper (PickleConvertors.convToInt64 endianness) st
            }

    /// A pickler/unpickler pair (PU) for uint64s of the supplied endianness
    let private uint64PUE endianness =
        PU
            {
            Pickle = fun (i64, s) -> pickleHelper (PickleConvertors.convFromUInt64 endianness) i64 s
            Unpickle = fun st -> unpickleHelper (PickleConvertors.convToUInt64 endianness) st
            }

    /// A pickler/unpickler pair (PU) for float32s of the supplied endianness
    let private float32PUE endianness =
        PU
            {
            Pickle = fun (f32, s) -> pickleHelper (PickleConvertors.convFromFloat32 endianness) f32 s
            Unpickle = fun st -> unpickleHelper (PickleConvertors.convToFloat32 endianness) st
            }

    /// A pickler/unpickler pair (PU) for floats of the supplied endianness
    let private floatPUE endianness =
        PU
            {
            Pickle = fun (f64, s) -> pickleHelper (PickleConvertors.convFromFloat64 endianness) f64 s
            Unpickle = fun st -> unpickleHelper (PickleConvertors.convToFloat64 endianness) st
            }

    /// A pickler/unpickler pair (PU) for decimals of the supplied endianness
    let private decimalPUE endianness =
        let intAToDecimal (a : int[]) = System.Decimal a
        wrap (intAToDecimal, System.Decimal.GetBits) (repeatA (int32PUE endianness) 4)

    let private byteLengthPrefixE fPickle fConv endianness pu =
        PU
            {
            Pickle = fun (v, s) -> 
                pickleHelper (fun v' -> 
                    let arr = pickle pu v'
                    let byteLen = fPickle endianness (fConv <| Array.length arr)
                    Array.concat [byteLen; arr]) v s
            Unpickle = fun st -> unpickleHelper (fun _ b -> unpickle pu (Array.skip 4 b)) st
            }

    /// A pickler/unpickler pair (PU) that prefixes the byte length of the structure as a (signed) int in the supplied endianness
    let private intByteLengthPrefixE endianness pu = byteLengthPrefixE PickleConvertors.convFromInt32 id endianness pu

    /// A pickler/unpickler pair (PU) that prefixes the byte length of the structure as an (unsigned) uint in the supplied endianness
    let private uintByteLengthPrefixE endianness pu = byteLengthPrefixE PickleConvertors.convFromUInt32 uint32 endianness pu

    /// A pickler/unpickler pair (PU) that prefixes the byte length of the structure as a (signed) int16 in the supplied endianness
    let private int16ByteLengthPrefixE endianness pu = byteLengthPrefixE PickleConvertors.convFromInt16 int16 endianness pu

    /// A pickler/unpickler pair (PU) that prefixes the byte length of the structure as an (unsigned) uint16 in the supplied endianness
    let private uint16ByteLengthPrefixE endianness pu = byteLengthPrefixE PickleConvertors.convFromUInt16 uint16 endianness pu

    /// A pickler/unpickler pair (PU) that prefixes the byte length of the structure as am (unsigned) byte in the supplied endianness
    let private byteByteLengthPrefixE endianness pu = byteLengthPrefixE (fun _ -> Array.singleton) byte endianness pu

    /// A pickler/unpickler pair (PU) for (signed) int16s in the Endianness of the current platform
    let int16PU = int16PUE (ByteOrder.systemEndianness)

    /// A pickler/unpickler pair (PU) for (unsigned) uint16s in the Endianness of the current platform
    let uint16PU = uint16PUE (ByteOrder.systemEndianness)

    /// A pickler/unpickler pair (PU) for (signed) ints in the Endianness of the current platform
    let intPU = int32PUE (ByteOrder.systemEndianness)

    /// A pickler/unpickler pair (PU) for (unsigned) uints in the Endianness of the current platform
    let uintPU = uint32PUE (ByteOrder.systemEndianness)

    /// A pickler/unpickler pair (PU) for (signed) int64s in the Endianness of the current platform
    let int64PU = int64PUE (ByteOrder.systemEndianness)

    /// A pickler/unpickler pair (PU) for (unsigned) uint64s in the Endianness of the current platform
    let uint64PU = uint64PUE (ByteOrder.systemEndianness)

    /// A pickler/unpickler pair (PU) for float32s in the Endianness of the current platform
    let float32PU = float32PUE (ByteOrder.systemEndianness)

    /// A pickler/unpickler pair (PU) for floats in the Endianness of the current platform
    let floatPU = floatPUE (ByteOrder.systemEndianness)

    /// A pickler/unpickler pair (PU) for decimals in the Endianness of the current platform
    let decimalPU = decimalPUE (ByteOrder.systemEndianness)

    /// Accepts a tagging function that partitions the type to be pickled/unpickled into two sets, then accepts a PU for each set.  This permits
    /// creating PUs that might pickle one of several alternatives.  The tag is stored using the supplied endianness.
    let private altE endianness tag ps = sequ tag (int32PUE endianness) (flip Map.find <| ps)

    /// Accepts a tagging function that partitions the type to be pickled/unpickled into two sets, then accepts a PU for each set.  This permits
    /// creating PUs that might pickle one of several alternatives. The tag is stored in the Endianness of the current platform
    let alt tag ps = altE (ByteOrder.systemEndianness) tag ps

    /// A pickler/unpickler pair (PU) for lists which prefixes the length using the Endianness of the current platform
    let list pa = sequ (List.length) intPU << repeat <| pa

    /// A pickler/unpickler pair (PU) for arrays which prefixes the length using the Endianness of the current platform
    let array pa = sequ (Array.length) intPU << repeatA <| pa

    /// A pickler/unpickler pair (PU) that prefixes the byte length of the structure  using the Endianness of the current platform
    let byteLengthPrefixed pu = intByteLengthPrefixE (ByteOrder.systemEndianness) pu

    /// A pickler/unpickler pair (PU) for creating length prefixed strings from a char PU.  The length is prefixed in the Endianness of the current platform
    let lengthPrefixed (pa : BinaryPU<char>) : BinaryPU<string> =
        let pArr = array pa
        pArr |> wrap ((fun chrs -> System.String(chrs)), fun str -> str.ToCharArray()) 

    /// A pickler/unpickler pair (PU) for creating null terminated strings from a char PU.
    let nullTerminated (pa : BinaryPU<char>) : BinaryPU<string> =
        wrap (Array.ofList >> System.String, List.ofSeq) (until '\000' pa)

    /// A pickler/unpickler pair (PU) for creating null terminated strings from a char PU.
    let lfTerminated (pa : BinaryPU<char>) : BinaryPU<string> =
        wrap (Array.ofList >> System.String, List.ofSeq) (until '\n' pa)

    /// A pickler/unpickler pair (PU) for creating CRLF terminated strings from a char PU.
    let crlfTerminated (pa : BinaryPU<char>) : BinaryPU<string> =
        let removeLast n (str : string) =
            str.Substring(0, str.Length-n)
        let stringsLister (arr : string[]) =
            arr.[arr.Length-1] <- arr.[arr.Length-1] + "\r"
            Array.toList arr
        let lfTerminatedStrings = (lfTerminated pa) |> untilCondIncl (fun (str : string) -> str.EndsWith("\r")) "\r"
        lfTerminatedStrings 
        |> wrap ((fun strs -> (String.concat "\n" strs + "\n") |> removeLast 2), (fun str -> str.Split([|'\n'|]) |> stringsLister ))
        
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
            PU
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
        /// A pickler/unpickler pair (PU) for (signed) int16s in Little Endian byte order
        let int16PU = int16PUE LittleEndian

        /// A pickler/unpickler pair (PU) for (unsigned) uint16s in Little Endian byte order
        let uint16PU = uint16PUE LittleEndian
        
        /// A pickler/unpickler pair (PU) for (signed) ints in Little Endian byte order
        let intPU = int32PUE LittleEndian

        /// A pickler/unpickler pair (PU) for (unsigned) uint32s in Little Endian byte order
        let uintPU = uint32PUE LittleEndian

        /// A pickler/unpickler pair (PU) for (signed) int64s in Little Endian byte order
        let int64PU = int64PUE LittleEndian

        /// A pickler/unpickler pair (PU) for (unsigned) uint64s in Little Endian byte order
        let uint64PU = uint64PUE LittleEndian

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
        let byteLengthPrefixed pu = intByteLengthPrefixE LittleEndian pu

        /// Accepts a tagging function that partitions the type to be pickled/unpickled into two sets, then accepts a PU for each set.  This permits
        /// creating PUs that might pickle one of several alternatives. The tag is stored in Little Endian byte order
        let alt tag ps = altE LittleEndian tag ps

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
        let byteLengthPrefixed pu = intByteLengthPrefixE BigEndian pu

        /// Accepts a tagging function that partitions the type to be pickled/unpickled into two sets, then accepts a PU for each set.  This permits
        /// creating PUs that might pickle one of several alternatives. The tag is stored in Big Endian byte order
        let alt tag ps = altE BigEndian tag ps

        /// A pickler/unpickler pair (PU) for option types in the Endianness in Big Endian byte order
        let optional pa = optionalPUE BigEndian pa

    /// A pickler/unpickler pair (PU) for UTF-16 strings which uses a byte order mark to indicate endianness when unpickling.  During pickling, little endian is used and a byte order
    /// mark to indicate this is prepended.
    let utf16PU = pickleUTFXWithEndiannessDetect (Encoding.UTF16 {Endianness = LittleEndian; ByteOrderMark = true}) LittleEndian.utf16PU BigEndian.utf16PU

    /// A pickler/unpickler pair (PU) for UTF-32 strings which uses a byte order mark to indicate endianness when unpickling.  During pickling, little endian is used and a byte order
    /// mark to indicate this is prepended.
    let utf32PU = pickleUTFXWithEndiannessDetect (Encoding.UTF32 {Endianness = LittleEndian; ByteOrderMark = true}) LittleEndian.utf32PU BigEndian.utf32PU

    /// Uses the supplied pickler/unpickler pair (PU) to unpickle from the supplied binary channel incrementally
    let unpickleIncr pu binaryChannel =
        match binaryChannel.IOStream.CanRead with
        |true -> 
            let incrUnpickler = {Raw = [||]; Position = 0; Reader = Some binaryChannel}
            IO.fromEffectful (fun _ -> fst <| runUnpickle (incrUnpickler) pu)
        |false -> raise ChannelDoesNotSupportReadingException

    /// Uses the supplied pickler/unpickler pair (PU) to pickle the supplied data to the supplied binary channel incrementally
    let pickleIncr pu binaryChannel value =
        match binaryChannel.IOStream.CanWrite with
        |true -> 
            let incrPickler = PickleIncremental {Writer = binaryChannel}
            IO.fromEffectful (fun _ -> 
                match (runPickle (value, incrPickler) pu) with 
                |PickleIncremental ps -> binaryChannel.IOStream.Flush()
                |_ -> invalidOp "A non-incremental binary pickler state was returned from an initially incremental pickler")
        |false -> raise ChannelDoesNotSupportReadingException


    

    
