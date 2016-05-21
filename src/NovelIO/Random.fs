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

namespace NovelFS.NovelIO

open System

open NovelFS.NovelIO.BinaryPickler

/// Purely functional random number functions
module Random =
    /// Generate a random seed for each thread local random using the crypto-random provider
    let private lclRnds = new Threading.ThreadLocal<_>(fun _ -> 
        let cryptoResult = Array.zeroCreate<byte> 4
        use rngCrypto = new System.Security.Cryptography.RNGCryptoServiceProvider()
        rngCrypto.GetBytes cryptoResult
        let seed = BinaryPickler.unpickle BinaryPickler.intPU cryptoResult
        Random(seed))

    /// An IO action that returns the next int from the global random number generator
    let nextIO = IO.fromEffectful (fun _ -> lclRnds.Value.Next())

    /// An IO action that returns the next int in the specified range from the global random number generator
    let nextRangeIO (min, max) = IO.fromEffectful (fun _ -> lclRnds.Value.Next(min, max))

    /// An IO action that returns the next float from the global random number generator
    let nextFloatIO = IO.fromEffectful (fun _ -> lclRnds.Value.NextDouble())

    /// An IO action that returns the next byte array of the supplied length from the global random number generator
    let nextBytesIO length = IO.fromEffectful (fun _ ->
        let bytes = Array.zeroCreate length
        lclRnds.Value.NextBytes(bytes)
        bytes)


            


