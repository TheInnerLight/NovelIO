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

namespace NovelFS.NovelIO.UnitTests

open NovelFS.NovelIO
open NovelFS.NovelIO.IO.Operators
open FsCheck
open FsCheck.Xunit

type ``IO Loops Tests``() =
    [<Property>]
    static member ``takeWhileM returns same results as takeWhile for pure operation`` (lst : int list, barrier : int) =
        IO.Loops.takeWhileM (fun x -> IO.return'(x > barrier) ) lst
        |> IO.run = List.takeWhile (fun x -> x > barrier) lst

    [<Property>]
    static member ``skipWhileM returns same results as skipWhile for pure operation`` (lst : int list, barrier : int) =
        IO.Loops.skipWhileM (fun x -> IO.return'(x > barrier) ) lst
        |> IO.run = List.skipWhile (fun x -> x > barrier) lst

    [<Property>]
    static member ``whileM with mutable counter that counts up to 'count' and returns zero creates zeros of length 'count'`` (count : PositiveInt) =
        let count = count.Get
        let bCount = ref 0
        let execCount = ref 0
        let continueWhile = IO.fromEffectful (fun _ -> 
            if !bCount < count then
                incr bCount
                true
            else false)
        let zeroIO = IO.fromEffectful (fun _ -> 0)
        let res = IO.Loops.whileM continueWhile zeroIO |> IO.run
        !bCount = count && res = List.init count (const' 0)

    [<Property>]
    static member ``iterWhileM with mutable counters that count up to 'count' both equal count`` (count : PositiveInt) =
        let count = count.Get
        let bCount = ref 0
        let execCount = ref 0
        let continueUntil = IO.fromEffectful (fun _ -> 
            if !bCount < count then
                incr bCount
                true
            else false)
        let incrIO = IO.fromEffectful (fun _ -> incr execCount)
        let res = IO.Loops.iterWhileM continueUntil incrIO |> IO.run
        !bCount = count && !execCount = count

    [<Property>]
    static member ``untilM with mutable counter that counts up to 'count' and returns zero creates zeros of length 'count'`` (count : PositiveInt) =
        let count = count.Get
        let bCount = ref 0
        let execCount = ref 0
        let continueUntil = IO.fromEffectful (fun _ -> 
            if !bCount < count then
                incr bCount
                false
            else true)
        let zeroIO = IO.fromEffectful (fun _ -> 0)
        let res = IO.Loops.untilM continueUntil zeroIO |> IO.run
        !bCount = count && res = List.init count (const' 0)

    [<Property>]
    static member ``iterUntilM with mutable counters that count up to 'count' both equal count`` (count : PositiveInt) =
        let count = count.Get
        let bCount = ref 0
        let execCount = ref 0
        let continueUntil = IO.fromEffectful (fun _ -> 
            if !bCount < count then
                incr bCount
                false
            else true)
        let incrIO = IO.fromEffectful (fun _ -> incr execCount)
        let res = IO.Loops.iterUntilM continueUntil incrIO |> IO.run
        !bCount = count && !execCount = count

    [<Property>]
    static member ``whileSome with mutable counter that counts up to 'count' and returns Some i creates list of 1..count'`` (count : PositiveInt) =
        let count = count.Get
        let bCount = ref 0
        let execCount = ref 0
        let optIO = IO.fromEffectful (fun _ -> 
            if !bCount < count then
                incr bCount
                Some !bCount
            else None)
        let res = IO.Loops.whileSome optIO (IO.return') |> IO.run
        !bCount = count && res = List.init count ((+) 1)
        