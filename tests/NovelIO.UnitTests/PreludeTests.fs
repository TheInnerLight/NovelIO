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
open NovelFS.NovelIO.BinaryPickler
open FsCheck
open FsCheck.Xunit

type ``Prelude Unit Tests``() =
    [<Property>]
    static member ``const' returns the first argument and throws away the second`` (testData : obj * obj) =
        let o1, o2 = testData
        const' o1 o2 = o1

    [<Property>]
    static member ``flip when used on (/) divides the arguments in the opposite order`` (testData : NonZeroInt * NonZeroInt) =
        let i1, i2 = testData
        flip (/) i1.Get i2.Get = i2.Get / i1.Get

    [<Property>]
    static member ``ByteOrder.systemEndianness returns the endianness of the current system`` () =
        ByteOrder.isBigEndian (ByteOrder.systemEndianness) <> System.BitConverter.IsLittleEndian

    [<Property>]
    static member ``listCons is equivalent to a::b`` (a : int, b : int list) =
        listCons a b = a :: b

    [<Property>]
    static member ``curry f(x, y) a b = f' a b `` (f : int * int -> int, a : int, b : int) =
        curry f a b = f(a, b)

    [<Property>]
    static member ``uncurry f a b = f'(a, b) `` (f : int -> int -> int, a : int, b : int) =
        uncurry f (a, b) = f a b

