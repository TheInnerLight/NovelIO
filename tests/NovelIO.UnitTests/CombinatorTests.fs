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

type ``Binary Pickler Combinator Tests`` =
    
    [<Property>]
    static member ``Pickle values until 0 should create zero terminated list`` (lst : int list) =
        let lst = List.filter ((<>) 0) lst
        let intConditionalPickler = BinaryPickler.repeatUntil ((=) 0) BinaryPickler.intPU
        let result = BinaryPickler.pickle intConditionalPickler lst 
        let lst = List.filter ((<>) 0) lst @ [0]
        let bytes = lst |> Array.ofList |> Array.map (System.BitConverter.GetBytes) |> Array.concat
        result = bytes

    [<Property>]
    static member ``Pickle values while 0 should create zero terminated list`` (lst : int list) =
        let lst = List.filter ((<>) 0) lst
        let intConditionalPickler = BinaryPickler.repeatWhile ((<>) 0) BinaryPickler.intPU
        let result = BinaryPickler.pickle intConditionalPickler lst 
        let lst = List.filter ((<>) 0) lst @ [0]
        let bytes = lst |> Array.ofList |> Array.map (System.BitConverter.GetBytes) |> Array.concat
        result = bytes

    [<Property>]
    static member ``Unpickle tuple of ints from two ints should match the tuple of the two ints`` (i1 : int, i2: int) =
        let bytes =
            Array.concat 
                [(System.BitConverter.GetBytes i1);
                (System.BitConverter.GetBytes i2)]
        let bytePickler = 
            BinaryPickler.tuple2 (BinaryPickler.intPU) (BinaryPickler.intPU)
        let result = BinaryPickler.unpickle bytePickler bytes 
        result = (i1, i2)

    [<Property>]
    static member ``Unpickle tuple of ints from three ints should match the tuple of the three ints`` (i1 : int, i2: int, i3 : int) =
        let bytes =
            Array.concat 
                [(System.BitConverter.GetBytes i1);
                (System.BitConverter.GetBytes i2);
                (System.BitConverter.GetBytes i3)]
        let bytePickler = 
            BinaryPickler.tuple3 (BinaryPickler.intPU) (BinaryPickler.intPU) (BinaryPickler.intPU)
        let result = BinaryPickler.unpickle bytePickler bytes 
        result = (i1, i2, i3)

    [<Property>]
    static member ``Unpickle tuple of ints from four ints should match the tuple of the four ints`` (i1 : int, i2: int, i3 : int, i4: int) =
        let bytes =
            Array.concat 
                [(System.BitConverter.GetBytes i1);
                (System.BitConverter.GetBytes i2);
                (System.BitConverter.GetBytes i3);
                (System.BitConverter.GetBytes i4)]
        let bytePickler = 
            BinaryPickler.tuple4 (BinaryPickler.intPU) (BinaryPickler.intPU) (BinaryPickler.intPU) (BinaryPickler.intPU)
        let result = BinaryPickler.unpickle bytePickler bytes 
        result = (i1, i2, i3, i4)

    [<Property>]
    static member ``Unpickle list of ints from bytes should match the values of the int list`` (lst : int list) =
        let byteData =
            lst
            |> Array.ofList
            |> Array.collect (System.BitConverter.GetBytes)
        let bytes = Array.concat [System.BitConverter.GetBytes (List.length lst); byteData]
        let lstPickler = BinaryPickler.list (BinaryPickler.intPU)
        let result = BinaryPickler.unpickle lstPickler bytes 
        result = lst

    [<Property>]
    static member ``Unpickle array of ints from bytes should match the values of the int array`` (arr : int[]) =
        let byteData =
            arr
            |> Array.collect (System.BitConverter.GetBytes)
        let bytes = Array.concat [System.BitConverter.GetBytes (Array.length arr); byteData]
        let arrPickler = BinaryPickler.array (BinaryPickler.intPU)
        let result = BinaryPickler.unpickle arrPickler bytes 
        result = arr

    [<Property>]
    static member ``Unpickle int option should match 0 or 1 tagged int`` (opt : int option) =
        let bytes =
            match opt with
            |Some i -> Array.concat [System.BitConverter.GetBytes 1; System.BitConverter.GetBytes i]
            |None -> System.BitConverter.GetBytes 0
        let arrPickler = BinaryPickler.optionPU (BinaryPickler.intPU)
        let result = BinaryPickler.unpickle arrPickler bytes 
        result = opt

    [<Property>]
    static member ``Unpickle values until 0 should match zero terminated list`` (lst : int list) =
        let lst = List.filter ((<>) 0) lst @ [0]
        let bytes = lst |> Array.ofList |> Array.map (System.BitConverter.GetBytes) |> Array.concat
        let intConditionalPickler = BinaryPickler.repeatUntil ((=) 0) BinaryPickler.intPU
        let result = BinaryPickler.unpickle intConditionalPickler bytes 
        result @ [0] = lst

    [<Property>]
    static member ``Unpickle values while not 0 should match zero terminated list`` (lst : int list) =
        let lst = List.filter ((<>) 0) lst @ [0]
        let bytes = lst |> Array.ofList |> Array.map (System.BitConverter.GetBytes) |> Array.concat
        let intConditionalPickler = BinaryPickler.repeatWhile ((<>) 0) BinaryPickler.intPU
        let result = BinaryPickler.unpickle intConditionalPickler bytes 
        result @ [0] = lst

