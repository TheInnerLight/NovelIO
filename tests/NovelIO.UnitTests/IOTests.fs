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

exception BracketCloseTestException

type ``IO Unit Tests``() =
    [<Property>]
    static member ``return' of some test data returns the test data when run`` (testData : obj) =
        IO.run <| IO.return' testData = testData

    [<Property>]
    static member ``fromEffectful of function which produces test data returns the test data when run`` (testData : obj) =
        IO.run <| IO.fromEffectful (fun _ -> testData) = testData

    [<Property>]
    static member ``traverse matches results of map when run on pure binding function`` (testData : int list) =
        let test = IO.traverse (IO.return' << ((+)1)) testData
        let result = List.ofSeq <| IO.run test
        let mappedTestData = List.map ((+) 1) testData
        result = mappedTestData

    [<Property>]
    static member ``foldM matches results of fold when run on pure binding function`` (testData : int list) =
        let test = IO.foldM (fun acc b -> IO.return' <| acc - b) 0 testData
        let result = IO.run test
        let foldedTestData = List.fold ((-)) 0 testData
        result = foldedTestData

    [<Property>]
    static member ``filterM matches results of filter when run on pure binding function`` (testData : int list) =
        let test = IO.filterM (IO.return' << ((>) 5)) testData
        let result = List.ofSeq <| IO.run test
        let filteredTestData = List.filter ((>) 5) testData
        result = filteredTestData

    [<Property>]
    static member ``chooseM matches results of choose when run on pure binding function`` (testData : int list) =
        let chooseFunc = function
            |x when x > 5 -> Some x
            |_ -> None
        let test = IO.chooseM (IO.return' << chooseFunc) testData
        let result = List.ofSeq <| IO.run test
        let filteredTestData = List.choose (chooseFunc) testData
        result = filteredTestData

    [<Property>]
    static member ``traverse does not create side effects until run`` (testData : obj list) =
        let createTestFail = IO.fromEffectful (fun _ -> failwith "Side effect created")
        let test = IO.traverse (fun _ -> createTestFail) testData
        true

    [<Property>]
    static member ``bracket calls close action if exception thrown`` () =
        let create = IO.return' ()
        let mutable called = false
        let closed = IO.fromEffectful (fun _ -> called <- true)
        let expt = IO.fromEffectful (fun _ -> raise BracketCloseTestException)
        try
            IO.bracket create (fun _ -> closed) (fun _ -> expt) |> IO.run
        with 
            | BracketCloseTestException -> ()
            | exn -> reraise()
        called = true

    [<Property>]
    static member ``bracket calls close action if exception not thrown`` () =
        let create = IO.return' ()
        let mutable called = false
        let closed = IO.fromEffectful (fun _ -> called <- true)
        let nothing = IO.fromEffectful (fun _ -> ())
        IO.bracket create (fun _ -> closed) (fun _ -> nothing) |> IO.run
        called = true

    [<Property>]
    static member ``bracket calls inner action`` () =
        let create = IO.return' ()
        let mutable called = false
        let closed = IO.fromEffectful (fun _ -> ())
        let act = IO.fromEffectful (fun _ -> called <- true)
        IO.bracket create (fun _ -> closed) (fun _ -> act) |> IO.run
        called = true

    [<Property(MaxTest=1)>]
    static member ``iterM over sequence of 0..1e6 value and incr action produces 1e6 value`` () =
        let mutable x = ref 0
        let action = IO.fromEffectful (fun _ -> incr x)
        let ios = Seq.init (1000000) id
        let iterm = IO.iterM (const' action) ios
        IO.run iterm
        !x = 1000000

    [<Property(MaxTest=1)>]
    static member ``for over sequence of 0..1e6 value and incr action produces 1e6 value`` () =
        let mutable x = ref 0
        let action = IO.fromEffectful (fun _ -> incr x)
        io {
            for x in [1..1000000] do
                do! action
        } |> IO.run
        !x = 1000000


