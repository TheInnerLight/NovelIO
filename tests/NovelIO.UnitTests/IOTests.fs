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

namespace NovelFS.NovelIO.UnitTests

open NovelFS.NovelIO
open NovelFS.NovelIO.BinaryParser
open FsCheck
open FsCheck.Xunit

type ``IO Unit Tests``() =
    [<Property>]
    static member ``Function: return' of some test data returns the test data when run`` (testData : obj) =
        match IO.run <| IO.return' testData with
        |IOSuccess testDataRes -> testData = testDataRes
        |IOError _ -> failwith "run produced an unexpected error"

    [<Property>]
    static member ``Function: fromEffectful of function which produces test data returns the test data when run`` (testData : obj) =
        match IO.run <| IO.fromEffectful (fun _ -> testData) with
        |IOSuccess testDataRes -> testData = testDataRes
        |IOError _ -> failwith "run produced an unexpected error"