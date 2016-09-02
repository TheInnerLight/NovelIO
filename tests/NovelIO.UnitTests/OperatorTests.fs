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

type ``IO Operator Tests``() =
    [<Property>]
    static member ``*> operator does action on left, then action on right and returns result of action on right`` () =
        let x = ref 0
        let addAction = IO.fromEffectful (fun _ -> 
            x := !x + 3
            10)
        let multAction = IO.fromEffectful (fun _ -> 
            x := !x * 3
            1)
        let act = addAction *> multAction
        let result = IO.run act
        result = 1 && !x = 9 // 9 confirms the order since 0 + 3 * 3 = 9 but 0 * 3 + 3 = 3

    [<Property>]
    static member ``<* operator does action on left, then action on right and returns result of action on left`` () =
        let x = ref 0
        let addAction = IO.fromEffectful (fun _ -> 
            x := !x + 3
            10)
        let multAction = IO.fromEffectful (fun _ -> 
            x := !x * 3
            1)
        let act = addAction <* multAction
        let result = IO.run act
        result = 10 && !x = 9 // 9 confirms the order since 0 + 3 * 3 = 9 but 0 * 3 + 3 = 3

        


