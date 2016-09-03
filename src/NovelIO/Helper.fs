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

open System.IO

module internal InternalIOHelper =
    /// Helper function to catch IO exceptions and structure the success/failure
    let withExceptionCheck f a : Result<_,_> =
        try 
            Choice1Of2 <| f a 
        with
            | exn -> Choice2Of2 exn

