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

namespace NovelFS.NovelIO

open System.IO

type Filename internal (path : string) =
    let path =
        match path.IndexOfAny(Path.GetInvalidFileNameChars()) = -1 with
        |true -> invalidArg "path" "Path Invalid"
        |false -> path
    member this.PathString = path

[<AutoOpen>]
module PathDiscriminators =
    let (|ValidFilename|InvalidFilename|) (path : string) =
        match path.IndexOfAny(Path.GetInvalidFileNameChars()) = -1 with
        |true -> ValidFilename (Filename path)
        |false -> InvalidFilename

module File =
    let assumeValidFilename path =
        match path with
        |ValidFilename fname -> fname
        |InvalidFilename -> invalidArg "path" "Assumption of valid path was not correct."

    let fileExists (filename : Filename) = File.Exists (filename.PathString)

    let readLines (filename : Filename) : IO<seq<string>> = IO.return' (File.ReadLines filename.PathString)

