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

type FileSystemWatcher = private {Watcher : System.IO.FileSystemWatcher}

type Renamed = { OldPath : Filename; NewPath : Filename}

type FileSystemEvent =
    |Changed of Filename
    |Created of Filename
    |Deleted of Filename
    |Renamed of Renamed

module Observable =
    let concat obs = List.reduce (Observable.merge) obs

module Watcher =
    let createForPath path = {Watcher = new System.IO.FileSystemWatcher(path)}

    let private mapToObs ctor (evt) =
        evt |> Observable.map (fun (fArgs : FileSystemEventArgs) -> ctor <| File.assumeValidFilename fArgs.FullPath)

    let subscribe watcher f =
        
        let obsCreated = mapToObs Created watcher.Watcher.Created 
        let obsChanged = mapToObs Changed watcher.Watcher.Changed 
        let obsDeleted = mapToObs Deleted watcher.Watcher.Deleted

        let obsRenamed = 
            watcher.Watcher.Renamed 
            |> Observable.map (fun rArgs -> 
                Renamed {OldPath = File.assumeValidFilename rArgs.OldFullPath; NewPath = File.assumeValidFilename rArgs.FullPath})
        
        let all = Observable.concat [obsCreated; obsChanged; obsDeleted; obsRenamed]

        let disp = Observable.subscribe (IO.run << f) all
        IO.fromEffectful (fun _ -> disp.Dispose())

