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

type FSWatcher = private {Watcher : System.IO.FileSystemWatcher}

type Renamed = { OldPath : Filename; NewPath : Filename}

type FileSystemEvent =
    |Changed of Filename
    |Created of Filename
    |Deleted of Filename
    |Renamed of Renamed

/// Additional functions on IObservable<'T>
module Observable =
    /// Concatenate a list of observables of the same type into a single observable
    let concat obs = List.reduce (Observable.merge) obs

module FileSystemWatcher =

    let private mapToObs ctor (evt) =
        evt |> Observable.map (fun (fArgs : FileSystemEventArgs) -> ctor <| File.assumeValidFilename fArgs.FullPath)

    /// An action that creates a file system watcher for the supplied path
    let createForPath path = 
        IO.fromEffectful (fun _ -> 
            let watcher = new System.IO.FileSystemWatcher(File.getPathString path)
            watcher.EnableRaisingEvents <- true
            {Watcher = watcher})

    /// An action that subscribes to the file system watcher, the contained IO action is used to unsubscribe
    let subscribe watcher f =
        IO.fromEffectful (fun _ ->
            // create observables of FileSystemEvent for created, changed and deleted
            let obsCreated = mapToObs Created watcher.Watcher.Created 
            let obsChanged = mapToObs Changed watcher.Watcher.Changed 
            let obsDeleted = mapToObs Deleted watcher.Watcher.Deleted
            // now create observable for renamed, use map because we need to store old path and new path for this one
            let obsRenamed = 
                watcher.Watcher.Renamed 
                |> Observable.map (fun rArgs -> 
                    Renamed {OldPath = File.assumeValidFilename rArgs.OldFullPath; NewPath = File.assumeValidFilename rArgs.FullPath})
            // concat all the file system observables together
            let all = Observable.concat [obsCreated; obsChanged; obsDeleted; obsRenamed]
            // subscribe and create the disposal action
            let disp = Observable.subscribe (IO.run << f) all
            IO.fromEffectful (fun _ -> disp.Dispose()))

