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

/// Console functions
module Console =
    /// An action that reads a key from the console
    let readKey = IO.fromEffectful (fun () -> System.Console.ReadKey())
    /// An action that reads a line from the console
    let readLine = IO.fromEffectful (fun () -> System.Console.ReadLine())
    /// An action that writes a line to the console
    let writeLine (str : string) = IO.fromEffectful (fun () -> System.Console.WriteLine str)

/// Threading functions
module Thread =
    /// An action that causes the current thread to sleep for a supplied number of milliseconds
    let sleep (ms : int) = IO.fromEffectful (fun _ -> System.Threading.Thread.Sleep(ms))

    /// An action that causes the current thread to yield execution to another thread
    let yld = IO.fromEffectful (fun _ -> ignore <| System.Threading.Thread.Yield())

/// Provides purely functional Date/Time functions
module DateTime =
    /// An aciton that gets the current local time
    let localNow = IO.fromEffectful (fun () -> System.DateTime.Now)
    /// An aciton that gets the current UTC time
    let utcNow = IO.fromEffectful (fun () -> System.DateTime.UtcNow)

