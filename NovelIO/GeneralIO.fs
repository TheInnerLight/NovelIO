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

/// A builder for handling IO expressions in computation expressions
type IOBuilder<'b when 'b :> IIOToken>() =

    member this.Bind(x, f) =
        fun token ->
            match x token with
            |IOSuccess (res, token2) -> f res token2
            |IOError error -> IOError error

    member this.Return(x) =
        fun token -> IOSuccess(x, token)

    member this.ReturnFrom(x) =
        x

[<AutoOpen>]
module IOExpressions =
    let io<'a when 'a :> IIOToken> = IOBuilder<'a>()

/// General IO functions
module IO =
    /// Perform some file IO and check for exceptions
    let performIoWithExceptionCheck f =
        try
            f() |> IOSuccess 
        with
        | :? System.IO.DirectoryNotFoundException as dnfe -> DirectoryNotFound dnfe |> IOError
        | :? System.IO.DriveNotFoundException as dnfe -> DriveNotFound dnfe |> IOError
        | :? System.IO.EndOfStreamException as eose -> PastEndOfStream eose |> IOError
        | :? System.IO.FileNotFoundException as fnfe -> FileNotFound fnfe |> IOError
        | :? System.IO.PathTooLongException as ptle -> PathTooLong ptle |> IOError
        | :? System.ObjectDisposedException as ode -> StreamClosed ode |> IOError
        | :? System.UnauthorizedAccessException as unax -> UnauthourisedAccess unax |> IOError
        | :? System.IO.IOException as ioex -> Other ioex |> IOError
    /// Transform n copies of an IO Expression into a new IO Expression containg a list of n results 
    let list n f token =
        List.init n id 
        |> List.fold (fun acc v ->
            match acc with
            |IOSuccess (lstC, token) ->
                match f token with
                |IOSuccess (res, newToken) -> IOSuccess (res :: lstC, newToken) 
                |IOError e -> IOError e
            |IOError e -> IOError e) (IOSuccess([], token))

    let mapM listFs token =
        listFs
        |> List.fold (fun acc v ->
            match acc with
            |IOSuccess (lstC, token) ->
                match v token with
                |IOSuccess (res, newToken) -> IOSuccess (res :: lstC, newToken) 
                |IOError e -> IOError e
            |IOError e -> IOError e) (IOSuccess([], token))



