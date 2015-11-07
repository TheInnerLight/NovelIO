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

module private IOExpressionFunctions = 
    let bind x f =
        fun (token : #IIOToken) ->
            match x token with
            |IOSuccess (res, token2) -> f res token2
            |IOError error -> IOError error

    let ereturn x = 
        fun (token : #IIOToken) -> IOSuccess(x, token)

/// A builder for handling IO expressions in computation expressions
type IOBuilder<'b when 'b :> IIOToken>() =

    member this.Bind(x, f) =
        IOExpressionFunctions.bind x f

    member this.Return(x) =
        IOExpressionFunctions.ereturn x

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
    /// Transform n copies of an IO Expression into a new IO Expression containing a list of n results 
    let list n f token =
        ([1..n]
        |> List.fold (fun acc _ ->
            IOExpressionFunctions.bind acc (fun lstC -> 
                IOExpressionFunctions.bind f (fun b t2 -> IOExpressionFunctions.ereturn (b::lstC)  t2)))
            (fun a -> IOSuccess([], token))) token


    let mapM listFs token =
        (listFs
        |> List.fold (fun acc f ->
            IOExpressionFunctions.bind acc (fun lstC -> 
                IOExpressionFunctions.bind f (fun b t2 -> IOExpressionFunctions.ereturn (b::lstC)  t2)))
            (fun a -> IOSuccess([], token))) token



