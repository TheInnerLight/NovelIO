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

module internal IOExpressionFunctions =
    let exReturn x = 
        fun (token : #IIO) -> IOSuccess(x, token)
    /// bind operation for #IOToken -> IOResult
    let (>>=) x f =
        fun (token : #IIO) ->
            match x token with
            |IOSuccess (res, token2) -> f res token2
            |IOError error -> IOError error

open IOExpressionFunctions

/// A builder for handling IO expressions in computation expressions
type IOBuilder<'b when 'b :> IIO>() =
    /// bind operation in expression for #IOToken -> IOResult
    member this.Bind(x, f) =
        x >>= f

    member this.Return(x) =
        IOExpressionFunctions.exReturn x

    member this.ReturnFrom(x) =
        x

[<AutoOpen>]
module IOExpressions =
    let io<'a when 'a :> IIO> = IOBuilder<'a>()

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
            acc >>= (fun lstC -> 
                f >>= (fun b -> IOExpressionFunctions.exReturn (b::lstC) )))
            (fun a -> IOSuccess([], token))) token
    /// Transform a list of IO Expressions into a list of IO results
    let mapM listFs token =
        (listFs
        |> List.fold (fun acc f ->
            acc >>= (fun lstC -> 
                f >>= (fun b -> IOExpressionFunctions.exReturn (b::lstC) )))
            (fun a -> IOSuccess([], token))) token
    /// Convert a pair of IO expressions in a single IO expression returning a tuple of the merged results
    let tuple2 f1 f2 =
        f1 >>= (fun a -> f2 >>= (fun b -> exReturn (a,b) ))
    /// Convert three IO expressions in a single IO expression returning a tuple of the merged results
    let tuple3 f1 f2 f3 =
        f1 >>= (fun a -> f2 >>= (fun b -> f3 >>= (fun c -> exReturn (a,b,c) )))
    /// Convert four IO expressions in a single IO expression returning a tuple of the merged results
    let tuple4 f1 f2 f3 f4 =
        f1 >>= (fun a -> f2 >>= (fun b -> f3 >>= (fun c -> f4 >>= (fun d -> exReturn (a,b,c,d) )))) 



