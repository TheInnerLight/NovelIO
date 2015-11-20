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
    /// return a result in the form of an IO expression
    let exReturn x = 
        fun (token : #IIO) -> IOSuccess(x, token)
    /// bind operation for #IO -> IOResult
    let (>>=) x f =
        fun (token : #IIO) ->
            match x token with
            |IOSuccess (res, token2) -> f res token2
            |IOError error -> IOError error
    /// lift2 operation for #IO -> IOResult
    let lift2 f x1 x2 =
        fun (token : #IIO) ->
            x1 >>= (fun a -> x2 >>= (fun b -> exReturn (f a b)))
    /// lift3 operation for #IO -> IOResult
    let lift3 f x1 x2 x3 =
        fun (token : #IIO) ->
            x1 >>= (fun a -> x2 >>= (fun b -> x3 >>= (fun c -> exReturn (f a b c) )))
    /// lift4 operation for #IO -> IOResult
    let lift4 f x1 x2 x3 x4 =
        fun (token : #IIO) ->
            x1 >>= (fun a -> x2 >>= (fun b -> x3 >>= (fun c -> x4 >>= (fun d -> exReturn (f a b c d) )))) 

open IOExpressionFunctions

/// A builder for handling IO expressions in computation expressions
type IOBuilder<'b when 'b :> IIO>() =
    /// bind operation in expression for #IOToken -> IOResult
    member this.Bind(x, f) = x >>= f
    /// return a result in the form of an IO expression
    member this.Return(x) = IOExpressionFunctions.exReturn x
    /// return an IO expression
    member this.ReturnFrom(x) = x

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
    let tuple2 f1 f2 = lift2 (fun a b -> a, b) f1 f2
    /// Convert three IO expressions in a single IO expression returning a tuple of the merged results
    let tuple3 f1 f2 f3 = lift3 (fun a b c -> (a, b, c)) f1 f2 f3
    /// Convert four IO expressions in a single IO expression returning a tuple of the merged results
    let tuple4 f1 f2 f3 f4 = lift4 (fun a b c d -> (a, b, c, d)) f1 f2 f3 f4



