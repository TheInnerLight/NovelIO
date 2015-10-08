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

module internal IOStatus =
    let map validFunc invalidFunc status =
        match status with
        |Valid -> validFunc()
        |Invalid -> invalidFunc()

type IOBuilder() =
    member this.Bind(x, f) =
        match x with
        |IOSuccess (res, token) -> f (res, token)
        |IOError error -> IOError error

    member this.Return(x) =
        x

type IOTokenBuilder<'b when 'b :> IIOToken>(tokenCreator : unit -> IOResult<unit, 'b>, tokenDestroyer : 'b -> IOResult<unit, unit>) =
    let mutable stateToken = 
        match tokenCreator() with
        |IOSuccess (_, token) -> token
        |IOError _ -> raise <| System.InvalidOperationException()

    member this.Bind(x, f) =
        match x stateToken with
        |IOSuccess (res, token) ->
            stateToken <- token 
            f res
        |IOError error -> IOError error

    member this.Return(x) =
        match tokenDestroyer stateToken with
        |IOSuccess (_, _) -> IOSuccess(x, ())
        |IOError error -> IOError error

    member this.ReturnFrom(x) =
        x stateToken

[<AutoOpen>]
module IOExpressions =
    let io = IOBuilder()

module GeneralIO =
    let ioCallWithExceptionCheck call =
        try
            call() |> IOSuccess
        with
        | :? DirectoryNotFoundException as dnfe -> DirectoryNotFound dnfe |> IOError
        | :? DriveNotFoundException as dnfe -> DriveNotFound dnfe |> IOError
        | :? EndOfStreamException as eose -> PastEndOfStream eose |> IOError
        | :? FileNotFoundException as fnfe -> FileNotFound fnfe |> IOError
        | :? PathTooLongException as ptle -> PathTooLong ptle |> IOError
        | :? System.ObjectDisposedException as ode -> StreamClosed ode |> IOError
        | :? System.UnauthorizedAccessException as unax -> UnauthourisedAccess unax |> IOError
        | :? IOException as ioex -> Other ioex |> IOError

    let private assume token =
        match token with
        |IOSuccess (succ, token) -> (succ, token)

    let iofy statement = IOSuccess (statement, ())

    let listOfN func n iToken =
        let first = func iToken
        match first with
        |IOSuccess (succ, token) ->
            [0..n-1] |> List.fold (fun acc _ ->
                match acc with
                |IOSuccess(res, token) ->
                    match func token with
                    |IOSuccess (newRes, newToken) -> IOSuccess(newRes :: res, newToken)
                    |IOError error -> IOError error 
                |IOError error -> IOError error) (IOSuccess([succ], token))
            |> function
            |IOSuccess (res, token) -> IOSuccess (List.rev res, token)
            |IOError error -> IOError error
        |IOError error -> IOError error

    let arrayOfN func n iToken =
        let first = func iToken
        match first with
        |IOSuccess (succ, token) ->
            [0..n-1] |> List.fold (fun acc _ ->
                match acc with
                |IOSuccess(res, token) ->
                    match func token with
                    |IOSuccess (newRes, newToken) -> IOSuccess(newRes :: res, newToken)
                    |IOError error -> IOError error 
                |IOError error -> IOError error) (IOSuccess([succ], token))
            |> function
               |IOSuccess (res, token) -> IOSuccess (List.rev res |> List.toArray, token)
               |IOError error -> IOError error
        |IOError error -> IOError error

    let remaining func iToken =
        let first = func iToken
        match first with
        |IOSuccess (succ, token) ->
            let lRes =
                (IOSuccess(succ, token)) |> Seq.unfold (fun st ->
                    let res = assume st |> snd |> func
                    match res with
                    |IOSuccess(suc2, token2) -> Some <| (res, res)
                    |_ -> None) |> Seq.toList
            IOSuccess(lRes |> List.map (fun x -> fst <| assume x), ())
        |IOError error -> IOError error

    let pipe2 one two func iToken =
        match one iToken with
        |IOSuccess(result, token) -> 
            match (two token) with
            |IOSuccess(result2, token2) -> IOSuccess(func result result2, token2)
            |IOError error2 -> IOError error2
        |IOError error -> error |> IOError

    let pipe3 one two three func iToken =
        match one iToken with
        |IOSuccess(result, token) -> 
            match (two token) with
            |IOSuccess(result2, token2) -> 
                match (three token2) with
                |IOSuccess(result3, token3) -> IOSuccess(func result result2 result3, token3)
                |IOError error3 -> IOError error3
            |IOError error2 -> IOError error2
        |IOError error -> error |> IOError

    let tuple2 one two = pipe2 one two (fun a b -> (a,b))

    let tuple3 one two three = pipe3 one two three (fun a b c -> (a,b,c))

[<AutoOpen>]
module GeneralIOOperators =
    let (.>>.) = GeneralIO.tuple2



