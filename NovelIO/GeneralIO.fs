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
type IOBuilder<'b when 'b :> IIOToken>(tokenCreator : 'b, tokenDestroyer : 'b -> unit) =
    let mutable stateToken =  tokenCreator with

    member this.Bind(x, f) =
        match x stateToken with
        |IOSuccess (res, token) ->
            stateToken <- token 
            f res
        |IOError error -> IOError error

    member this.Return(x) =
        tokenDestroyer stateToken
        IOSuccess(x, ())

    member this.ReturnFrom(x) =
        tokenDestroyer stateToken
        x

[<AutoOpen>]
module IOExpressions =
    let io creator destroyer = IOBuilder(creator, destroyer)

/// General IO functions
module IO =
    /// Convert two IO formats of types <'a> and <'b> into a combined IO format of type<'a*'b>
    let inline tuple2 (a:^a) (b:^b) =
        (^a : (static member Tuplify2: ^a * ^b -> ^c) (a, b))
    /// Convert three IO formats of types <'a>, <'b> and <'c> into a combined IO format of type<'a*'b'c>
    let inline tuple3 (a:^a) (b:^b) (c:^c)  =
        (^a : (static member Tuplify3: ^a * ^b * ^c -> ^d) (a, b, c))
    /// Convert an IO format of type <'a> into an IO format of type <'a list> with supplied length
    let inline list len (a:^a) =
        (^a : (static member Listify: ^a * int -> ^b) (a, len))
    /// Convert an IO format of type <'a> into an IO format of type <'a[]> with supplied length
    let inline array len (a:^a)  =
        (^a : (static member Arrayify: ^a * int -> ^b) (a, len))


