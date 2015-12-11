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

type IOFormat<'a,'b when 'b :> IOStream> = 'b -> 'a

module IOFormats =
    /// return a result in the form of an IOParser
    let return' a : IOFormat<_,_> = 
        fun (reader : #IOStream) -> IOSuccess a
    /// bind function for IOParsers
    let bind (x : IOFormat<_,_>) (f : 'a -> IOFormat<_,_>) : IOFormat<_,_> =
        fun (reader : #IOStream) ->
            match x reader with
            |IOSuccess a -> f a reader
            |IOError b -> IOError b
    /// bind operator for IOParsers
    let (>>=) x f = bind x f
    /// lift2 operation for IOParsers
    let lift2 f x1 x2 =
        x1 >>= (fun a -> x2 >>= (fun b -> return' (f a b)))
    /// lift3 operation for IOParsers
    let lift3 f x1 x2 x3 =
        x1 >>= (fun a -> x2 >>= (fun b -> x3 >>= (fun c -> return' (f a b c) )))
    /// lift4 operation for IOParsers
    let lift4 f x1 x2 x3 x4 =
        x1 >>= (fun a -> x2 >>= (fun b -> x3 >>= (fun c -> x4 >>= (fun d -> return' (f a b c d) )))) 
    /// Convert a pair of IO expressions in a single IO expression returning a tuple of the merged results
    let tuple2 f1 f2 = lift2 (fun a b -> a, b) f1 f2
    /// Convert three IO expressions in a single IO expression returning a tuple of the merged results
    let tuple3 f1 f2 f3 = lift3 (fun a b c -> (a, b, c)) f1 f2 f3
    /// Convert four IO expressions in a single IO expression returning a tuple of the merged results
    let tuple4 f1 f2 f3 f4 = lift4 (fun a b c d -> (a, b, c, d)) f1 f2 f3 f4
    /// Transforms a function from an object to IO Expression and a list of objects into an IO Expression returning a list of results 
    let mapM mFunc list : IOFormat<_,_>  =
        let folder head tail = 
            mFunc head >>= (fun h -> 
                tail >>= (fun t ->
                    return' (h::t) ))
        List.foldBack (folder) list (return' [])
    /// As mapM but ignores the results
    let mapM_ mFunc list : IOFormat<_,_>  =
        mapM mFunc list >>= (fun x -> return'())
    /// Transform a list of IOFormats into an IOFormat of list results
    let sequence list : IOFormat<_,_>  =
        mapM (id) list
    /// Applies the supplied IOFormat n times
    let replicateM mFunc n : IOFormat<_,_>  =
        sequence (List.init n (fun _ -> mFunc))
    /// Applies the supplied IOFormat n times and ignores the result
    let replicateM_ mFunc n : IOFormat<_,_>  =
        replicateM mFunc n >>= (fun f -> return' ())
    /// A format which expects a particular supplied value from a supplied format
    let expectSpecificValue format value =
        format >>= (fun i bw ->
            if i = value then return' i bw
            else IOError <| IncorrectFormat)

open IOFormats

type IOFormatBuilder() =
    static member Return x = return' x
    static member Bind(x, f) = x >>= f
    static member Zero = return' ()

module IOHandlerBuilders =
    let ioformatter = IOFormatBuilder()

