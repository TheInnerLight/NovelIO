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

module BinaryReading =
    let createBinaryReadToken path =
        GeneralIO.ioCallWithExceptionCheck 
            (fun () ->
                let br = new BinaryReader(File.Open(path, FileMode.Open))
                (), BinaryReadFileToken(br))

    let private readGeneral readFunction (token : BinaryReadFileToken) =
        token.IOStatus |> IOStatus.map 
            (token.Invalidate(); GeneralIO.ioCallWithExceptionCheck (fun () -> token.BinaryReader |> readFunction, BinaryReadFileToken(token.BinaryReader)) )
            (InvalidToken |> IOError)
        
    let readByte (token : BinaryReadFileToken) = token |> readGeneral (fun br -> br.ReadByte())
    let readInt16 (token : BinaryReadFileToken) = token |> readGeneral (fun br -> br.ReadInt16())
    let readInt (token : BinaryReadFileToken) = token |> readGeneral (fun br -> br.ReadInt32())
    let readInt64 (token : BinaryReadFileToken) = token |> readGeneral (fun br -> br.ReadInt64())  
    let readFloat (token : BinaryReadFileToken) = token |> readGeneral (fun br -> br.ReadDouble())        
    let readFloat32 (token : BinaryReadFileToken) = token |> readGeneral (fun br -> br.ReadSingle())
    let readString(token : BinaryReadFileToken) = token |> readGeneral (fun br -> br.ReadString()) 

