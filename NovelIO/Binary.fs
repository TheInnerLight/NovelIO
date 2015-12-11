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

/// Generic stream for binary IO reading
type IBinaryReadStream =
    inherit IOStream
    abstract member Reader : BinaryReader
/// Generic stream for binary IO writing
type IBinaryWriteStream =
    inherit IOStream
    abstract member Writer : BinaryWriter

/// A stream for reading and writing with Binary IO
type BinaryIOReadWriteStream = 
    private {Reader : BinaryReader; Writer : BinaryWriter}
    interface IBinaryReadStream with
        member this.Reader = this.Reader
    interface IBinaryWriteStream with
        member this.Writer = this.Writer
/// A stream for writing with Binary IO
type BinaryIOWriteStream = 
    private {Writer : BinaryWriter}
    interface IBinaryWriteStream with
        member this.Writer = this.Writer
/// A stream for reading with Binary IO
type BinaryIOReadStream = 
    private {Reader : BinaryReader}
    interface IBinaryReadStream with
        member this.Reader = this.Reader

open IOFormats

/// Functions for binary IO
module BinaryIO =
    let private readBasic<'a,'b when 'a :> IBinaryReadStream> (f : 'a ->'b) (bw : 'a) =
        try 
            f bw |> IOSuccess
        with
            | :? EndOfStreamException as eose -> PastEndOfStream eose |> IOError
            | :? System.ObjectDisposedException as ode -> StreamClosed ode |> IOError

    //
    // ------------ Binary file handle ------------ 
    //

    /// create a handle for reading binary files
    let createBinaryReadHandle path = 
        {Reader = new BinaryReader(new FileStream(path, FileMode.Open))} |> IO.return'
    /// create a handle for writing binary files
    let createBinaryWriteHandle path = 
        {Writer = new BinaryWriter(new FileStream(path, FileMode.CreateNew))} |> IO.return'
    /// create a handle for reading and writing binary files
    let createBinaryReadWriteHandle path = 
        {Reader = new BinaryReader(new FileStream(path, FileMode.Open)); Writer = new BinaryWriter(new FileStream(path, FileMode.CreateNew))} |> IO.return'

    let run (format : IOFormat<_,_>) io = IO.bind io (fun stream -> format stream)

    // 
    // ------------ Read any value formats ------------ 
    //

    /// A binary read format for reading decimals
    let readDecimal<'a when 'a :> IBinaryReadStream>() : IOFormat<_,'a> = 
        readBasic (fun bw -> bw.Reader.ReadDecimal())
    /// A binary read format for reading int16s
    let readInt16<'a when 'a :> IBinaryReadStream>() : IOFormat<_,'a> = 
        readBasic (fun bw -> bw.Reader.ReadInt16())
    /// A binary read format for reading int32s
    let readInt32<'a when 'a :> IBinaryReadStream>() : IOFormat<_,'a> = 
        readBasic (fun bw -> bw.Reader.ReadInt32())
    /// A binary read format for reading int64s
    let readInt64<'a when 'a :> IBinaryReadStream>() : IOFormat<_,'a> = 
        readBasic (fun bw -> bw.Reader.ReadInt64())
    /// A binary read format for reading float32s
    let readFloat32<'a when 'a :> IBinaryReadStream>() : IOFormat<_,'a> = 
        readBasic (fun bw -> bw.Reader.ReadSingle())
    /// A binary read format for reading float64s
    let readFloat64<'a when 'a :> IBinaryReadStream>() : IOFormat<_,'a> = 
        readBasic (fun bw -> bw.Reader.ReadDouble())
    /// A binary read format for reading strings
    let readString<'a when 'a :> IBinaryReadStream>() : IOFormat<_,'a> = 
        readBasic (fun bw -> bw.Reader.ReadString())
    
    // 
    // ------------ Read specific value formats ------------ 
    //

    /// A binary read format which reads a specified decimal
    let expectDecimal<'a when 'a :> IBinaryReadStream> dec : IOFormat<_,'a>  = expectSpecificValue (readDecimal()) dec
    /// A binary read format which reads a specified int16
    let expectInt16<'a when 'a :> IBinaryReadStream>  i16 : IOFormat<_,'a>  = expectSpecificValue (readInt16()) i16
    /// A binary read format which reads a specified int32
    let expectInt32<'a when 'a :> IBinaryReadStream>  i32 : IOFormat<_,'a>  = expectSpecificValue (readInt32()) i32
    /// A binary read format which reads a specified int64
    let expectInt64<'a when 'a :> IBinaryReadStream>  i64 : IOFormat<_,'a>  = expectSpecificValue (readInt64()) i64
    /// A binary read format which reads a specified float32
    let expectFloat32<'a when 'a :> IBinaryReadStream>  f32 : IOFormat<_,'a>  = expectSpecificValue (readFloat32()) f32
    /// A binary read format which reads a specified float64
    let expectFloat64<'a when 'a :> IBinaryReadStream>  f64 : IOFormat<_,'a>  = expectSpecificValue (readFloat64()) f64
    /// A binary read format which reads a specified string
    let expectString<'a when 'a :> IBinaryReadStream>  str : IOFormat<_,'a>  = expectSpecificValue (readString()) str
    
