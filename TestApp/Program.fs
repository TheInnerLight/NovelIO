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

open NovelFS.NovelIO

[<EntryPoint>]
let main argv = 

    let fileReader = io {
        let! a = BinaryIO.readByte
        let! b = BinaryIO.readChar
        let! c = BinaryIO.readFloat32
        let! d = BinaryIO.readByte
        let! lst1 = IO.list (int d) (BinaryIO.readFloat)
        let! lst2 = IO.sequence [BinaryIO.readChar; BinaryIO.readChar; BinaryIO.readChar; BinaryIO.readChar; BinaryIO.readChar]
        return a, b, c, d, lst2
        }

    let result =  
        match BinaryIO.run (FileReadIO("test.txt",fileReader)) with
        |IOSuccess (res, tok2) -> res
        |IOError e -> failwith "error"

    printfn "%A" result

    let writer =
        io {
            do! TextIO.writeLine "Hello, this is a purely functional IO library"
            do! TextIO.writeLine "Here is some nice text"
            do! TextIO.writeLine "Isn't that great?"
            do! IO.mapM_ (TextIO.printfn "printing %f") [1.0; 1.0; 1.0]
            return ()
        }

    let result =  
        match TextIO.run (FileWriteIO ("test3.txt", writer)) with
        |IOSuccess (res, tok2) -> res
        |IOError e -> failwith "error"

    let tcpListener = System.Net.Sockets.TcpListener(System.Net.IPAddress.Any, 9001)
    tcpListener.Start()
    let tcpReader =
        io{ 
            let! one = TextIO.readLine
            let! two = TextIO.readLine
            return one, two
        }

    let clientHandler =
        async{
            let! socket = Async.AwaitTask <| tcpListener.AcceptSocketAsync()
            match TextIO.run (TCPServerSocketReadIO(socket, tcpReader)) with
            |IOSuccess (res, tok2) -> printfn "%A" res
            |IOError e -> failwith "error"
            }
    Async.RunSynchronously(clientHandler)
    0

