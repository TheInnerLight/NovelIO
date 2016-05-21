(*
   Copyright 2015-2016 Philip Curzon

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

namespace NovelFS.NovelIO.IntegrationTests

open NovelFS.NovelIO
open NovelFS.NovelIO.Network
open FsCheck
open FsCheck.Xunit

type ``TCP Integration Tests``() =
    [<Property>]
    static member ``Function: createServer creates server on free port which can be connected to be TCP Client`` () =
        let port = IO.run <| io{
            let! server = TCP.createServerOnFreePort (IPAddress.Any)
            let! port = TCP.getServerPort server
            do! IO.forkIO <| TCP.acceptConnection server // fork the client acceptance to new thread so we can return the port immediately
            return port
          }
        use client = new System.Net.Sockets.TcpClient()
        client.Connect("127.0.0.1", port)
        true


    [<Property>]
    static member ``Function: connectSocket connects to created TCP Server`` () =
        IO.run <| io{
            let! server = TCP.createServerOnFreePort (IPAddress.Any)
            let! port = TCP.getServerPort server
            do! IO.forkIO <| TCP.acceptConnection server // fork the client acceptance to new thread so we can return the port immediately
            let! sock = TCP.connectSocket (IPAddress.Loopback) port
            return ()
          }
        true
