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

namespace NovelFS.NovelIO

open System.IO
open System.Net

/// Provides functions and types relating to network connectivity
module Network =
    /// Type abbreviation for System.Net.IPAddress
    type IPAddress = System.Net.IPAddress

/// Provides functions relating to TCP connections
module TCP =
    /// Create a TCP server at the specfied IP on the specified port
    let createServer ip port = IO.fromEffectful (fun () -> SideEffectingIO.startTCPServer ip port)

    /// Create a TCP server at the specfied IP
    let createServerOnFreePort ip = IO.fromEffectful (fun () -> SideEffectingIO.startTCPServer ip 0)

    /// Accept a connection from the supplied TCP server
    let acceptConnection serv = IO.fromEffectful (fun () -> SideEffectingIO.acceptSocketFromServer serv)

    /// Close a connected socket
    let closeConnection socket = IO.fromEffectful (fun () -> SideEffectingIO.closeSocket socket)

    /// Create a TCP connection to the supplied IP and specified port
    let connectSocket ip port = IO.fromEffectful (fun () -> SideEffectingIO.connectTCPSocket ip port)

    /// Retrieves the port the server is listening on
    let getServerPort server = 
        IO.fromEffectful (fun () -> 
            let ipend = server.TCPListener.Server.LocalEndPoint :?> System.Net.IPEndPoint
            ipend.Port)

    /// Create a handle from a connected socket
    let socketToHandle tcpSocket =
        IO.return' 
            {TextReader = new StreamReader(new Sockets.NetworkStream(tcpSocket.TCPConnectedSocket)) :> TextReader |> Some;
             TextWriter = new StreamWriter(new Sockets.NetworkStream(tcpSocket.TCPConnectedSocket)) :> TextWriter |> Some}


