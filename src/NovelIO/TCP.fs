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
    module private SideEffecting =
        /// Accept a socket from a TCP Server
        let acceptSocketFromServer serv =
            {TCPConnectedSocket = serv.TCPListener.AcceptSocket()}

        /// Connect a TCP Socket to a specified ip and port
        let connectTCPSocket (ip : IPAddress) (port : int) =
            let sock = new Sockets.Socket(Sockets.SocketType.Stream, Sockets.ProtocolType.Tcp)
            sock.Connect(ip, port)
            {TCPConnectedSocket = sock}

        /// Close a socket
        let closeSocket sock = sock.TCPConnectedSocket.Disconnect(false)

        /// Retrieves the port the server is listening on
        let getServerPort server = (server.TCPListener.Server.LocalEndPoint :?> System.Net.IPEndPoint).Port

        /// Start a TCP server on a supplied ip address and port
        let startTCPServer ip port =
            let listener = Sockets.TcpListener(ip, port)
            listener.Start()
            {TCPListener = listener}

    /// Accept a connection from the supplied TCP server
    let private acceptConn serv = IO.fromEffectful (fun () -> SideEffecting.acceptSocketFromServer serv)

    /// Create a TCP server at the specfied IP on the specified port
    let createServer ip port = IO.fromEffectful (fun () -> SideEffecting.startTCPServer ip port)

    /// Create a TCP server at the specfied IP
    let createServerOnFreePort ip = IO.fromEffectful (fun () -> SideEffecting.startTCPServer ip 0)

    /// Close a connected socket
    let closeConnection socket = IO.fromEffectful (fun () -> SideEffecting.closeSocket socket)

    /// Accept a connection from the supplied TCP server and handle it with the supplied function 
    let acceptConnection serv f = IO.bracket (acceptConn serv) (closeConnection) (f)

    /// Accept a connection from the supplied TCP server and handle it with the supplied function on a different thread
    let acceptFork serv f = IO.forkIO <| acceptConnection serv f

    /// Create a TCP connection to the supplied IP and specified port
    let connectSocket ip port = IO.fromEffectful (fun () -> SideEffecting.connectTCPSocket ip port)

    /// Retrieves the port the server is listening on
    let getServerPort server = IO.fromEffectful (fun () -> SideEffecting.getServerPort server)

    /// Create a text channel from a connected socket
    let socketToTextChannel tcpSocket =
        IO.fromEffectful (fun _ ->
            {
                TextReader = new StreamReader(new Sockets.NetworkStream(tcpSocket.TCPConnectedSocket)) |> Some;
                TextWriter = new StreamWriter(new Sockets.NetworkStream(tcpSocket.TCPConnectedSocket)) |> Some;
             })

    /// Create a binary channel from a connect socket
    let socketToBinaryChannel tcpSocket =
        IO.fromEffectful (fun _ ->
            let nStream = new Sockets.NetworkStream(tcpSocket.TCPConnectedSocket)
            {
                IOStream = nStream; 
                EOS = false
            })


