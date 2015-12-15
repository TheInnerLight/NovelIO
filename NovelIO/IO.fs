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
open System.Net

type IO<'a,'b,'c,'d when 'c :> IOStream> = 
    private 
    |Return of 'a
    |ConsoleWrite of Printf.TextWriterFormat<'b> * string * IO<'a,'b,'c,'d>
    |ConsoleWriteLine of Printf.TextWriterFormat<'b> * string * IO<'a,'b,'c,'d>
    |ConsoleReadKey of (System.ConsoleKeyInfo -> IO<'a,'b,'c,'d>)
    |ConsoleReadLine of (string -> IO<'a,'b,'c,'d>)
    |FileReadLines of Filename * (seq<string> -> IO<'a,'b,'c,'d>)
    |ReadAllBytes of Filename * (byte[] -> IO<'a,'b,'c,'d>)
    |OpenFileHandle of Filename * FileMode * (Handle -> IO<'a,'b,'c,'d>)
    |StartTCPListener of IPAddress * int * (TCPServer -> IO<'a,'b,'c,'d>)
    |TCPServerAccept of TCPServer * (TCPConnectedSocket -> IO<'a,'b,'c,'d>)
    |HGetLine of Handle * (string -> IO<'a,'b,'c,'d>)
    |HPutStringLine of Handle * string * IO<'a,'b,'c,'d>
    |ConnectedSocketToHandle of TCPConnectedSocket * (Handle -> IO<'a,'b,'c,'d>)
    |IsReady of Handle * (bool -> IO<'a,'b,'c,'d>)


module IO =
    /// Return a value as an IO value
    let return' x = Return x
    /// Monadic bind for IO values
    let rec bind x f =
        match x with
        |Return a -> f a
        |ConsoleWrite (fmt, str, a) -> ConsoleWrite (fmt, str, (bind a f))
        |ConsoleWriteLine (fmt, str, a) -> ConsoleWriteLine (fmt, str, (bind a f))
        |ConsoleReadKey (g) -> ConsoleReadKey (fun key -> bind (g key) f)
        |ConsoleReadLine (g) -> ConsoleReadLine (fun line -> bind (g line) f)
        |FileReadLines (fName, g) -> FileReadLines (fName, fun str -> bind (g str) f)
        |ReadAllBytes (fName, g) -> ReadAllBytes (fName, fun bytes -> bind (g bytes) f)
        |OpenFileHandle (fName, mode, g) -> OpenFileHandle(fName, mode, fun fName -> bind (g fName) f)
        |StartTCPListener (ip, port, g) -> StartTCPListener(ip, port, fun tcpServer -> bind(g tcpServer) f)
        |TCPServerAccept (serv, g) -> TCPServerAccept (serv, fun sock -> bind (g sock) f)
        |HGetLine (handle, g) -> HGetLine(handle, fun hand -> bind (g hand) f)
        |HPutStringLine (handle, str, a) -> HPutStringLine (handle, str, (bind a f))
        |ConnectedSocketToHandle (socket, g) -> ConnectedSocketToHandle(socket, fun hand -> bind (g hand) f)
        |IsReady (handle, g) -> IsReady (handle, fun bl -> bind (g bl) f)
    /// Monadic bind operator for IO values
    let (>>=) x f = bind x f
    /// Map each element of a list to a monadic action, evaluate these actions from left to right and collect the results.
    let mapM mFunc list =
        let folder head tail = 
            mFunc head >>= (fun h -> 
                tail >>= (fun t ->
                    return' (h::t) ))
        List.foldBack (folder) list (return' [])
    /// As mapM but ignores the result.
    let mapM_ mFunc list =
        mapM mFunc list >>= (fun x -> return' ())
    /// Evaluate each action in the list from left to right, and and collect the results.
    let sequence list =
        mapM (id) list
    /// Performs the action mFunc n times, gathering the results.
    let replicateM mFunc n =
        sequence (List.init n (fun _ -> mFunc))
    /// As replicateM but ignores the results
    let replicateM_ mFunc n  =
        replicateM mFunc n >>= (fun f -> return' ())

    let rec private mLoopHelper mFunc predF m lst =
        m >>= (fun res ->
            mFunc >>= (fun pred ->
                if predF pred then
                    mLoopHelper mFunc predF m (res::lst)
                else return' (List.rev lst)))

    let untilM mBool m =
        mLoopHelper mBool ((=) false) m []

    let whileM mBool m =
        mLoopHelper mBool ((=) true) m []

    let takeWhileM mFunc m =
        let rec iterateWhileRec mFunc m lst =
            m >>= (fun res ->
                mFunc res >>= (fun pred ->
                    if pred = true then
                        iterateWhileRec mFunc m (res::lst)
                    else return' (List.rev lst)))
        iterateWhileRec mFunc m []

    /// Runs the IO actions and evaluates the result
    let rec run io =
        match io with
        |Return a -> IOResult.return' a
        |ConsoleWrite (fmt, str, a) -> // Write to the console
            let print = IOResult.withExceptionCheck (fun _ -> printf fmt str) ()
            run a
        |ConsoleWriteLine (fmt, str, a) -> // Write a line to the console
            let print = IOResult.withExceptionCheck (fun _ -> printfn fmt str) ()
            run a
        |ConsoleReadKey (g) -> // Read a key from the console
            let consoleKey = IOResult.withExceptionCheck (fun _ -> System.Console.ReadKey()) ()
            IOResult.bind consoleKey (fun key -> run (g key))
        |ConsoleReadLine (g) -> // Read a line from the console
            let consoleLine = IOResult.withExceptionCheck (fun _ -> System.Console.ReadLine()) ()
            IOResult.bind consoleLine (fun line -> run (g line))
        |FileReadLines (fName, g) -> // Read lines lazily from file
            let fileLines = IOResult.withExceptionCheck (fun _ -> File.ReadLines(fName.PathString)) ()
            IOResult.bind fileLines (fun lns -> run (g lns))
        |ReadAllBytes (fName, g) -> // read all bytes from a file
            let fileBytes = IOResult.withExceptionCheck (fun _ -> File.ReadAllBytes(fName.PathString)) ()
            IOResult.bind fileBytes (fun lns -> run (g lns))
        |OpenFileHandle (fName, mode, g) -> // open a file handle to a file
            let ioHandle = IOResult.withExceptionCheck (fun _ -> 
                {TextReader = new StreamReader(new FileStream(fName.PathString, mode)) :> TextReader |> Some; TextWriter = None}) ()
            IOResult.bind ioHandle (fun hand -> run (g hand))
        |StartTCPListener (ip, port, g) -> // start a TCP listener on the specified ip and port
            let tcpServ = IOResult.withExceptionCheck (fun _ ->
                let listener = Sockets.TcpListener(ip, port)
                listener.Start()
                {TCPListener = listener}) ()
            IOResult.bind tcpServ (fun serv -> run (g serv))
        |TCPServerAccept (serv, g) -> // accept a tcp connection on the specified server
            let ioSock = IOResult.withExceptionCheck (fun _ ->
                let socket = serv.TCPListener.AcceptSocket()
                {TCPConnectedSocket = socket}) ()
            IOResult.bind ioSock (fun conSock -> run (g conSock))
        |HGetLine (handle, g) -> // get a line from the handle
            match handle.TextReader with
            |Some txtRdr ->
                let ioLine = IOResult.withExceptionCheck (fun _ -> txtRdr.ReadLine()) ()
                IOResult.bind ioLine (fun line -> run (g line))
            |None -> IOError (StreamStateUnsupported "Stream does not support reading")
        |HPutStringLine (handle, str, a) -> // write a line to the handle
            match handle.TextWriter with
            |Some txtWrtr ->
                let ioWrite = IOResult.withExceptionCheck (fun _ -> 
                    txtWrtr.WriteLine str
                    txtWrtr.Flush()) ()
                IOResult.bind ioWrite (fun () -> run a)
            |None -> IOError (StreamStateUnsupported "Stream does not support writing")
        |ConnectedSocketToHandle (socket, g) -> // convert a connected socket to a handle
            let ioHandle = IOResult.withExceptionCheck (fun _ -> 
                {TextReader = new StreamReader(new Sockets.NetworkStream(socket.TCPConnectedSocket)) :> TextReader |> Some;
                 TextWriter = new StreamWriter(new Sockets.NetworkStream(socket.TCPConnectedSocket)) :> TextWriter |> Some }) ()
            IOResult.bind ioHandle (fun hand -> run (g hand))
        |IsReady (handle, g) -> // determine if a handle has available input
            match handle.TextReader with
            |Some txtRdr ->
                let ioEnd = IOResult.withExceptionCheck (fun _ -> txtRdr.Peek() = -1) ()
                IOResult.bind ioEnd (fun line -> run (g line))
            |None -> IOError (StreamStateUnsupported "Stream does not support reading")
            
    /// Reads a line from the file or channel
    let hGetLine handle =
        HGetLine(handle, return')
    /// Determines if the handle has data available
    let hIsReady handle =
        IsReady(handle, return')
    /// Writes a line to the final or channel
    let hPutStrLn handle str =
        HPutStringLine (handle, str, return' ())

/// Console functions
module Console =
    /// print a string to the console using the supplied formatter
    let printf fmt str = 
        ConsoleWrite (fmt, str, IO.return' ())
    /// print a line to the console using the supplied formatter
    let printfn fmt str = 
        ConsoleWriteLine (fmt, str, IO.return' ())
    /// read a line from the console
    let readLine =
        ConsoleReadLine(IO.return')

module TCP =
    /// Create a TCP server at the specfied IP on the specified port
    let createServer ip port =
        StartTCPListener (ip, port, IO.return')
    /// Accept a connection from the supplied TCP server
    let acceptConnection serv =
         TCPServerAccept (serv, IO.return')
    /// Convert a socket to a handle
    let socketToHandle socket =
        ConnectedSocketToHandle (socket, IO.return')

type IOBuilder() =
    member this.Return a = IO.return' a
    member this.ReturnFrom a = a
    member this.Bind (x, f) = IO.bind x f

[<AutoOpen>]
module IOBuilders =
    let io = IOBuilder()
            

