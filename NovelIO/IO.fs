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

    type IOBuilder() =
        member this.Return a = return' a
        member this.ReturnFrom a = a
        member this.Bind (x, f) = bind x f

    let private io = IOBuilder()
    /// Monadic bind operator for IO values
    let (>>=) x f = bind x f
    /// Left to right Kleisli composition of IO values
    let (>=>) f g x = f x >>= g
    /// Right to left Kleisli composition of IO values
    let (<=<) f g x = flip (>=>) f g x
    /// Map function for IO Values
    let map f x = x >>= (return' << f)
    /// Map each element of a list to a monadic action, evaluate these actions from left to right and collect the results.
    let mapM mFunc list =
        let folder head tail =
            io {
                let! h = mFunc head
                let! t = tail
                return (h::t)
            }
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

    /// Runs the IO actions and evaluates the result
    let run io =
        let rec runRec io =
            match io with
            |Return a -> a
            |ConsoleWrite (fmt, str, a) -> // Write to the console
                printf fmt str
                runRec a
            |ConsoleWriteLine (fmt, str, a) -> // Write a line to the console
                printfn fmt str
                runRec a
            |ConsoleReadKey (g) -> // Read a key from the console
                let consoleKey = System.Console.ReadKey()
                runRec <| g consoleKey
            |ConsoleReadLine (g) -> // Read a line from the console
                let consoleLine = System.Console.ReadLine()
                runRec <| g consoleLine
            |FileReadLines (fName, g) -> // Read lines lazily from file
                let fileLines = File.ReadLines(fName.PathString)
                runRec <| g fileLines
            |ReadAllBytes (fName, g) -> // read all bytes from a file
                let fileBytes = File.ReadAllBytes(fName.PathString)
                runRec <| g fileBytes
            |OpenFileHandle (fName, mode, g) -> // open a file handle to a file
                let ioHandle = 
                    {TextReader = new StreamReader(new FileStream(fName.PathString, mode)) :> TextReader |> Some; TextWriter = None}
                runRec <| g ioHandle
            |StartTCPListener (ip, port, g) -> // start a TCP listener on the specified ip and port
                let listener = Sockets.TcpListener(ip, port)
                listener.Start()
                let tcpServ = {TCPListener = listener}
                runRec <| g tcpServ
            |TCPServerAccept (serv, g) -> // accept a tcp connection on the specified server
                let socket = serv.TCPListener.AcceptSocket()
                let ioSock = {TCPConnectedSocket = socket}
                runRec <| g ioSock
            |HGetLine (handle, g) -> // get a line from the handle
                match handle.TextReader with
                |Some txtRdr ->
                    let line = txtRdr.ReadLine()
                    runRec <| g line
                |None -> raise HandleDoesNotSupportReadingException
            |HPutStringLine (handle, str, a) -> // write a line to the handle
                match handle.TextWriter with
                |Some txtWrtr ->
                    txtWrtr.WriteLine str
                    txtWrtr.Flush()
                    runRec a
                |None -> raise HandleDoesNotSupportWritingException
            |ConnectedSocketToHandle (socket, g) -> // convert a connected socket to a handle
                let hand = 
                    {TextReader = new StreamReader(new Sockets.NetworkStream(socket.TCPConnectedSocket)) :> TextReader |> Some;
                     TextWriter = new StreamWriter(new Sockets.NetworkStream(socket.TCPConnectedSocket)) :> TextWriter |> Some }
                runRec<| g hand
            |IsReady (handle, g) -> // determine if a handle has available input
                match handle.TextReader with
                |Some txtRdr ->
                    let endS = txtRdr.Peek() = -1
                    runRec <| g endS
                |None -> raise HandleDoesNotSupportReadingException
        IOResult.withExceptionCheck (runRec) io
            
    /// Reads a line from the file or channel
    let hGetLine handle =
        HGetLine(handle, return')
    /// Determines if the handle has data available
    let hIsReady handle =
        IsReady(handle, return')
    /// Writes a line to the final or channel
    let hPutStrLn handle str =
        HPutStringLine (handle, str, return' ())

    // ------ LOOPS ------ //

    /// IO looping constructs
    module Loops =
        /// Take elements repeatedly while a condition is met
        let rec takeWhileM p xs =
            match xs with
            |[] -> return' []
            |x::xs ->
                io {
                    let! v = x
                    let! q = p v
                    if q then 
                        let! res = (takeWhileM p xs) 
                        return (v::res)
                    else return []
                }
        /// Drop elements repeatedly while a condition is met
        let rec dropWhileM p xs =
            match xs with
            |[] -> return' []
            |x::xs ->
                io {
                    let! v = x
                    let! q = p v
                    if q then return! dropWhileM p xs
                    else return! sequence (x::xs)
                }
        /// Execute an action repeatedly as long as the given boolean expression returns true
        let rec whileM p f =
            io {
                let! x = p
                let! x2 = f
                let! xs = whileM p f
                return x2 :: xs
            }
        /// As long as the supplied "Maybe" expression returns "Some _", the loopbody will be called and passed the value contained in the 'Some'.
        /// Results are collected into a list.
        let rec whileSome p f =
            io {
                let! x = p
                match x with
                |None -> return []
                |Some x ->
                    let! x = f x
                    let! xs = whileSome p f
                    return x :: xs
            }

        /// Yields the result of applying f until p holds.
        let rec iterateUntilM p f v =
            match p v with
            |true -> return' v
            |false -> f v >>= iterateUntilM p f

        /// Execute an action repeatedly until its result satisfies a predicate and return that result (discarding all others).
        let iterateUntil p x = x >>= iterateUntilM p (const' x)
        /// Execute an action repeatedly until its result fails to satisfy a predicate and return that result (discarding all others).
        let iterateWhile p x = iterateUntil (not << p) x

        /// Repeatedly evaluates the second argument until the value satisfies the given predicate, and returns a list of all
        /// values that satisfied the predicate.  Discards the final one (which failed the predicate).
        let rec unfoldWhileM p m =
            io {
                let! x = m
                if p x then return! unfoldWhileM p m
                else return []
             }

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



[<AutoOpen>]
module IOBuilders =
    let io = IO.IOBuilder()
            

