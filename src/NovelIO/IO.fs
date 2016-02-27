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

/// A value of type IO<'a> is a computation which, when performed, does some I/O before returning a value of type 'a.
type IO<'a> = 
    private 
    |Return of 'a
    |Bind of (unit -> IO<'a>)

/// Side effecting helper functions - this is where ugly things happen
module internal SideEffectingIO =
    /// Hidden helper functions
    module private Helpers =
        /// Writes a a string to the console with a supplied function
        let hPutStrF f handle =
            match handle.TextWriter with
            |Some txtWrtr ->
                f txtWrtr
                txtWrtr.Flush()
            |None -> raise HandleDoesNotSupportWritingException
    /// Accept a socket from a TCP Server
    let acceptSocketFromServer serv =
        {TCPConnectedSocket = serv.TCPListener.AcceptSocket()}
    /// Connect a TCP Socket to a specified ip and port
    let connectTCPSocket (ip : IPAddress) (port : int) =
        let sock = new Sockets.Socket(Sockets.SocketType.Stream, Sockets.ProtocolType.Tcp)
        sock.Connect(ip, port)
        {TCPConnectedSocket = sock}
    /// Close a socket
    let closeSocket sock =
        sock.TCPConnectedSocket.Disconnect false
    /// Gets a line from a handle
    let hGetLine handle =
        match handle.TextReader with
        |Some txtRdr -> txtRdr.ReadLine()
        |None -> raise HandleDoesNotSupportReadingException
    /// Writes a string to a handle
    let hPutStr (str : string) handle =
        Helpers.hPutStrF (fun txtWrtr -> txtWrtr.Write str) handle
    /// Writes a string line to a handle
    let hPutStrLn (str : string) handle =
        Helpers.hPutStrF (fun txtWrtr -> txtWrtr.WriteLine str) handle
    /// Determines whether a supplied handle is ready to be read from
    let isHandleReadyToRead handle = 
        match handle.TextReader with
        |Some txtRdr -> txtRdr.Peek() = -1
        |None -> raise HandleDoesNotSupportReadingException
    /// Create a file handle for a supplied file name, file mode and file access
    let openFileHandle (fName : Filename) mode access =
        let crTxtRdr (fStream : FileStream) = new StreamReader(fStream) :> TextReader
        let crTxtWrtr (fStream : FileStream) = new StreamWriter(fStream) :> TextWriter
        let fStream = new FileStream(fName.PathString, mode, access)
        let (reader, writer) =
            match access with
            |FileAccess.Read -> Some <| crTxtRdr fStream, None
            |FileAccess.ReadWrite -> Some <| crTxtRdr fStream, Some <| crTxtWrtr fStream
            |FileAccess.Write -> None, Some <| crTxtWrtr fStream
        {TextReader = reader; TextWriter = writer}
    /// Start a TCP server on a supplied ip address and port
    let startTCPServer ip port =
        let listener = Sockets.TcpListener(ip, port)
        listener.Start()
        {TCPListener = listener}

/// Pure IO Functions
module IO =
    /// Return a value as an IO value
    let return' x = Return x
    /// Creates an IO value from an effectful computation
    let fromEffectful f = Bind (return' << f)
    /// Monadic bind for IO values
    let rec bind x f =
        match x with
        |Return a -> f a
        |Bind (g) -> Bind (fun _ -> bind (g ()) f)

    /// Builder for IO computation expressions
    type IOBuilder() =
        /// Monadic return for IO values
        member this.Return a : IO<'a> = return' a
        /// Bare return for IO Values
        member this.ReturnFrom a : IO<'a> = a
        /// Monadic bind for IO values
        member this.Bind (x : IO<'a>, f : 'a -> IO<'b>) = bind x f
        member this.Delay f : IO<'a> = f()
        member this.Combine(f1, f2) =
            bind f1 (fun () -> f2)
        /// The zero IO value
        member this.Zero() = return' ()
        /// Definition of while loops within IO computation expressions
        member this.While(guard, body) =
            match guard() with
            |false -> this.Zero()
            |true -> bind (body) (fun () -> this.While(guard, body))
        /// Definition of for loops within IO computation expressions
        member this.For (sequence : seq<_>, body) =
            use enum = sequence.GetEnumerator()
            enum.MoveNext() |> ignore
            this.While(enum.MoveNext, 
                this.Delay(fun () -> bind (enum.Current) body ))

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
    let listM list =
        mapM (id) list
    /// Performs the action mFunc n times, gathering the results.
    let replicateM mFunc n =
        listM (List.init n (fun _ -> mFunc))
    /// As replicateM but ignores the results
    let replicateM_ mFunc n  =
        replicateM mFunc n >>= (fun f -> return' ())

    // ----- GENERAL ----- //
            
    /// Reads a line from the file or channel
    let hGetLine handle = fromEffectful (fun _ -> SideEffectingIO.hGetLine handle)
    /// Determines if the handle has data available
    let hIsReady handle = fromEffectful (fun _ -> SideEffectingIO.isHandleReadyToRead handle)
    /// Writes a line to the final or channel
    let hPutStrLn handle str = fromEffectful (fun _ -> SideEffectingIO.hPutStrLn str handle)

    // ------- RUN ------- //

    /// Runs the IO actions and evaluates the result
    let run io =
        let rec runRec (io : IO<'a>) =
            match io with
            |Return a -> a            
            |Bind (a) -> runRec <| a()
        // run recursively and handle exceptions in IO
        IOResult.withExceptionCheck (runRec) io

    /// Sparks off a new thread to run the IO computation passed as the first argument
    let forkIO io = 
        fromEffectful (fun _ -> System.Threading.Tasks.Task.Factory.StartNew(fun () -> run io) |> ignore)

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
                    else return! listM (x::xs)
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
                if p x then
                    let! xs = unfoldWhileM p m
                    return x :: xs
                else return []
             }

/// Console functions
module Console =
    /// print a string to the console using the supplied formatter
    let printf fmt str = IO.fromEffectful (fun () -> Core.Printf.printf fmt str)
    /// print a line to the console using the supplied formatter
    let printfn fmt str = IO.fromEffectful (fun () -> Core.Printf.printfn fmt str)
    /// read a key from the console
    let readKey = IO.fromEffectful (fun () -> System.Console.ReadKey())
    /// read a line from the console
    let readLine = IO.fromEffectful (fun () -> System.Console.ReadLine())

/// Provides purely functional Date/Time functions
module DateTime =
    /// Get the current local time
    let localNow = IO.fromEffectful (fun () -> System.DateTime.Now)
    /// Get the current UTC time
    let utcNow = IO.fromEffectful (fun () -> System.DateTime.UtcNow)

/// Module to provide the definition of the io computation expression
[<AutoOpen>]
module IOBuilders =
    let io = IO.IOBuilder()
            

