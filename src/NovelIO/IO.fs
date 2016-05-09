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

/// A value of type IO<'a> represents an action which, when performed (e.g. by calling the IO.run function), does some I/O which results in a value of type 'a.
type IO<'a> = 
    private 
    |Return of 'a
    |Delay of (unit -> IO<'a>)

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
    /// Return a value as an IO action
    let return' x = Return x
    /// Creates an IO action from an effectful computation, this simply takes a side effecting function and brings it into IO
    let fromEffectful f = Delay (return' << f)
    /// Monadic bind for IO action, this is used to combine and sequence IO actions
    let rec bind x f =
        match x with
        |Return a -> f a
        |Delay (g) -> Delay (fun _ -> bind (g ()) f)

    /// Computation Expression builder for IO actions
    type IOBuilder() =
        /// Return a value as an IO action
        member this.Return a : IO<'a> = return' a
        /// Bare return for IO values
        member this.ReturnFrom a : IO<'a> = a
        /// Monadic bind for IO action, this is used to combine and sequence IO action
        member this.Bind (x : IO<'a>, f : 'a -> IO<'b>) = bind x f
        /// Delays a function of type unit -> IO<'a> as an IO<'a>
        member this.Delay f : IO<'a> = Delay f
        /// Combine an IO action of type unit an IO action of type 'a into a combined IO action of type 'a
        member this.Combine(f1, f2) =
            bind f1 (fun () -> f2)
        /// The zero IO action
        member this.Zero() = return' ()
        /// Definition of while loops within IO computation expressions
        member this.While(guard, body) =
            match guard() with
            |false -> this.Zero()
            |true -> bind (body) (fun () -> this.While(guard, body))

    let private io = IOBuilder()
    /// Monadic bind operator for IO actions
    let (>>=) x f = bind x f
    /// Left to right Kleisli composition of IO actions, allows composition of binding functions
    let (>=>) f g x = f x >>= g
    /// Right to left Kleisli composition of IO actions, allows composition of binding functions
    let (<=<) f g x = flip (>=>) f g x
    /// Takes a function which transforms a value to another value and an IO action which produces 
    /// the first value, producing a new IO action which produces the second value
    let map f x = x >>= (return' << f)
    /// Map operator for IO actions
    let (<!>) f x = map f x
    /// Takes an IO action which produces a function that maps from a value to another value and an IO action
    /// which produces the first value, producing a new IO action which produces the second value.  This is like 
    /// map but the mapping function is contained within IO.
    let apply (f : IO<'a -> 'b>) (x : IO<'a>) =
        f >>= (fun fe -> map fe x)
    /// Apply operator for IO actions
    let (<*>) (f : IO<'a -> 'b>) (x : IO<'a>) = apply f x
    /// Removes a level of IO structure
    let join x = x >>= id

    // ----- GENERAL ----- //
            
    /// Reads a line from the file or channel
    let hGetLine handle = fromEffectful (fun _ -> SideEffectingIO.hGetLine handle)
    /// Determines if the handle has data available
    let hIsReady handle = fromEffectful (fun _ -> SideEffectingIO.isHandleReadyToRead handle)
    /// Writes a line to the final or channel
    let hPutStrLn handle str = fromEffectful (fun _ -> SideEffectingIO.hPutStrLn str handle)
    /// Writes a line to console
    let putStrLn (str : string) = fromEffectful (fun _ -> System.Console.WriteLine str)

    // ------- RUN ------- //

    /// Runs the IO actions and evaluates the result
    let run io =
        let rec runRec (io : IO<'a>) =
            match io with
            |Return a -> a            
            |Delay (a) -> runRec <| a()
        runRec io

    /// Runs the IO actions and evaluates the result, handling success or failure using IOResult
    let runGuarded io =
        // run recursively and handle exceptions in IO
        IOResult.withExceptionCheck (run) io

    /// Sparks off a new thread to run the IO action passed as the first argument
    let forkIO io = 
        fromEffectful (fun _ -> System.Threading.Tasks.Task.Factory.StartNew(fun () -> run io) |> ignore)

    /// Map each element of a list to a monadic action, evaluate these actions from left to right and collect the results as a sequence.
    let mapM mFunc sequ =
        fromEffectful (fun _ ->
            sequ
            |> Seq.map (run << mFunc)
            |> List.ofSeq
            |> Seq.ofList)

    /// Map each element of a list to a monadic action of options, evaluate these actions from left to right and collect the results which are 'Some' as a sequence.
    let chooseM mFunc sequ =
        fromEffectful (fun _ ->
            sequ
            |> Seq.choose (run << mFunc)
            |> List.ofSeq
            |> Seq.ofList)

    /// Filters a sequence based upon a monadic predicate, collecting the results as a sequence
    let filterM pred sequ =
        fromEffectful (fun _ ->
            sequ
            |> Seq.filter (run << pred)
            |> List.ofSeq
            |> Seq.ofList)

    /// As mapM but ignores the result.
    let iterM mFunc sequ =
        fromEffectful (fun _ ->
            sequ
            |> Seq.iter (ignore << run << mFunc))

    /// Analogous to fold, except that the results are encapsulated within IO
    let foldM accFunc acc sequ =
        fromEffectful (fun _ ->
            Seq.fold (fun acc it -> run <| accFunc acc it) acc sequ)

    /// Evaluate each action in the sequence from left to right and collect the results as a sequence.
    let sequence seq =
        mapM id seq

    /// Performs the action mFunc n times, gathering the results.
    let replicateM mFunc n =
        sequence (Seq.init n (fun _ -> mFunc))

    /// As replicateM but ignores the results
    let repeatM mFunc n  =
        replicateM mFunc n >>= (return' << ignore)

    /// IOBuilder extensions so that traverseM_ can be used to define For
    type IOBuilder with
        /// Definition of for loops within IO computation expressions
        member this.For (sequence : seq<_>, body) =
            iterM body sequence

    // ------ LOOPS ------ //

    /// IO looping constructs
    module Loops =
        /// Take elements repeatedly while a condition is met
        let takeWhileM p xs =
            fromEffectful  (fun _ ->
                xs 
                |> Seq.takeWhile p
                |> Seq.map (run)
                |> List.ofSeq
                |> Seq.ofList)

        /// Drop elements repeatedly while a condition is met
        let skipWhileM p xs =
            fromEffectful (fun _ ->
                xs 
                |> Seq.skipWhile p
                |> Seq.map (run)
                |> List.ofSeq
                |> Seq.ofList)

        /// Execute an action repeatedly as long as the given boolean IO action returns true
        let whileM (pAct : IO<bool>) (f : IO<'a>) =
            fromEffectful (fun _ ->
                Seq.initInfinite (fun _ -> f)
                |> Seq.map (run)
                |> Seq.takeWhile (fun _ -> run pAct)
                |> List.ofSeq
                |> Seq.ofList)

        /// As long as the supplied "Maybe" expression returns "Some _", each element will be bound using the value contained in the 'Some'.
        /// Results are collected into a sequence.
        let whileSome act binder =
            fromEffectful (fun _ ->
                Seq.initInfinite (fun _ -> run act)
                |> Seq.takeWhile (Option.isSome)
                |> Seq.map (run << binder << Option.get)
                |> List.ofSeq
                |> Seq.ofList)

        /// Yields the result of applying f until p holds.
        let rec iterateUntilM p f v =
            match p v with
            |true -> return' v
            |false -> f v >>= iterateUntilM p f

        /// Execute an action repeatedly until its result satisfies a predicate and return that result (discarding all others).
        let iterateUntil p x = x >>= iterateUntilM p (const' x)

        /// Execute an action repeatedly until its result fails to satisfy a predicate and return that result (discarding all others).
        let iterateWhile p x = iterateUntil (not << p) x

        /// Repeatedly evaluates the second argument while the value satisfies the given predicate, and returns a list of all
        /// values that satisfied the predicate.  Discards the final one (which failed the predicate).
        let unfoldWhileM p (f : IO<'a>) =
            fromEffectful (fun _ ->
                Seq.initInfinite (fun _ -> f)
                |> Seq.map (run)
                |> Seq.takeWhile p
                |> List.ofSeq
                |> Seq.ofList)

/// Console functions
module Console =
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
            

