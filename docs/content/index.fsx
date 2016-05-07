(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "NovelIO/NovelIO.dll"
open NovelFS.NovelIO

(**
Introduction
======================

NovelIO is a library designed to bring the explicit safety and robustness of Haskell's IO monad to the .NET framework. The result is a purely functional approach to describing I/O operations whereby the application of functions does not perform side-effecting computations but rather constructs a data type representing a sequence of actions that can later be executed.

Much like in Haskell, we introduce the `IO<'a>` type which represents some action that, when performed successfully, returns some result `'a.`  Here are some examples:

* An IO action that prints the string "Hello World" to the screen has type `IO<unit>`.
* An IO action that gets a line of text from the Console has type `IO<string>`.
* An IO action that opens a TCP connection to google.com on port 80 has type `IO<TCPConnectedSocket>`.

The IO action can equally represent a sequence of actions:

* An IO action that requests a Name, then that person's Date of Birth from a service might have type `IO<string, DateTime>`

Values of type `IO<'a>` are distinct from traditional values in that they do not represent the result of some side effect, they rather represent an action (or sequence of actions) that can be `run` to produce a particular result.

## Running IO Actions

`IO<'a>` Actions can be `run` using the `IO.run` function.  This results in all of the side-effects being evaluated, resulting in something of type `'a`.

These values can then be re-used and run again to evaluate the side-effects once more.

Consider a standard .NET impure IO example:

*)

let exmpl = System.Console.ReadLine()
printfn "%s" exmpl
printfn "%s" exmpl

(**

And a NovelIO example:

*)

let exmpl2 = io {return! Console.readLine}
printfn "%s" (IO.run exmpl2)
printfn "%s" (IO.run exmpl2)

(**

If you run these examples, you will note the different behaviour.

In the first example `exmpl` represents the result of the user input from the console, we perform that side effect only once and print the same value to the console twice.

In the second example `exmpl` represents the action of reading user input from the console and running it gives us the result.  Hence, in this case, the user is prompted for input twice and potentially different results are printed.

## Sequencing IO Actions

It is possible to sequence I/O operations using the `io` computation expression (very similar to the do notation found in Haskell).

*)

io {
    let! l1 = Console.readLine
    let! l2 = Console.readLine
    let! l3 = Console.readLine
    do! IO.putStrLn <| sprintf "You entered: %A" [l1; l2; l3]
} |> IO.run

(**
Here we simply read three lines from the console and print the result back to the console as a list.

## Motivation

Consider the following code where we try to combine lazy evaluation with File IO:

*)

let floatLines = 
    try
        System.IO.File.ReadLines("testfile.txt")
        |> Seq.map (float) // parse each line as a float
    with
        | _ -> Seq.empty

Seq.iter (printfn "%f") floatLines // print each float to the console

(**

This code appears to be relatively safe - we have a comforting `try`/`with` block around a function that may fail at runtime.  This code, however, does not function in the way it immediately appears to.

In reality, the map is not actually evaluated until we enumerate the sequence with `Seq.iter`, this means that any exception, if triggered, will actually be thrown outside the `try`/`with` block causing the program to crash.

Consider an alternative using NovelIO's expression of IO:

*)

let fName = File.assumeValidFilename "testfile.txt"

let fileIO = io {
    let! lines = File.readLines fName // sequence of io actions which each read a line from a file
    let! floatLines = IO.mapM (IO.map float) lines // parse each line, collecting the results
    do! IO.iterM (IO.putStrLn << sprintf "%f") floatLines // print each float to the console
}

try
    IO.run fileIO // side effects occur *only* on this line
with 
    |_ -> () // error case


(**

This code describes exactly the same problem but we know that side-effects can occur in exactly one place `IO.run`.  That means that success or failure need be handled in only that one location.  We can therefore design complicated programs where IO is described using pure, referentially transparent functions and potentially error-prone behaviour is made very explicit and side-effects are restricted to very specific and obvious locations.

*)