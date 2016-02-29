(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "NovelIO/NovelIO.dll"
open NovelFS.NovelIO

(**
#NovelIO
======================

## Introduction

NovelIO is a library designed to bring the explicit safety and robustness of Haskell's IO monad to the .NET framework. The result is a purely functional approach to describing I/O operations whereby the evaluation of functions do not produce side-effects, rather they describe sequenced operations which can later be evaluated.

Much like in Haskell, we introduce the `IO<'a>` type which represents some action that, when performed successfully, returns some result `'a.`  Here are some examples:

* An IO action that prints the string "Hello World" to the screen has type `IO<unit>`.
* An IO action that gets a line of text from the Console has type `IO<string>`.
* An IO action that opens a TCP connection to google.com on port 80 has type `IO<TCPConnectedSocket>`.

Values of type `IO<'a>` are distinct from traditional values in that they do not represent the result of some side effect, they rather represent an action that can be `run` to produce a particular result.

## Running IO Actions

`IO<'a>` Actions can be `run` using the `IO.run` function.  This results in all of the side-effects being evaluated, resulting in something of type `IOResult<'a>`.

`IOResult<'a>` is a Discriminated Union defined as follows:


    type IOResult<'a> =
        |IOSuccess of 'a
        |IOError of IOErrorResult

Thus, our IO action can either be successful, returning the desired result of type `'a` or it can fail, returning some error.

## Sequencing IO Actions

It is possible to sequence I/O operations using the `io` computation expression (very similar to the do notation found in Haskell).

*)

io {
    let! l1 = Console.readLine
    let! l2 = Console.readLine
    let! l3 = Console.readLine
    do! Console.printf "You entered: %A" [l1; l2; l3]
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
    let! floatLines = IO.traverseM (IO.map float) lines // traverse the `seq<io<string>>` to an `io<seq<float>>` by parsing
    do! IO.traverseM_ (Console.printfn "%f") floatLines // print each float to the console
}

match IO.run fileIO with // side effects occur *only* on this line
|IOSuccess _ -> () // success case
|IOError err -> () // error case


(**

This code describes exactly the same problem but we know that side-effects can occur in exactly one place `IO.run`, in practise this means that no side-effects can 'escape' the `io` computation expression.

*)