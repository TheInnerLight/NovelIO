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
* An IO action that opens a TCP connection has type `IO<TCPConnectedSocket>`.

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

If we run these examples, we can note the different behaviour.

In the first example `exmpl` represents the result of the user input from the console, we perform that side effect only once and print the same value to the console twice.

In the second example `exmpl2` represents the action of reading user input from the console and running it gives us the result.  Hence, in this case, the user is prompted for input twice and potentially different results are printed.

`IO.run` is the only non-referentially transparent function exposed to the user in this library and is equivalent to `unsafePerformIO` in Haskell.  Since F# is fundamentally still an impure language, it is up to the developer how often they wish to make use of it.

It is possible (albeit certainly not recommended!) to call `IO.run` on every small block of IO code.  It is also possible to call `IO.run` only once, in the main function, for an entire program: it is then possible to visualise the running of a program as the only effectful part of otherwise pure, referentially transparent code.

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

Needless to say, highly complex actions can be built up in this way.  For example, running a webserver could be represented as a single `IO` action.

## Parallel IO

IO actions can also be performed in parallel using the `IO.parallel` combinators.  This gives us very explicit, fine-grained, control over what actions should take place in parallel.

In order to execute items in parallel, we can simply build a list of the IO actions we wish to perform and use the `par` combinator.  For example:
*)

io {
    let fName = File.assumeValidFilename "file.txt"
    let! channel = File.openTextChannel FileMode.Open FileAccess.Read fName
    return IO.Parallel.par [Console.readLine; TextChannel.getLine channel]
} |> IO.run

(**

This describes a program that gets a line from the console and a line from a specified file in parallel and returns them in a list of strings.

## Bringing impure functions into IO

It's very likely that the set of functions included in this library will not cover every possible IO action we might ever wish to perform.  In this case, we can use the `IO.fromEffectful` to take a non-referentially transparent function and bring it within IO.

If we decide to create an action that exits the program, this could be accomplished as follows:

*)

let exit = IO.fromEffectful (fun _ -> System.Environment.Exit 0)

(**

This should allow us to construct arbitrary programs entirely within IO.

*)