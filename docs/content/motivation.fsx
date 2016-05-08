(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "NovelIO/NovelIO.dll"
open NovelFS.NovelIO

(**
Motivation
======================

## Lazy evaluation and exceptions

This example is more or less taken from Erik Meijer's Curse of the excluded middle (https://queue.acm.org/detail.cfm?ref=rss&id=2611829)

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

## Side-effects and lazy evaluation

In general, writing code that combines side effects and lazy evaluation can be complex and error prone, the developer can often be left with little idea when effects will actually be triggered.

Consider a program that gets some keystrokes:
*)

let keys = Seq.initInfinite (fun _ -> System.Console.ReadKey())

let untilEnter = 
    keys |> Seq.takeWhile (fun ki -> ki.Key <> System.ConsoleKey.Enter)

printfn "Length: %d" (Seq.length untilEnter)

untilEnter
|> Seq.map(fun ki -> string <| ki.KeyChar)
|> String.concat ""
|> printfn "String: %s"

(**

At first glance, this program might appear to record key strokes until the user presses 'Enter', print the length and then print the result.  Of course, it does not.

In reality, this program counts key strokes until the user presses 'Enter' and prints this length, then it records key strokes again until the user presses 'Enter' and prints the result.

If we express this program using this library, the side effects are clearly apparent:

*)

let keysIO = 
    Console.readKey 
    |> IO.Loops.unfoldWhileM(fun ki -> ki.Key <> System.ConsoleKey.Enter)

(**
We can then choose to perform the action twice (as before):
*)

io {
    let! sequence1 = keysIO
    do! IO.putStrLn <| sprintf "Length: %d" (Seq.length sequence1)
    let! sequence2 = keysIO
    do! sequence2
        |> Seq.map(fun ki -> string <| ki.KeyChar)
        |> String.concat ""
        |> sprintf "String: %s"
        |> IO.putStrLn
} |> IO.run

(**
Or execute the action only once:
*)

io {
    let! sequence = keysIO
    do! IO.putStrLn <| sprintf "Length: %d" (Seq.length sequence)
    do! sequence
        |> Seq.map(fun ki -> string <| ki.KeyChar)
        |> String.concat ""
        |> sprintf "String: %s"
        |> IO.putStrLn
} |> IO.run

(**
In both cases, we have successfully made our decision explicit.

## Random numbers

Sequences of random numbers are another area where side-effects can be particularly devastating while being produced by code that looks innocuous.

Consider this code:

*)

let rnd = System.Random()

let randomSeq = Seq.init 20 (fun _ -> rnd.Next())
let sortedSeq = Seq.sort randomSeq

printfn "Sorted: %A" sortedSeq
printfn "Random: %A" randomSeq

(**

Indeed, the numbers shown in the 'Sorted' and 'Random' lists could be totally different.  Each time we enumerate **randomSeq**, the side effect of getting the next random number is produced again!

Here is the same program written using NovelIO.  Notice that we have to explicitly ask for a second sequence.

*)

io {
    let randomSeqIO = IO.replicateM (Random.nextIO) 20
    let! randomSeq = randomSeqIO
    let! randomSeq2 = randomSeqIO
    let sortedSeq = Seq.sort randomSeq2
    do! IO.putStrLn <| sprintf "Sorted: %A" sortedSeq
    do! IO.putStrLn <| sprintf "Random: %A" randomSeq
    } |> IO.run

(**

If we do not ask for the second sequence, we get what was the original desired behaviour of the program:

*)

io {
    let randomSeqIO = IO.replicateM (Random.nextIO) 20
    let! randomSeq = randomSeqIO
    let sortedSeq = Seq.sort randomSeq
    do! IO.putStrLn <| sprintf "Sorted: %A" sortedSeq
    do! IO.putStrLn <| sprintf "Random: %A" randomSeq
    } |> IO.run

(**

Hopefully this demonstrates how being explicit about when side-effects occur can massively improve the ability of developers to understand and reason about their code.

*)
