(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "NovelIO/NovelIO.dll"
open NovelFS.NovelIO

(**
Motivation
======================

## Referential Transparency

In functional programming, referential transparency is a very useful property, an expression which is referentially transparent can be replaced by its value without affecting the behaviour of the program.

Consider:

*)

let x = 2*2

(**

The value `x`, `4` and `2*2` are all completely interchangeable, any of these descriptions anywhere in our program are all equally valid and do not change its behaviour.

Now consider:

*)

let rnd = System.Random()
let y = rnd.Next()

(**

The value `y` does not have this property.  We cannot freely interchange `y` and `rnd.Next()` without changing the behaviour of the program, this is because `rnd.Next` is not referentially transparent.

Referential transparency, however, is a very useful property, all functions in mathematics are referentially transparent and this helps us to reason about the behaviours of equations.  In computer science, this is no different, referential transparency makes it easier to prove correctness and to avoid bugs (especially in concurrent and parallel code).

If we express this logic again using `IO`, we can restore referential transparency:

*)

let yPure = Random.nextIO

(**

Once again, we can freely replace `yPure` and `Random.nextIO` wherever they appear in our program.  They both have precisely the same meaning, namely: an action which, when run, gets the next number from the global random number generator.

As mentioned in the introduction, `IO.run` is the only non-referentially transparent function exposed by this library and, as such, should be used sparingly!

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

If we express this program using this library, the effects are clearly apparent:

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

let randomSeq = Seq.init 4 (fun _ -> rnd.Next())
let sortedSeq = Seq.sort randomSeq

printfn "Sorted: %A" sortedSeq
printfn "Random: %A" randomSeq

(**

Let's at the results of an example run of this program:

> Sorted: seq [42595606; 980900814; 1328311795; 1497661916]
> Random: seq [308839936; 1514073672; 36105878; 741971034]

While this program appears to generate one sequence, sort it, then print the sorted and unsorted result - that isn't what it actually does.  What it actually does is effectively define two random sequence generators, one of which is sorted and the other is not.

Each time we enumerate `randomSeq` or `sortedSeq`, the original side effect of getting random numbers is produced again and again!

Here is the original program we desired to write using NovelIO.

*)

io {
    let randomSeqIO = IO.replicateM (Random.nextIO) 4
    let! randomSeq = randomSeqIO
    let sortedSeq = Seq.sort randomSeq
    do! IO.putStrLn <| sprintf "Sorted: %A" sortedSeq
    do! IO.putStrLn <| sprintf "Random: %A" randomSeq
    } |> IO.run

(**

> Sorted: seq [75121301; 124930198; 609009994; 824551074]
> Random: [824551074; 609009994; 75121301; 124930198]

Notice that now, both sequences contain the same values.  The generation of actual random numbers is triggered by the line `let! randomSeq = randomSeqIO` which makes the effect completely explicit.

In order to get our program to behave like the original one that uses a sequence with side effects, we have to explicitly ask for a second set of evaluated effects.

*)

io {
    let randomSeqIO = IO.replicateM (Random.nextIO) 4
    let! randomSeq = randomSeqIO
    let! randomSeq2 = randomSeqIO // evaluate the effects of randomSeqIO again
    let sortedSeq = Seq.sort randomSeq2
    do! IO.putStrLn <| sprintf "Sorted: %A" sortedSeq
    do! IO.putStrLn <| sprintf "Random: %A" randomSeq
    } |> IO.run

(**

> Sorted: seq [79034179; 1625119183; 1651455963; 1775638512]
> Random: [1801985798; 963004958; 1819358047; 292397249]

Hopefully this demonstrates how being explicit about when side-effects occur can massively improve the ability of developers to understand and reason about their code.

*)
