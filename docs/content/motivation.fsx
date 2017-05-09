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

let x = 2 * 2

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
    do! Console.writeLine <| sprintf "Length: %d" (Seq.length sequence1)
    let! sequence2 = keysIO
    do! sequence2
        |> Seq.map(fun ki -> string <| ki.KeyChar)
        |> String.concat ""
        |> sprintf "String: %s"
        |> Console.writeLine
} |> IO.run

(**
Or execute the action only once:
*)

io {
    let! sequence = keysIO
    do! Console.writeLine <| sprintf "Length: %d" (Seq.length sequence)
    do! sequence
        |> Seq.map(fun ki -> string <| ki.KeyChar)
        |> String.concat ""
        |> sprintf "String: %s"
        |> Console.writeLine
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
    do! Console.writeLine <| sprintf "Sorted: %A" sortedSeq
    do! Console.writeLine <| sprintf "Random: %A" randomSeq
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
    let sortedSeq = Seq.sort randomSeq // sort the first set
    let! randomSeq2 = randomSeqIO // evaluate the effects of randomSeqIO again
    do! Console.writeLine<| sprintf "Sorted: %A" sortedSeq
    do! Console.writeLine <| sprintf "Random: %A" randomSeq2
    } |> IO.run

(**

> Sorted: seq [79034179; 1625119183; 1651455963; 1775638512]
> Random: [1801985798; 963004958; 1819358047; 292397249]

So, using this approach we can easily describe either behaviour while still keeping the intent clear and explicit.

*)

