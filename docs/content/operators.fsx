(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "NovelIO/NovelIO.dll"
open NovelFS.NovelIO

(**
Operator reference
======================

A set of useful operators are provided in the `IO.Operators` module.

*)

open IO.Operators

(**
## Bind Operator

Bind is the most fundamental way of sequencing IO actions, it takes an IO action and a function that operates on the result of the first IO action and returns a new IO action.

Example function defintion:

*)

let bind x f =
    io {
        let! x' = x
        return! f x'
    }

(**
Example use - reading a line from the console and then writing it back out:
*)

let readWriteLine = bind Console.readLine (fun l -> Console.writeLine l)

(**
Example operator use:
*)

let readWriteLine' = Console.readLine >>= (fun l -> Console.writeLine l)

(**

## Map Operator

The map operator allows you to transform IO actions using an ordinary function.

Example function defintion:

*)

let map f x =
    io {
        let! x' = x
        return f x'
    }

(**
Example use - reading a line from the console and parsing it as an int:
*)

let readIntLine = map (fun l -> int l) Console.readLine

(**
Example operator use:
*)

let readIntLine' = (fun l -> int l) <!> Console.readLine 
let readIntLine'' = int <!> Console.readLine 

(**

## Apply Operator

The apply operator allows functions within `IO` to operate on actions.

Example function defintion:

*)

let apply f x = bind f (fun fe -> map fe x)

(**
Example use - creating a tuple from two strings read from the console:
*)

let readTupleString = apply (map (fun a b -> a,b) Console.readLine) Console.readLine

let readTupleString' = (fun a b -> a,b) <!> Console.readLine <*> Console.readLine

(**
This is a very powerful pattern because the pattern holds for arbitrarily large functions just by adding more uses of `<*>`.
*)

let readTuple2String = (fun a b -> a,b) <!> Console.readLine <*> Console.readLine

let readTuple3String = (fun a b c -> a,b,c) <!> Console.readLine <*> Console.readLine <*> Console.readLine

let readTuple4String = (fun a b c d -> a,b,c,d) <!> Console.readLine <*> Console.readLine <*> Console.readLine <*> Console.readLine

(**
In general, this pattern allows you to take any pure function and apply it as a transformation to the results of IO actions.

## Sequence Actions Operators

The sequence operators `>>.` and `.>>` allow you to perform a pair of actions and return the result from only one of them.

Example function defintion:

*)

let sequenceFirst x y =
    io {
        let! x' = x
        let! y' = y
        return x'
    }

let sequenceSecond x y =
    io {
        let! x' = x
        let! y' = y
        return y'
    }

(**
Example use - writing a fixed string to the console and reading a line:
*)

let readWrite = sequenceFirst Console.readLine (Console.writeLine "Complete.")

let writeRead = sequenceSecond (Console.writeLine "Please enter some input:") Console.readLine

(**
Example operator use:
*)

let readWrite' = Console.readLine .>> (Console.writeLine "Complete.")

let writeRead' = (Console.writeLine "Please enter some input:") >>. Console.readLine


