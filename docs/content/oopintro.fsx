(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "NovelIO/NovelIO.dll"
open NovelFS.NovelIO

(**
## Introduction from an Object-Oriented Perspective

For those coming from an OOP background, the purpose of purely functional I/O might not seem immediately apparent.  Two principles that might be familiar to developers more generally are:

* Command–query separation
* The Principle of least astonishment

## Command-Query Seperation

Command-query seperation (CQS) is an imperative programming principle that says that each method should either be a Command or a Query.

* Commands perform side effects but return no data, in F# terms they return `unit`.
* Queries return data to the caller, they must be referentially transparent (i.e. possess no side-effects).

*)

    type ExampleClass =
        /// Performs the side effect of writing text to the screen
        member this.Command() = printfn "Hello World!"
        /// Pure function that raises x to the power of 3
        member this.Query x = pown x 3 

(**
## The Principle of Least Astonishment

The principle of least astonishment is more nebulously designed but it effectively states that the design of your software API should match the mental model of its user.  Design decisions that take people by surprise are damaging because it will invariably result in them using the software incorrectly. 

Command-query seperation helps to avoid astonishment on the part of the developer.  They can see that functions which return `unit` do some side effect.

The effect of CQS on queries is even more significant.  If we don't follow the principle, there is really no way for our API users to see whether or not a function with a return value does some side-effect before returning the value or if the function is referentially transparent by contrast, following the principle means our API consumer knows that queries do not produce side-effects.

## A Better Alternative

While command-query seperation is a way of solving the problem, can we offer a better one?

### Introducing `IO<'a>`

Imagine two possible queries:

1. cube : A pure function that raises x to the power of 3
*)

let cube x = pown x 3

(**
2. readIntFromFile : A function that gets an int from a file.
*)

open NovelFS.NovelIO.BinaryParser

let readIntFromFile file =
    io {
        let! bytes = File.readAllBytes file
        match BinaryParser.run bytes (BinaryParser.parseInt32) with
        |ParseSuccess flt -> return Some flt
        |_ -> return None
    }

(**
It is worth looking at the type signatures of these functions.

`cube` simply has the type signature: `int -> int`

`readFloatFromFile` file, by contrast, has type signature: `Filename -> IO<int option>`

We can now quite clearly see that, even were it not obvious from the name, the second function is different.  The fact that it needs to interact with the file system is now encoded in the type signature.

Hopefully this shows how we can use purely functional IO to make our coder richer but with less risk of astonishing our API users.
*)