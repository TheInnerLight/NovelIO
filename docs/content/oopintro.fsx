(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "NovelIO/NovelIO.dll"
open NovelFS.NovelIO
open IO.Operators

(**
Introduction from an Object-Oriented Perspective
======================

For those coming from an OOP background, the purpose of purely functional I/O might not seem immediately apparent but in many cases, what would be regarded as "good practise" in the object oriented world is something we can simply have by construction using referentially transparent IO.

## Inversion of Control

In the OO world, you might design an interface to retrieve some resource.  Alongside it, you might design a processing class with that interface as an injected dependency.  You call a method on that interface in order to retrieve the data to process.
One implementation of that interface might touch the file system and then a mock implementation would return test data for unit testing purposes.

A simple example might look something like this:

*)

type IResourceSupplier =
    abstract member GetList : unit -> int list

(**

You can then pass the `IResourceSupplier` implementation to its consumers.

Imagine we wished to add one to every number retrieved by our resource supplier so we decide to use a constructor injected dependency.

*)

type Adder(supplier : IResourceSupplier) =
    member this.AddOne = List.map ((+) 1) (supplier.GetList())


(**

This class is now pretty easy to test, we just use a custom `IResourceSupplier` implementation and we can test the logic that the `Adder` class performs in isolation of its dependency.

Of course, we've had to add extra boilerplate to do this and this is just a trivial example.  I think it's also fair to say that those uninitiated to the world of object oriented programming probably wouldn't start by structuring their code in this way.
Logically and intuitively, it makes more sense to think of getting a resource and passing it to a processing function.

We can actually maintain both the logical ordering and the testability by simply lifting a pure processing function into IO.

*)

// this pure function works on a standard lists, it's trivial to unit test
let addOneToList lst = List.map ((+) 1) lst 

// this function is just the above function made to operate on lists in IO using map.
let addOneToIOList lstIO = IO.map (addOneToList) lstIO

// alternatively, we can write the above in operator form:
let addOneToIOList' lstIO = addOneToList <!> lstIO

(**

`IO.map` can take any function of the form `'a -> 'b` and return an `IO<'a> -> IO<'b>`, allowing our previously pure function to easily operate within IO.

This approach is simple, practical and retains all of the testability of the Inversion of Control based approach.

We can also see through the type system which function performs IO and which does not.  That's a massive win for both readability and maintenance!

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

CSQ has a laudable objective, to make it easier to reason about the way code behaves.

Any time we see `unit` we know that an effect is happening and any time we see a value returned, we know we have no side-effects.  Unfortunately, this pattern forbids common patterns like `random.Next()` which are ubiquitous in OO language standard library APIs.

Now let's express these using NovelIO:

*)

let exampleIO = Console.writeLine "Hello World!"

let query x = pown x 3

(**

This looks very much the same as what we had before, exampleCommand is now of type `IO<unit>` instead of `unit -> unit`.  But now lets look at the random example that we couldn't solve neatly using CQS:

*)

let randomIO = Random.nextIO

(**

`randomIO` here has type `IO<int>`.  That provides a strong and clear distinction from the type of `query` which has type `int -> int`.

You can therefore think of referentially transparent IO as a more powerful version of CQS.

*)