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

## Dependency Inversion

In the OO world, you might design an interface to retrieve some resource.  One implementation might touch the file system and then mock implementation would return test data for unit testing purposes.

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

Of course, we've had to add quite a bit of boilerplate to actually get to this point.  

Exactly the same results can be achieved in IO by lifting a pure function into IO.

*)

let addOneToList lst = List.map ((+) 1) lst // this function works on any old list, making it trivial to test

let addOneToIOList lstIO = IO.map (addOneToList) lstIO // this function takes the above function and makes it operate on lists in IO.

(**

`IO.map` can take any function of the form `'a -> 'b` and return an `IO<'a> -> IO<'b>`, allowing our previously pure function to easily operate within IO.

We have gained the same testability advantage as the OO dependency inversion example with less boilerplate required to actually realise it.

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