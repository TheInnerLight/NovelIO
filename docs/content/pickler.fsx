(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "NovelIO/NovelIO.dll"
open NovelFS.NovelIO.BinaryPickler

(**
Pickler Combinators
======================

Pickler combinators are a concept described by Andrew J. Kennedy (http://research.microsoft.com/pubs/64036/picklercombinators.pdf).  
The purpose is to give developers explicit control over their serialisation and deserialiation process while avoiding a tedious and
error prone manual process.

## Pickler primitives
*)

let intPickler = BinaryPickler.pickleInt32
let floatPickler = BinaryPickler.pickleFloat
let asciiPickler = BinaryPickler.pickleAscii

(**

These are some examples of the primitive picklers defined in this library.  These picklers can be used to serialise (pickle) or
deserialise (unpickle) values of the type associated with their pickler to and from their binary representations.

The same picklers can be used to transform data in both directions.

## Running picklers
*)

let bytes = BinaryPickler.pickle (BinaryPickler.pickleInt32) 64

let int = BinaryPickler.unpickle (BinaryPickler.pickleInt32) bytes

(**

Here we use the pickle function to transform the int 64 into its binary representation and then transform it back using
the unpickle function.

## Tuple combinators

A variety of tuple combinators are provided to allow the pickling/unpickling of tuples.  These can be constructed, for example, 
as follows:
*)

let intUtf8Pickler = BinaryPickler.tuple2 BinaryPickler.pickleInt32 BinaryPickler.pickleUTF8

(**

The tuple2 function is simply supplied with two picklers and it constructs a combined pickler.

tuple3 and tuple4 functions are also provided, allowing the construction of more complex picklers.

## Wrapping picklers

Rather than constructing data from tuples, we may wish to pickle/unpickle custom data types.  
It is possible to do this by providing a function which constructs and deconstructs this custom
data-type.

*)

[<Measure>] type GBP
type Product = {ProductName : string; ProductPrice : decimal<GBP>}

let productPickler =
    let nameDecPickler = BinaryPickler.tuple2 BinaryPickler.pickleUTF8 BinaryPickler.pickleDecimal
    let toProd (name,price) = {ProductName = name; ProductPrice = price*1.0M<GBP>}
    let fromProd prod = prod.ProductName, decimal prod.ProductPrice
    BinaryPickler.wrap (toProd, fromProd) nameDecPickler

(**

Here, the custom record type contains a string and a decimal with a unit of measure so we define a tuple pickler
which will pickle/unpickle the underlying data and provide functions that construct and deconstruct data from that
form.

Data can then easily be read into or written from our custom data type.

## List and Array combinators

The list and array combinators take a pickler of type 'a and produce a pickler that pickles the corresponding collection type.

*)

let intArrayPickler = BinaryPickler.array BinaryPickler.pickleInt32

let floatListPickler = BinaryPickler.list BinaryPickler.pickleFloat

(**

It is, of course, possible to combine several of these combinators to produce complicated list picklers, e.g.:

*)

let complexPickler = 
    BinaryPickler.list (BinaryPickler.tuple3 BinaryPickler.pickleAscii BinaryPickler.pickleFloat BinaryPickler.pickleInt32)