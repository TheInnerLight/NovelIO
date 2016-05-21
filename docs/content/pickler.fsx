(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "NovelIO/NovelIO.dll"
open NovelFS.NovelIO
open NovelFS.NovelIO.BinaryPickler

(**
Pickler Combinators
======================

Pickler combinators are a concept described by Andrew J. Kennedy (http://research.microsoft.com/pubs/64036/picklercombinators.pdf).

Their purpose is to present a serialisation/deserialiation mechanism that grants the developer explicit control over their serialisation format while avoiding the requirement to write lots of tedious, error prone code.

Pickler combinators operate by allowing the construction of pickler/unpickler pairs (hereafter called PUs from brevity) which excapsulate the serialisation process.

## PU primitives

PU primitives are provided to handle simple data types and more complicated PUs can be constructed by combining these PUs using combinator functions.

*)

let intPU = BinaryPickler.intPU

let floatPU = BinaryPickler.float32PU

let asciiPU = BinaryPickler.asciiPU

(**

These are just a few examples of the primitive PUs defined in this library.  These PUs can be used to serialise (pickle) or deserialise (unpickle) values of the type associated with their PU to and from their binary representations.

The beauty of pickler combinators is that the same PU can be used to transform data in both directions.

## Endianness

The default PUs for each datatype, such as `intPU` will pickle in the endianness of the current platform.  It's also possible to specify endianness for each primitive.

*)

let intPULtE = BinaryPickler.int16LittleEPU // little endian int PU

let utf32PUBgE = BinaryPickler.utf32BigEPU // big endian utf-32 PU

(**

Little endian PUs are suffixed with `LittleEPU` and big endian PUs are suffixed with `BigEPU`.

## Running PUs
*)

let bytes = BinaryPickler.pickle (BinaryPickler.intPU) 64 // convert the int value 64 into a byte array

let intR = BinaryPickler.unpickle (BinaryPickler.intPU) bytes // convert the byte array back into an int

(**

Here we use the pickle function to transform the int 64 into its binary representation and then transform it back using the unpickle function.

## Tuple combinators

A variety of tuple combinators are provided to allow the pickling/unpickling of tuples.  These can be constructed, for example, as follows:
*)

let intUtf8PU = BinaryPickler.tuple2 BinaryPickler.intPU BinaryPickler.utf8PU

(**

The `tuple2` function is simply supplied with two PUs and it constructs a combined pickler.

`tuple3` and `tuple4` functions are also provided, allowing the construction of more complex PUs.

## Wrapping PUs

Rather than constructing data from tuples, we may wish to pickle/unpickle custom data types.  It is possible to do this by providing a function which constructs and deconstructs this custom data-type.

*)

/// Unit of pounds sterling
[<Measure>] type GBP

/// A product with an associated price
type Product = {ProductName : string; ProductPrice : decimal<GBP>}

/// A pickler/unpickler pair (PU) for products
let productPU =
    let nameDecPU = BinaryPickler.tuple2 BinaryPickler.utf8PU BinaryPickler.decimalPU
    let toProd (name,price) = {ProductName = name; ProductPrice = price*1.0M<GBP>} // tuple to product
    let fromProd prod = prod.ProductName, decimal prod.ProductPrice // product to tuple
    BinaryPickler.wrap (toProd, fromProd) nameDecPU

(**

Here, the custom record type contains a string and a decimal with a unit of measure so we define a tuple PU which will pickle/unpickle the underlying data and provide functions that construct and deconstruct data from that form.

Data can then easily be read into or written from our custom data type.

## List and Array combinators

The list and array combinators take a PU of type 'a and produce a pickler that pickles the corresponding collection type.

*)

let intArrayPU = BinaryPickler.array BinaryPickler.intPU

let floatListPU = BinaryPickler.list BinaryPickler.intPU

(**

It is, of course, possible to combine several of these combinators to produce complicated list PUs, e.g.:

*)

let complexPickler = 
    BinaryPickler.list 
        (BinaryPickler.tuple3 
            BinaryPickler.asciiPU BinaryPickler.floatPU BinaryPickler.intPU)

(**

## Incremental Pickling

In many cases, especially when dealing with large binary files, it could be desirable to not have to convert back and forth between extremely large byte arrays, indeed this approach might not be viable due to available memory.

In this case, we can use incremental pickling to read/write as part of the pickling process.  Unlike the simple conversion process shown above, this action is effectful so is encapsulated within `IO`.

This process is quite simple, instead of using the `pickle` and `unpickle` functions, we use the `pickleIncr` and `unpickleIncr` functions.  These simply take the additional argument of a `BinaryHandle` upon which they will act.

Example of incremental unpickling:

*)

io {
    let! handle = File.openBinaryHandle FileMode.Open FileAccess.Read (File.assumeValidFilename "test.txt")
    return! BinaryPickler.unpickleIncr complexPickler handle
}

(**
Example of incremental pickling:
*)

io {
    let! handle = File.openBinaryHandle FileMode.Create FileAccess.Write (File.assumeValidFilename "test.txt")
    let data = [("A", 7.5, 16); ("B", 7.5, 1701)]
    return! BinaryPickler.pickleIncr complexPickler handle data
}