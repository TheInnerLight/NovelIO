(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "NovelIO/NovelIO.dll"
open NovelFS.NovelIO

(**
#NovelIO
======================

## Introduction

NovelIO is a library designed to bring the explicit safety and robustness of Haskell's IO monad to the .NET framework. The result is a purely functional approach to describing I/O operations whereby the evaluation of functions do not produce side-effects, rather they describe sequenced operations which can later be evaluated.

Much like in Haskell, we introduce the `IO<'a>` type which represents some action that, when performed successfully, returns some result `'a.`  Here are some examples:

* An IO action that prints the string "Hello World" to the screen has type `IO<unit>`.

* An IO action that gets a line of text from the Console has type 'IO<string>`.

* An IO action that opens a TCP connection to google.com on port 80 has type `IO<TCPConnectedSocket>`.

Values of type `IO<'a>` are distinct from traditional values in that they do not represent the result of some side effect, they rather represent an action that can be `run` to produce a particular result.

## Running IO Actions

`IO<'a>` Actions can be `run` using the `IO.run` function.  This results in all of the side-effects being evaluated, resulting in something of type `IOResult<'a>`.

`IOResult<'a>` is a Discriminated Union defined as follows:*)

    type IOResult<'a> =
        |IOSuccess of 'a
        |IOError of IOErrorResult

(**
Thus, our IO action can either be successful, returning the desired result of type `'a` or it can fail, returning some error.

It is possible to sequence I/O operations using the io computation expression, this results in syntax very similar to the do notation found in Haskell.

*)


