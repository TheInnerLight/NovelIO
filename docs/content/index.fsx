(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
NovelIO
======================

Introduction

NovelIO is a library designed to bring the explicit safety and robustness of Haskell's IO monad to the .NET framework. The result is a purely functional approach to describing I/O operations whereby the evaluation of functions do not produce side-effects, rather they describe sequenced operations which can later be evaluated.

Much like in Haskell, we introduce the `IO<'a>` type which represents some action that, when performed successfully, returns some result `'a.`

It is possible to sequence I/O operations using the io computation expression, this results in syntax very similar to the do notation found in Haskell.

*)


