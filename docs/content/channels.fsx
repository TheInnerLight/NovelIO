(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "NovelIO/NovelIO.dll"
open NovelFS.NovelIO

(**
Channels
======================

Channels represent a mechanism of one or two way communication with some kind of resource, such as a file or remote server (e.g. via TCP).

Channels come in two flavours, a `TChannel` which supports text data and a `BChannel` which supports binary data, functions are provided in the `TextChannel` and `BinaryChannel` modules respectively.

## Controlling lifetime

The typical method of controlling method of explicitly controlling lifetime in .NET is to use `IDispoable`, however this approach fundamentally relies on side-effects.  NovelIO therefore uses a different approach: the bracket pattern.

`bracket` is a function supplied in the `IO` module: `val bracket : IO<'a> -> ('a -> IO<'b>) -> ('a -> IO<'c>) -> IO<'c>`.

The first argument is an action of type `IO<'a>` which creates a resource.

The second argument is a function which takes the resource created by the first action and cleans it up.

The third argument is a function which takes the resource created by the first action and returns a new action, this new action is then returned by the `bracket` function.

Put succinctly, there is a way of creating a resource, a way of cleaning it up and a function to happen in between.

### Using the bracket pattern

In general, you don't need to worry about using 'bracket' explictly.  Functions are created for different resources to avoid you having to fill out all of `bracket`'s arguments manually.

An example is the `File.withTextChannel` function:

*)


File.withTextChannel File.Open.defaultRead (File.Path.fromValid "test.txt") (fun channel ->
    io {
        let! l1 = TextChannel.getLine channel
        let! l2 = TextChannel.getLine channel
        return l1, l2
    })


(**

By using the `withTextChannel` function, we can supply an argument of the form `TChannel -> IO<'a>` which determines what to do with the channel during its entire lifetime.  This is equivalent to just the final argument of `bracket` where the two preceeding arguments are filled in for us.

`with_` functions are provided throughout the library for other types of channels and resources but they follow the same pattern as described here.

*)