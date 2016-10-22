# purescript-coroutines

[![Latest release](http://img.shields.io/github/release/purescript-contrib/purescript-coroutines.svg)](https://github.com/purescript-contrib/purescript-coroutines/releases)
[![Build Status](https://travis-ci.org/purescript-contrib/purescript-coroutines.svg?branch=master)](https://travis-ci.org/purescript-contrib/purescript-coroutines)
[![Maintainer: paf31](https://img.shields.io/badge/maintainer-paf31-lightgrey.svg)](http://github.com/paf31)
[![Pursuit](http://pursuit.purescript.org/packages/purescript-coroutines/badge)](http://pursuit.purescript.org/packages/purescript-coroutines/)

- [Module Documentation](http://pursuit.purescript.org/packages/purescript-coroutines/)
- [Example](test/Main.purs)

## Usage

This library can be installed using Bower:

    bower i purescript-coroutines

and built using Pulp:

    pulp build
    pulp test

The basic building block is the coroutine type `Co`, which exhibits different behavior when it suspends based on a functor `f`:

- When `f` is the `Emit o` functror, the coroutine emits an output of type `o`
- When `f` is the `Await i` functor, the coroutine waits for an input of type `i`
- When `f` is the `Transform i o` functor, the coroutine waits for an input of type `i`, and emits an output of type `o`

See the article "Coroutine Pipelines" in _The Monad Reader, Issue 19_ for more details.

Using these building blocks, we can create some standard coroutines, which can form pipelines.

Here is a coroutine which generates the natural numbers in order:

```purescript
nats :: forall m. (Monad m) => Producer Int m Unit
nats = go 0
  where
  go i = do
    emit i
    go (i + 1)
```

Here is a coroutine which accepts and prints strings:    

```purescript
printer :: forall a. Consumer String (Eff _) Unit
printer = forever do
  s <- await
  lift (log s)
```

Here is a coroutine which transforms inputs by showing them as strings:

```purescript
showing :: forall a m. (Show a, Monad m) => Transformer a String m Unit
showing = forever (transform show)
```

These coroutines can be combined using a handful of operators, and run using `runFreeT` or `runProcess`:

```purescript
main = runProcess (nats $~ showing $$ printer)
```
