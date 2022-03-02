# Coroutines

[![CI](https://github.com/purescript-contrib/purescript-coroutines/workflows/CI/badge.svg?branch=main)](https://github.com/purescript-contrib/purescript-coroutines/actions?query=workflow%3ACI+branch%3Amain)
[![Release](https://img.shields.io/github/release/purescript-contrib/purescript-coroutines.svg)](https://github.com/purescript-contrib/purescript-coroutines/releases)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-coroutines/badge)](https://pursuit.purescript.org/packages/purescript-coroutines)
[![Maintainer: garyb](https://img.shields.io/badge/maintainer-garyb-teal.svg)](https://github.com/garyb)

Coroutines are a general control structure allowing flow control to pass cooperatively between two different routines. Coroutines in this library are computations which can suspend their execution and return control to their invoker, which can resume the computation. Coroutines can be used to implement pipelines as described in [Coroutine Pipelines by Mario Blažević](https://themonadreader.files.wordpress.com/2011/10/issue19.pdf).

If you are creating asynchronous or concurrent pipelines you may be interested in:

- [aff-coroutines](https://github.com/purescript-contrib/purescript-aff-coroutines)

If you need a featureful streaming library, you may also be interested in:

- [pipes](https://github.com/felixSchl/purescript-pipes)
- [run-streaming](https://github.com/natefaubion/purescript-run-streaming)
- [machines](https://github.com/purescript-contrib/purescript-machines)

If you are looking for a FRP library, then you may be interested in:

- [event](https://github.com/paf31/purescript-event)

## Installation

Install `coroutines` with [Spago](https://github.com/purescript/spago):

```sh
spago install coroutines
```

## Quick start

This quick start briefly introduces the basics of `coroutines`. For a more thorough, beginner-friendly introduction to the library, please see the [library documentation](./docs).

The basic building block of `coroutines` is the coroutine type `Co`, which exhibits different behavior when it suspensd based on a functor `f`:

- When `f` is the `Emit o` functor, the coroutine emits an output of type `o` and is a `Producer`
- When `f` is the `Await i` functor, the coroutine waits for an input of type `i` and is a `Consumer`
- When `f` is the `Transform i o` functor, the coroutine waits for an input of type `i`, and emits an output of type `o`, and is a `Transformer`.

A coroutine which _emits_ can be thought of as a generator, where each yield produces a value. A coroutine which _awaits_ can be thought of as an iteratee, where each yield demands a value. And a coroutine which _transforms_ does both: it demands an input, and produces an output.

Using these three building blocks we can create some standard coroutines and form them into pipelines.

Here is a coroutine which generates natural numbers:

```purs
nats :: forall m. (Monad m) => Producer Int m Unit
nats = go 0
  where
  go i = do
    emit i
    go (i + 1)
```

The computation runs, emits a number, and suspends; when resumed it will emit the next number and then suspend again. It uses the `emit` function:

```purs
emit :: forall m o. Monad m => o -> Producer o m Unit
```

Here is a coroutine which accepts and prints strings:

```purs
printer :: forall a. Consumer String (Aff _) Unit
printer = forever do
  s <- await
  lift (log s)
```

The computation suspends, awaiting a number. When it receives a number it logs the number and then suspends, awaiting a new number. It uses the `await` function:

```purs
await :: forall m i. Monad m => Consumer i m i
```

Here is a coroutine which transforms inputs by showing them as strings:

```purs
showing :: forall a m. (Show a, Monad m) => Transformer a String m Unit
showing = forever (transform show)
```

The computation suspends, awaiting an `a`. When it receives the awaited value, it transforms it into a string, emits the new value, and suspends again, awaiting another `a`. It uses the `transform` function:

```purs
transform :: forall m i o. Monad m => (i -> o) -> Transformer i o m Unit
```

These coroutines can be combined together using a handful of operators, the most common of which include:

```purs
connect :: forall o f m a. MonadRec m => Parallel f m => Producer o m a -> Consumer o m a -> Process m a
infixr 2 connect as $$

transformProducer :: forall i o f m a. MonadRec m => Parallel f m => Producer i m a -> Transformer i o m a -> Producer o m a
infixr 2 transformProducer as $~

transformConsumer :: forall i o f m a. MonadRec m => Parallel f m => Transformer i o m a -> Consumer o m a -> Consumer i m a
infixr 2 transformConsumer as ~$

composeTransformers :: forall i j k f m a. MonadRec m => Parallel f m => Transformer i j m a -> Transformer j k m a -> Transformer i k m a
infixr 2 composeTransformers as ~~
```

Once composed, the resulting computation can be run using `runFreeT`. If you have used `connect` to connect a producer and consumer, then you can use `runProcess` (a helper function for running a producer/consumer pair via `runFreeT`).

This example transforms the `nats` producer so that instead of producing integers it produces strings, and then connects the resulting producer to the `printer` consumer. Once connect, we can use `runProcess` to run the pipeline:

```purs
main = launchAff $ runProcess ((nats $~ showing) $$ printer)
```

The producer will emit a value, then yield; this value will satisfy the `await` call in the consumer, which will use the value and then yield back to the producer. This process will continue indefinitely.

## Documentation

`coroutines` documentation is stored in a few places:

1. Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-coroutines).
2. Written documentation is kept in the [docs directory](./docs).
3. Usage examples can be found in [the test suite](./test).

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/purescript-contrib/purescript-coroutines/issues) if you have encountered a bug or problem.
- Ask general questions on the [PureScript Discourse](https://discourse.purescript.org) forum or the [PureScript Discord](https://purescript.org/chat) chat.

## Contributing

You can contribute to `coroutines` in several ways:

1. If you encounter a problem or have a question, please [open an issue](https://github.com/purescript-contrib/purescript-coroutines/issues). We'll do our best to work with you to resolve or answer it.

2. If you would like to contribute code, tests, or documentation, please [read the contributor guide](./CONTRIBUTING.md). It's a short, helpful introduction to contributing to this library, including development instructions.

3. If you have written a library, tutorial, guide, or other resource based on this package, please share it on the [PureScript Discourse](https://discourse.purescript.org)! Writing libraries and learning resources are a great way to help this library succeed.
