# purescript-coroutines

[![Latest release](http://img.shields.io/github/release/purescript-contrib/purescript-coroutines.svg)](https://github.com/purescript-contrib/purescript-coroutines/releases)
[![Build status](https://travis-ci.org/purescript-contrib/purescript-coroutines.svg?branch=master)](https://travis-ci.org/purescript-contrib/purescript-coroutines)
[![Pursuit](http://pursuit.purescript.org/packages/purescript-coroutines/badge)](http://pursuit.purescript.org/packages/purescript-coroutines/)
[![Maintainer: natefaubion](https://img.shields.io/badge/maintainer-natefaubion-lightgrey.svg)](http://github.com/natefaubion)

- [Module Documentation](http://pursuit.purescript.org/packages/purescript-coroutines/)
- [Example](test/Main.purs)

## Usage

This section provides a brief overview of how to install this library and some of the basic concepts behind it. For a richer explanation of this guide, see the section at the end of this file.

This library can be installed using Bower:

    bower i purescript-coroutines

and built using Pulp:

    pulp build
    pulp test

The basic building block is the coroutine type `Co`, which exhibits different behavior when it suspends based on a functor `f`:

- When `f` is the `Emit o` functor, the coroutine emits an output of type `o`
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
printer :: forall a. Consumer String (Aff _) Unit
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
main = launchAff $ runProcess ((nats $~ showing) $$ printer)
```

## Contributing

Read the [contribution guidelines](https://github.com/purescript-contrib/purescript-coroutines/blob/master/.github/contributing.md) to get started and see helpful related resources.

Here's my basic understanding of how `purescript-coroutines` work based on my viewing yesterday. It isn't complete (and I hope to finish it later this week), but it should be enough to get people 80% of what they need.

## What Problem does `purescript-coroutines` Solve?

### An Overview of the Problem

Based on an [example provided by the library](https://github.com/purescript-contrib/purescript-coroutines/blob/v5.0.0/test/Main.purs), it seems the library is trying to model functional reactive programming.
In its simplest version, functional reactive programming models "data pipelines" in a declarative way rather than an imperative way. Through it, one can write,
- (simple) "When the event X occurs, do Y."
- (complex) "When EITHER (event A occurs, and B has not happened in the past 3 seconds, and user is in context C) OR (event D occurs and E is not false), do Z."
- (simple) "Pull the next 10 characters from a file and do X with them."

"Data pipelines" work the same way as using the pipe/`|` character to compose multiple Linux bash programs together.
```bash
command1 | command2 | command3 | ... | finalCommand
```
It works similar to using `>>>` to compose multiple functions together.

To model this pipeline-like concept, coroutines provides 4 different concepts: Producers, Consumers, Transformers, and Cotransformers. We'll cover the first two before describing the latter two.

### The Basic Concepts

Producers represent a way to produce some initial data. For example, an apple tree produces apples. An HTML tag "produces" click events. A paper tray in a printer _can_ provide paper when someone needs to print something. Producers don't know or care what happens to the things they produce; they simply produce it.

Consumers represent a way to consume some final data. For example, one might eat an apple. Or a click event might be handled by alerting the user of some information. Or a printer might use the paper to print out a document. Consumers don't know or care where the data comes from; they simply consume it once they get it.

Thus, the shortest program one can build is a Producer (e.g. apple tree) that is connected with a Consumer (e.g. a person who likes eating apples). When the Producer "emits" or produces some data, the Consumer "eats" or consumes that data. Put visually:
```
Producer --- (data) ---> Consumer
```

However, if all we had were `Producers` and `Consumers`, things would get pretty boring. This style of programming gets more interesting when we introduce `Transformers` and `Cotransformers`.

### `Transformer`, a `Functor` for `Producer` and a `Contravariant` for `Consumer`

`Transformer` has similarities to the `Functor` and `Contravariant` type classes. In short, a `Transformer` changes the type emitted by `Producer` or consumed by `Consumer`.

Put visually, if `Producer` produces/emits/outputs values of the `a` type, and `Transformer` knows how to change values of the `a` type to values of the `b` type, and `Consumer` consumes values of the `b` type, then we would draw this like so:
```
Producer --- (a) --> Transformer ---- (b) ---> Consumer
```

If we combine the `Producer` with the `Transformer`, then the `Transformer` has a `Functor`-like affect on the `Producer`. In other words, it acts like `map`/`<$>` on the `Producer`'s output.
```
| ------ the whole Producer ------ |
( Producer --- (a) --> Transformer ) --- (b) ---> Consumer
```

If we combine the `Consumer` with the `Transformer`, the `Transformer` has a `Contravariant`-like affect on the `Consumer`. In other words, it acts like `contrmap`/`>$<` on the `Consumer`'s input.

```
                     | ------ the whole Consumer ------  |
Producer --- (a) --> ( Transformer --- (b) ---> Consumer )
```

#### `Cotransformer`, a `Producer`-`Consumer` with a transformative "hook"

`Cotransformer` is a way of combining both a `Producer` and `Consumer` together in an underlying "black box." However, this black box exposes a "hook" that can accomplish the same `Functor`/`Contravariant`-like affect of a `Transformer`.

Put visually, the below box represents the `Cotransformer` and the function, `a -> b`, is the hook.

```
                       (a -> b)
                          ↓↓
                      +---++---+
                      |   ||   |
+---------------------+   ||   +------------------+
|                         ↓↓                      |
| Producer --- (a) ---> "hook" (b) ---> Consumer  |
|                                                 |
+-------------------------------------------------+
```

This is useful when you have the same `Producer`-`Consumer` relationship, but you need to change the `Producer`'s output depending on other contextual information.

### Data Pipelines: Recipes in the Making

In our original example of a producer, we mentioned that an apple tree produces apples. Then, we said that one apple can be transformed into applesauce or apple pie. Using this example, one might think that an apple can only be "consumed" once.

In reality, we can consume one apple multiple times. We're dealing with data and binary: things that can be copied or referenced. Thus, we might have one `Producers` that emits a value that is consumed by multiple `Consumers` (e.g. 1-to-many). Or we might have multiple `Producers` whose outputs are eventually consumed by one `Consumer` (e.g. many-to-1). Or it might be multpile `Producers` whose outputs are consumed by multiple `Consumers` (e.g. many-to-many). [The "food recipe" idea to functional programming definitely applies here](http://www.lihaoyi.com/post/WhatsFunctionalProgrammingAllAbout.html#functional-programming-recipes)

### To Push or to Pull?

Once we have a data pipeline constructed, the question becomes, "Who controls the flow of data: the `Producer` or the `Consumer`?" 

| Program description starts with the phrase: | Controller | Pipeline Type |
| - | - | - |
| When a new event A occurs... then...  | `Producer` | push-based  |
| When I am ready for the next ... then...  | `Consumer` | pull-based  |

For example, when the apple tree produces apples and the apple-lover eats the apple, the apple-lover (i.e. Consumer) depends upon the apple tree (Producer). Thus, this is a push-based relationship because the Producer must "push" the data to the Consumer.

When the printer needs to print a new document, the printer (i.e. Consumer) will pull one sheet of paper from the paper tray (i.e. Producer). Thus, this is a pull-based relationships because the Consumer must "pull" data from the Producer. 

## The Internals of `purescript-coroutines`

### Overviewing the Types

Looking at the [library's documentation](https://pursuit.purescript.org/packages/purescript-coroutines/5.0.0/docs/Control.Coroutine), we have the following types:
- Co
- Emit
- Producer
- Await
- Consumer
- Transformer
- Cotransformer
- Process

If we look at the initial 5 types, we'll see the following (and their desugared definitions):
```purescript
type Co = FreeT
-- desugars into
type Co f m a = FreeT f m a

data Emit output a = Emit output a
type Producer output = Co (Emit output)
-- desugars into
type Producer output monad a = FreeT (Emit output) monad a

data Await input a = Await (input -> a)
type Consumer input = Co (Await input)
-- desugars into
type Consumer input monad a = FreeT (Await input) monad a

type Process = Co Identity
-- desugars into
type Process monad a = FreeT Identity monad a
```

### `FreeT` and Its Usage

```purescript
data Free functor output = -- implementation not shown

createAST :: Free functor output
createAST = -- implementation not shown

interpretAST
  :: forall functor m output
  --            (functor        ~> m       ) -- a NaturalTransformation
   . Monad m => (functor output -> m output) -> Free functor output -> m output
interpretAST = -- implementation not shown
```
Given that `functor` is a data structure that has a `Funtor` instance, `Free functor output` enables one to build a pure, side-effect-free and useless abstract syntax tree (AST) using data structures and then to interpret that AST into an impure, side-effectful but useful program. This interpretation works via a `NaturalTransformation` from the `functor` to the side-effectful `m`onad.

`FreeT` changes this slightly by adding another type to the mix:
```purescript
data FreeT functor baseMonad output = -- implementation not shown
```

`FreeT` has the shape of a monad transformer. A monad transformer is a way to "augment" a base monad (e.g. `Effect` or `Aff`) with some additional capabilities. It enables us to write "pure, side-effect-free" business logic that is decoupled from the impure, side-effectful implementation of that logic. For example, one writes their logic using a type class (e.g. `MonadReader`, for getting a configuration value at any point in the computation) and then uses a monad transformer to implement the requirements of that type class (e.g. `ReaderT`).

`FreeT` is a way of merging the `Free`-based notion of a pure AST data structure with the impure, side-effectful a monad transformer that implements the program. In other words, `FreeT` does not need a type class to achieve the same thing that a regular "type class and monad transformer" combination does. Thus, `FreeT functor baseMonad output` says,
> If you give me a domain-specific language in terms of data structures that have a `Functor` instance (e.g. `purescript-coroutines` "commands:" `Emit` a value and `Await` for a value), and you give me a `NaturalTransformation` from those `Functor` types to a monadic data type like `Aff` (e.g. something the end-user of `coroutines` provides), then I can use those pure commands to run an impure program that produces `output`."

### Reviewing `purescript-coroutines`' types

#### Producer

Thus, looking back at our types
```purescript
type Co f m a = FreeT f m a

data Emit output a = Emit output a

-- Produces values of type, `output`. These values can be produced
-- via side-effects if the `monad` type supports that.
type Producer output monad a = FreeT (Emit output) monad a

-- Create a producer that doesn't use side effects to produce its output.
-- It will always produce the same value each time in a pull-based consumer
-- or only once in a push-based consumer
emit :: forall m o. Monad m => o -> Producer o m Unit

-- Create a producer that uses side effects to produce its output.
-- If this side-effect returns `Left o`, the producer emits the `o`.
-- If it returns `Right r`, the producer stops emitting values.
producer :: forall o m r. Monad m => m (Either o r) -> Producer o m r
```

#### Consumer

```purescript
type Co f m a = FreeT f m a

data Await input a = Await (input -> a)

-- Consume values of type, `input`. When consuming the input,
-- the Consumer can run side-effectful computations if the `monad` type
-- supports it.
type Consumer input monad a = FreeT (Await input) monad a

-- Create a consumer that never 'closes.' It will always respond to new
-- values produced by the Producer.
await :: forall m i. Monad m => Consumer i m i

-- Create a consumer that can close. Once closed, it will stop consuming
-- values produced by the Producer.
consumer :: forall i m r. Monad m => (i -> m (Maybe r)) -> Consumer i m r
```

#### Transformer

```purescript
-- Lift a function into a coroutines type, but don't yet use it to transform
-- a Producer or Consumer.
transform :: forall m i o. Monad m => (i -> o) -> Transformer i o m Unit

-- Transform a Producer.
transformProducer :: forall i o f m a. MonadRec m => Parallel f m => Producer i m a -> Transformer i o m a -> Producer o m a

infixr 2 transformProducer as $~

-- Transform a Consumer.
transformConsumer :: forall i o f m a. MonadRec m => Parallel f m => Transformer i o m a -> Consumer o m a -> Consumer i m a

infixr 2 transformConsumer as ~$

-- Compose Transformers
composeTransformers :: forall i j k f m a. MonadRec m => Parallel f m => Transformer i j m a -> Transformer j k m a -> Transformer i k m a

infixr 2 composeTransformers as ~~
```

#### `Process`: a way to run a `Producer`-`Consumer` relationship

While not covered above, a `Process` is the result of connecting a `Producer` to a `Consumer`, so that the data pipeline is fully formulated and now ready to be executed.

```purescript
-- Run a `Process` to completion.
runProcess :: forall m a. MonadRec m => Process m a -> m a

-- Create a Process by connecting a Producer and Consumer together using a
-- "push-based" relationship (i.e. Producer is in control).
-- The process ends when the Producer closes.
connect :: forall o f m a. MonadRec m => Parallel f m => Producer o m a -> Consumer o m a -> Process m a

infixr 2 connect as $$

-- Create a Process by connecting a Producer and Consumer together using a
-- "pull-based" relationship (i.e. Consumer is in control).
-- The process ends when the Consumer closes.
pullFrom :: forall o m a. MonadRec m => Consumer o m a -> Producer o m a -> Process m a
```

#### Infix Notation Cheatsheet

In the infixes above, the `~` indicates which side the transformer is on while the `$` indicates which side the producer/consumer is on:
```
producesA      $~ transformsAToB == producesB
transformsAToB ~$ consumesB      == consumesA
transformsAToB ~~ transformsBToC == transformsAToC
producesA      $$ consumesA      == process
```
