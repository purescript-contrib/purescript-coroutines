## Module Control.Coroutine

This module defines types and functions for working with coroutines.
Coroutines are defined based on some underlying functor, which means that
the same machinery can be used for coroutines which emit values, await values,
fork, join, or any combination.

#### `Co`

``` purescript
data Co f m a
```

A coroutine whose commands are given by the functor `f`, with side effects
at each step given by the monad `m`.

##### Instances
``` purescript
instance functorCo :: (Functor f, Functor m) => Functor (Co f m)
instance applyCo :: (Functor f, Monad m) => Apply (Co f m)
instance applicativeCo :: (Functor f, Monad m) => Applicative (Co f m)
instance bindCo :: (Functor f, Monad m) => Bind (Co f m)
instance monadCo :: (Functor f, Monad m) => Monad (Co f m)
instance monadTransCo :: (Functor f) => MonadTrans (Co f)
```

#### `Process`

``` purescript
type Process = Co Identity
```

A `Process` is a `Co`routine which only has side effects, and supports no commands.

#### `hoistCo`

``` purescript
hoistCo :: forall f m n a. (Functor f, Functor n) => (forall a. m a -> n a) -> Co f m a -> Co f n a
```

Change the underlying `Monad` for a `Co`routine.

#### `stateful`

``` purescript
stateful :: forall f m a s. (Functor f, Monad m) => (s -> Co f m (Either a s)) -> s -> Co f m a
```

Construct a `Co`routine from a stateful updater function.

#### `loop`

``` purescript
loop :: forall f m a b. (Functor f, Monad m) => Co f m (Maybe a) -> Co f m a
```

Loop until the computation returns a `Just`.

#### `repeatedly`

``` purescript
repeatedly :: forall f m a b. (Functor f, Monad m) => Co f m a -> Co f m b
```

Loop indefinitely.

#### `liftCo`

``` purescript
liftCo :: forall f m a. (Functor f, Monad m) => f a -> Co f m a
```

Lift a command from the functor `f` into a one-step `Co`routine.

#### `runCo`

``` purescript
runCo :: forall f m a. (Functor f, MonadRec m) => (forall a. f a -> m a) -> Co f m a -> m a
```

Run a `Co`routine to completion.

#### `runProcess`

``` purescript
runProcess :: forall m a. (MonadRec m) => Process m a -> m a
```

Run a `Process` to completion.

#### `fuse`

``` purescript
fuse :: forall f g m a. (Functor f, Functor g, MonadRec m) => (forall a b c. (a -> b -> c) -> f a -> g b -> c) -> Co f m a -> Co g m a -> Process m a
```

Fuse two `Co`routines.

#### `Emit`

``` purescript
data Emit o a
  = Emit o a
```

A generating functor for emitting output values.

##### Instances
``` purescript
instance functorEmit :: Functor (Emit o)
```

#### `Producer`

``` purescript
type Producer o = Co (Emit o)
```

A type synonym for a `Co`routine which only emits values.

#### `emit`

``` purescript
emit :: forall m o. (Monad m) => o -> Producer o m Unit
```

Emit an output value.

#### `Await`

``` purescript
newtype Await i a
  = Await (i -> a)
```

A generating functor for awaiting input values.

##### Instances
``` purescript
instance functorAwait :: Functor (Await i)
```

#### `Consumer`

``` purescript
type Consumer i = Co (Await i)
```

A type synonym for a `Co`routine which only awaits values.

#### `await`

``` purescript
await :: forall m i. (Monad m) => Consumer i m i
```

Await an input value.

#### `emitAwait`

``` purescript
emitAwait :: forall e a b c. (a -> b -> c) -> Emit e a -> Await e b -> c
```

Fuse the `Emit` and `Await` functors.

#### `feed`

``` purescript
feed :: forall e m a. (MonadRec m) => Producer e m a -> Consumer e m a -> Process m a
```

Feed the values produced by a producer into a consumer.

#### `(>~>)`

``` purescript
(>~>) :: forall e m a. (MonadRec m) => Producer e m a -> Consumer e m a -> Process m a
```

_left-associative / precedence -1_

Infix version of `feed`.

#### `(<~<)`

``` purescript
(<~<) :: forall e m a. (MonadRec m) => Consumer e m a -> Producer e m a -> Process m a
```

_left-associative / precedence -1_

Infix version of `flip feed`.


