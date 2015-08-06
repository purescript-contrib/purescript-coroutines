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
instance monadRecCo :: (Functor f, Monad m) => MonadRec (Co f m)
```

#### `Process`

``` purescript
type Process = Co Identity
```

A `Process` is a `Co`routine which only has side effects, and supports no commands.

#### `liftCo`

``` purescript
liftCo :: forall f m a. (Functor f, Monad m) => f a -> Co f m a
```

Lift a command from the functor `f` into a one-step `Co`routine.

#### `hoistCo`

``` purescript
hoistCo :: forall f m n a. (Functor f, Functor n) => (forall a. m a -> n a) -> Co f m a -> Co f n a
```

Change the underlying `Monad` for a `Co`routine.

#### `interpret`

``` purescript
interpret :: forall f g m a. (Functor f, Functor m) => (forall a. f a -> g a) -> Co f m a -> Co g m a
```

Change the functor `f` for a `Co`routine.

#### `loop`

``` purescript
loop :: forall f m a b. (Functor f, Monad m) => Co f m (Maybe a) -> Co f m a
```

Loop until the computation returns a `Just`.

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

#### `fuseWith`

``` purescript
fuseWith :: forall f g h m a. (Functor f, Functor g, Functor h, MonadRec m) => (forall a b c. (a -> b -> c) -> f a -> g b -> h c) -> Co f m a -> Co g m a -> Co h m a
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

#### `producer`

``` purescript
producer :: forall o m r. (Monad m) => m (Either o r) -> Producer o m r
```

Create a `Producer` by providing a monadic function that produces values.

The function should return a value of type `r` at most once, when the
`Producer` is ready to close.

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

#### `consumer`

``` purescript
consumer :: forall i m r. (Monad m) => (i -> m (Maybe r)) -> Consumer i m r
```

Create a `Consumer` by providing a handler function which consumes values.

The handler function should return a value of type `r` at most once, when the
`Consumer` is ready to close.

#### `Transform`

``` purescript
newtype Transform i o a
  = Transform (i -> Tuple o a)
```

A generating functor for transforming input values into output values.

##### Instances
``` purescript
instance functorTransform :: Functor (Transform i o)
```

#### `Transformer`

``` purescript
type Transformer i o = Co (Transform i o)
```

A type synonym for a `Co`routine which transforms values.

#### `transform`

``` purescript
transform :: forall m i o. (Monad m) => (i -> o) -> Transformer i o m Unit
```

Transform input values.

#### `($$)`

``` purescript
($$) :: forall o m a. (MonadRec m) => Producer o m a -> Consumer o m a -> Process m a
```

_left-associative / precedence -1_

Connect a producer and a consumer.

#### `($~)`

``` purescript
($~) :: forall i o m a. (MonadRec m) => Producer i m a -> Transformer i o m a -> Producer o m a
```

_left-associative / precedence -1_

Transform a producer.

#### `(~$)`

``` purescript
(~$) :: forall i o m a. (MonadRec m) => Transformer i o m a -> Consumer o m a -> Consumer i m a
```

_left-associative / precedence -1_

Transform a consumer.

#### `(~~)`

``` purescript
(~~) :: forall i j k m a. (MonadRec m) => Transformer i j m a -> Transformer j k m a -> Transformer i k m a
```

_left-associative / precedence -1_

Compose transformers


