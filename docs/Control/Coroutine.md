## Module Control.Coroutine

This module defines types and functions for working with coroutines.
Coroutines are defined based on some underlying functor, which means that
the same machinery can be used for coroutines which emit values, await values,
fork, join, or any combination.

#### `Co`

``` purescript
type Co = FreeT
```

A coroutine whose commands are given by the functor `f`, with side effects
at each step given by the monad `m`.

#### `Process`

``` purescript
type Process = Co Identity
```

A `Process` is a `Co`routine which only has side effects, and supports no commands.

#### `loop`

``` purescript
loop :: forall f m a. (Functor f, Monad m) => Co f m (Maybe a) -> Co f m a
```

Loop until the computation pures a `Just`.

#### `runProcess`

``` purescript
runProcess :: forall m a. MonadRec m => Process m a -> m a
```

Run a `Process` to completion.

#### `fuseWith`

``` purescript
fuseWith :: forall f g h m a. (Functor f, Functor g, Functor h, MonadRec m) => (forall d. forall b c. (b -> c -> d) -> f b -> g c -> h d) -> Co f m a -> Co g m a -> Co h m a
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
Bifunctor Emit
Functor (Emit o)
```

#### `Producer`

``` purescript
type Producer o = Co (Emit o)
```

A type synonym for a `Co`routine which only emits values.

#### `emit`

``` purescript
emit :: forall m o. Monad m => o -> Producer o m Unit
```

Emit an output value.

#### `producer`

``` purescript
producer :: forall o m r. Monad m => m (Either o r) -> Producer o m r
```

Create a `Producer` by providing a monadic function that produces values.

The function should pure a value of type `r` at most once, when the
`Producer` is ready to close.

#### `Await`

``` purescript
newtype Await i a
  = Await (i -> a)
```

A generating functor for awaiting input values.

##### Instances
``` purescript
Profunctor Await
Functor (Await i)
```

#### `Consumer`

``` purescript
type Consumer i = Co (Await i)
```

A type synonym for a `Co`routine which only awaits values.

#### `await`

``` purescript
await :: forall m i. Monad m => Consumer i m i
```

Await an input value.

#### `consumer`

``` purescript
consumer :: forall i m r. Monad m => (i -> m (Maybe r)) -> Consumer i m r
```

Create a `Consumer` by providing a handler function which consumes values.

The handler function should pure a value of type `r` at most once, when the
`Consumer` is ready to close.

#### `Transform`

``` purescript
newtype Transform i o a
  = Transform (i -> Tuple o a)
```

A generating functor for transforming input values into output values.

##### Instances
``` purescript
Bifunctor (Transform i)
Functor (Transform i o)
```

#### `Transformer`

``` purescript
type Transformer i o = Co (Transform i o)
```

A type synonym for a `Co`routine which transforms values.

#### `transform`

``` purescript
transform :: forall m i o. Monad m => (i -> o) -> Transformer i o m Unit
```

Transform input values.

#### `CoTransform`

``` purescript
data CoTransform i o a
  = CoTransform o (i -> a)
```

A generating functor which yields a value before waiting for an input.

##### Instances
``` purescript
Bifunctor (CoTransform i)
Functor (CoTransform i o)
```

#### `CoTransformer`

``` purescript
type CoTransformer i o = Co (CoTransform i o)
```

A type synonym for a `Co`routine which "cotransforms" values, emitting an output
before waiting for its input.

#### `cotransform`

``` purescript
cotransform :: forall m i o. Monad m => o -> CoTransformer i o m i
```

Cotransform input values.

#### `transformCoTransformL`

``` purescript
transformCoTransformL :: forall i1 i2 o m a. MonadRec m => Transformer i1 i2 m a -> CoTransformer i2 o m a -> CoTransformer i1 o m a
```

Transform a `CoTransformer` on the left.

#### `transformCoTransformR`

``` purescript
transformCoTransformR :: forall i o1 o2 m a. MonadRec m => CoTransformer i o1 m a -> Transformer o1 o2 m a -> CoTransformer i o2 m a
```

Transform a `CoTransformer` on the right.

#### `fuseCoTransform`

``` purescript
fuseCoTransform :: forall i o m a. MonadRec m => Transformer i o m a -> CoTransformer o i m a -> Process m a
```

Fuse a transformer and a cotransformer.

#### `connect`

``` purescript
connect :: forall o m a. MonadRec m => Producer o m a -> Consumer o m a -> Process m a
```

Connect a producer and a consumer.

#### `($$)`

``` purescript
infixr 2 connect as $$
```

#### `transformProducer`

``` purescript
transformProducer :: forall i o m a. MonadRec m => Producer i m a -> Transformer i o m a -> Producer o m a
```

Transform a producer.

#### `($~)`

``` purescript
infixr 2 transformProducer as $~
```

#### `transformConsumer`

``` purescript
transformConsumer :: forall i o m a. MonadRec m => Transformer i o m a -> Consumer o m a -> Consumer i m a
```

Transform a consumer.

#### `(~$)`

``` purescript
infixr 2 transformConsumer as ~$
```

#### `composeTransformers`

``` purescript
composeTransformers :: forall i j k m a. MonadRec m => Transformer i j m a -> Transformer j k m a -> Transformer i k m a
```

Compose transformers

#### `(~~)`

``` purescript
infixr 2 composeTransformers as ~~
```

#### `composeCoTransformers`

``` purescript
composeCoTransformers :: forall i j k m a. MonadRec m => CoTransformer i j m a -> CoTransformer j k m a -> CoTransformer i k m a
```

Compose cotransformers

#### `joinProducers`

``` purescript
joinProducers :: forall o1 o2 m a. MonadRec m => Producer o1 m a -> Producer o2 m a -> Producer (Tuple o1 o2) m a
```

Run two producers together.

#### `(/\)`

``` purescript
infixr 3 joinProducers as /\
```

#### `joinConsumers`

``` purescript
joinConsumers :: forall i1 i2 m a. MonadRec m => Consumer i1 m a -> Consumer i2 m a -> Consumer (Tuple i1 i2) m a
```

Run two consumers together

#### `(\/)`

``` purescript
infixr 3 joinConsumers as \/
```


