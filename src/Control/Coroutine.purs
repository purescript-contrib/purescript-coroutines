-- | This module defines types and functions for working with coroutines.
-- | Coroutines are defined based on some underlying functor, which means that
-- | the same machinery can be used for coroutines which emit values, await values,
-- | fork, join, or any combination.

module Control.Coroutine
  ( Co
  , Process
  , loop
  , runProcess
  , fuseWith
  , fuseWithL
  , Emit(..), Producer, emit, producer
  , Await(..), Consumer, await, consumer
  , Transform(..), Transformer, transform
  , CoTransform(..), CoTransformer, cotransform
  , connect, ($$), pullFrom
  , transformProducer, ($~)
  , transformConsumer, (~$)
  , composeTransformers, (~~)
  , composeCoTransformers
  , fuseCoTransform
  , transformCoTransformL
  , transformCoTransformR
  , joinProducers, (/\)
  , joinConsumers, (\/)
  ) where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Free.Trans (FreeT, liftFreeT, freeT, resume, runFreeT)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Parallel (class Parallel, parallel, sequential)

import Data.Bifunctor as B
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Profunctor as P
import Data.Tuple (Tuple(..))

-- | A coroutine whose commands are given by the functor `f`, with side effects
-- | at each step given by the monad `m`.
type Co = FreeT

-- | A `Process` is a `Co`routine which only has side effects, and supports no commands.
type Process = Co Identity

-- | Loop until the computation returns a `Just`.
loop :: forall f m a. Functor f => Monad m => Co f m (Maybe a) -> Co f m a
loop me = tailRecM (\_ -> map (maybe (Loop unit) Done) me) unit

-- | Run a `Process` to completion.
runProcess :: forall m a. MonadRec m => Process m a -> m a
runProcess = runFreeT (pure <<< unwrap)

-- | Fuse two `Co`routines.
fuseWith
  :: forall f g h m a par
   . Functor f
  => Functor g
  => Functor h
  => MonadRec m
  => Parallel par m
  => (forall b c d. (b -> c -> d) -> f b -> g c -> h d)
  -> Co f m a
  -> Co g m a
  -> Co h m a
fuseWith zap fs gs = freeT \_ -> go (Tuple fs gs)
  where
  go :: Tuple (Co f m a) (Co g m a) -> m (Either a (h (Co h m a)))
  go (Tuple fs' gs') = do
    next <- sequential
              (lift2 (zap Tuple) <$> parallel (resume fs')
                                 <*> parallel (resume gs'))
    case next of
      Left a -> pure (Left a)
      Right o -> pure (Right (map (\t -> freeT \_ -> go t) o))

-- | Fuse two `Co`routines with a bias to the left.
fuseWithL
  :: forall f g h m a
   . Functor f
  => Functor g
  => Functor h
  => MonadRec m
  => (forall b c d. (b -> c -> d) -> f b -> g c -> h d)
  -> Co f m a
  -> Co g m a
  -> Co h m a
fuseWithL zap fs gs = freeT \_ -> go (Tuple fs gs)
  where
  go :: Tuple (Co f m a) (Co g m a) -> m (Either a (h (Co h m a)))
  go (Tuple fs' gs') = runExceptT do
    l <- ExceptT $ resume fs'
    r <- ExceptT $ resume gs'
    pure (map (\t -> freeT \_ -> go t) (zap Tuple l r))

-- | A generating functor for emitting output values.
data Emit o a = Emit o a

instance bifunctorEmit :: B.Bifunctor Emit where
  bimap f g (Emit o a) = Emit (f o) (g a)

instance functorEmit :: Functor (Emit o) where
  map = B.rmap

-- | A type synonym for a `Co`routine which only emits values.
type Producer o = Co (Emit o)

-- | Emit an output value.
emit :: forall m o. Monad m => o -> Producer o m Unit
emit o = liftFreeT (Emit o unit)

-- | Create a `Producer` by providing a monadic function that produces values.
-- |
-- | The function should return a value of type `r` at most once, when the
-- | `Producer` is ready to close.
producer :: forall o m r. Monad m => m (Either o r) -> Producer o m r
producer recv = loop do
  e <- lift recv
  case e of
    Left o -> emit o $> Nothing
    Right r -> pure (Just r)

-- | A generating functor for awaiting input values.
newtype Await i a = Await (i -> a)

instance profunctorAwait :: P.Profunctor Await where
  dimap f g (Await k) = Await (P.dimap f g k)

instance functorAwait :: Functor (Await i) where
  map = P.rmap

-- | A type synonym for a `Co`routine which only awaits values.
type Consumer i = Co (Await i)

-- | Await an input value.
await :: forall m i. Monad m => Consumer i m i
await = liftFreeT (Await id)

-- | Create a `Consumer` by providing a handler function which consumes values.
-- |
-- | The handler function should return a value of type `r` at most once, when the
-- | `Consumer` is ready to close.
consumer :: forall i m r. Monad m => (i -> m (Maybe r)) -> Consumer i m r
consumer send = loop do
  a <- await
  lift (send a)

-- | A generating functor for transforming input values into output values.
newtype Transform i o a = Transform (i -> Tuple o a)

instance bifunctorTransform :: B.Bifunctor (Transform i) where
  bimap f g (Transform k) = Transform (B.bimap f g <<< k)

instance functorTransform :: Functor (Transform i o) where
  map = B.rmap

-- | A type synonym for a `Co`routine which transforms values.
type Transformer i o = Co (Transform i o)

-- | Transform input values.
transform :: forall m i o. Monad m => (i -> o) -> Transformer i o m Unit
transform f = liftFreeT (Transform \i -> Tuple (f i) unit)

-- | A generating functor which yields a value before waiting for an input.
data CoTransform i o a = CoTransform o (i -> a)

instance bifunctorCoTransform :: B.Bifunctor (CoTransform i) where
  bimap f g (CoTransform o k) = CoTransform (f o) (g <<< k)

instance functorCoTransform :: Functor (CoTransform i o) where
  map = B.rmap

-- | A type synonym for a `Co`routine which "cotransforms" values, emitting an output
-- | before waiting for its input.
type CoTransformer i o = Co (CoTransform i o)

-- | Cotransform input values.
cotransform :: forall m i o. Monad m => o -> CoTransformer i o m i
cotransform o = freeT \_ -> pure (Right (CoTransform o pure))

-- | Transform a `CoTransformer` on the left.
transformCoTransformL
  :: forall i1 i2 o f m a
   . MonadRec m
  => Parallel f m
  => Transformer i1 i2 m a
  -> CoTransformer i2 o m a
  -> CoTransformer i1 o m a
transformCoTransformL = fuseWith \f (Transform t) (CoTransform o c) ->
  CoTransform o \i1 ->
    case t i1 of
      Tuple i2 a -> f a (c i2)

-- | Transform a `CoTransformer` on the right.
transformCoTransformR
  :: forall i o1 o2 f m a
   . MonadRec m
  => Parallel f m
  => CoTransformer i o1 m a
  -> Transformer o1 o2 m a
  -> CoTransformer i o2 m a
transformCoTransformR = fuseWith \f (CoTransform o1 c) (Transform t) ->
  case t o1 of
    Tuple o2 a -> CoTransform o2 ((_ `f` a) <<< c)

-- | Fuse a transformer and a cotransformer.
fuseCoTransform :: forall i o f m a. MonadRec m => Parallel f m => Transformer i o m a -> CoTransformer o i m a -> Process m a
fuseCoTransform = fuseWith \f (Transform t) (CoTransform i c) -> Identity (case t i of Tuple o a -> f a (c o))

-- | Connect a producer and a consumer.
connect :: forall o f m a. MonadRec m => Parallel f m => Producer o m a -> Consumer o m a -> Process m a
connect = fuseWith \f (Emit e a) (Await c) -> Identity (f a (c e))

infixr 2 connect as $$

-- | Connect a producer and a consumer so that the consumer pulls from the
-- | producer. This means the process ends immediately when the consumer closes.
pullFrom :: forall o m a. MonadRec m => Consumer o m a -> Producer o m a -> Process m a
pullFrom = fuseWithL \f (Await c) (Emit e a) -> pure (f (c e) a)

-- | Transform a producer.
transformProducer :: forall i o f m a. MonadRec m => Parallel f m => Producer i m a -> Transformer i o m a -> Producer o m a
transformProducer = fuseWith \f (Emit i a) (Transform t) -> case t i of Tuple o b -> Emit o (f a b)

infixr 2 transformProducer as $~

-- | Transform a consumer.
transformConsumer :: forall i o f m a. MonadRec m => Parallel f m => Transformer i o m a -> Consumer o m a -> Consumer i m a
transformConsumer = fuseWith \f (Transform t) (Await g) -> Await \i -> case t i of Tuple o a -> f a (g o)

infixr 2 transformConsumer as ~$

-- | Compose transformers
composeTransformers :: forall i j k f m a. MonadRec m => Parallel f m => Transformer i j m a -> Transformer j k m a -> Transformer i k m a
composeTransformers = fuseWith \f (Transform g) (Transform h) -> Transform \i -> case g i of Tuple j a -> case h j of Tuple k b -> Tuple k (f a b)

infixr 2 composeTransformers as ~~

-- | Compose cotransformers
composeCoTransformers :: forall i j k f m a. MonadRec m => Parallel f m => CoTransformer i j m a -> CoTransformer j k m a -> CoTransformer i k m a
composeCoTransformers = fuseWith \f (CoTransform j g) (CoTransform k h) ->
  CoTransform k (\i -> f (g i) (h j))

-- | Run two producers together.
joinProducers :: forall o1 o2 f m a. MonadRec m => Parallel f m => Producer o1 m a -> Producer o2 m a -> Producer (Tuple o1 o2) m a
joinProducers = fuseWith \f (Emit o1 a) (Emit o2 b) -> Emit (Tuple o1 o2) (f a b)

infixr 3 joinProducers as /\

-- | Run two consumers together
joinConsumers :: forall i1 i2 f m a. MonadRec m => Parallel f m => Consumer i1 m a -> Consumer i2 m a -> Consumer (Tuple i1 i2) m a
joinConsumers = fuseWith \f (Await k1) (Await k2) -> Await \(Tuple i1 i2) -> f (k1 i1) (k2 i2)

infixr 3 joinConsumers as \/
