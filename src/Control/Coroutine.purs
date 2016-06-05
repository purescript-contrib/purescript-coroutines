-- | This module defines types and functions for working with coroutines.
-- | Coroutines are defined based on some underlying functor, which means that
-- | the same machinery can be used for coroutines which emit values, await values,
-- | fork, join, or any combination.

module Control.Coroutine
  ( Co()
  , Process()
  , loop
  , runProcess
  , fuseWith
  , Emit(..), Producer(), emit, producer
  , Await(..), Consumer(), await, consumer
  , Transform(..), Transformer(), transform
  , connect, ($$)
  , transformProducer, ($~)
  , transformConsumer, (~$)
  , composeTransformers, (~~)
  , joinProducers, (/\)
  , joinConsumers, (\/)
  ) where

import Prelude

import Control.Monad.Free.Trans (FreeT, liftFreeT, freeT, resume, runFreeT)
import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Control.Monad.Trans (lift)
import Data.Bifunctor as B
import Data.Either (Either(..))
import Data.Functor (($>))
import Data.Identity (Identity(..), runIdentity)
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor as P
import Data.Tuple (Tuple(..))

-- | A coroutine whose commands are given by the functor `f`, with side effects
-- | at each step given by the monad `m`.
type Co = FreeT

-- | A `Process` is a `Co`routine which only has side effects, and supports no commands.
type Process = Co Identity

-- | Loop until the computation pures a `Just`.
loop :: forall f m a. (Functor f, Monad m) => Co f m (Maybe a) -> Co f m a
loop me = tailRecM (\_ -> map (maybe (Left unit) Right) me) unit

-- | Run a `Process` to completion.
runProcess :: forall m a. MonadRec m => Process m a -> m a
runProcess = runFreeT (pure <<< runIdentity)

-- | Fuse two `Co`routines.
fuseWith :: forall f g h m a. (Functor f, Functor g, Functor h, MonadRec m) =>
                              (forall b c d. (b -> c -> d) -> f b -> g c -> h d) ->
                              Co f m a ->
                              Co g m a ->
                              Co h m a
fuseWith zap fs gs = freeT \_ -> go (Tuple fs gs)
  where
  go :: Tuple (Co f m a) (Co g m a) -> m (Either a (h (Co h m a)))
  go (Tuple fs gs) = do
    e2 <- resume gs
    e1 <- resume fs
    case zap Tuple <$> e1 <*> e2 of
      Left a -> pure (Left a)
      Right o -> pure (Right (map (\t -> freeT \_ -> go t) o))
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
-- | The function should pure a value of type `r` at most once, when the
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
-- | The handler function should pure a value of type `r` at most once, when the
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

-- | Connect a producer and a consumer.
connect :: forall o m a. MonadRec m => Producer o m a -> Consumer o m a -> Process m a
connect = fuseWith \f (Emit e a) (Await c) -> Identity (f a (c e))

infixr 2 connect as $$

-- | Transform a producer.
transformProducer :: forall i o m a. MonadRec m => Producer i m a -> Transformer i o m a -> Producer o m a
transformProducer = fuseWith \f (Emit i a) (Transform t) -> case t i of Tuple o b -> Emit o (f a b)

infixr 2 transformProducer as $~

-- | Transform a consumer.
transformConsumer :: forall i o m a. MonadRec m => Transformer i o m a -> Consumer o m a -> Consumer i m a
transformConsumer = fuseWith \f (Transform t) (Await g) -> Await \i -> case t i of Tuple o a -> f a (g o)

infixr 2 transformConsumer as ~$

-- | Compose transformers
composeTransformers :: forall i j k m a. MonadRec m => Transformer i j m a -> Transformer j k m a -> Transformer i k m a
composeTransformers = fuseWith \f (Transform g) (Transform h) -> Transform \i -> case g i of Tuple j a -> case h j of Tuple k b -> Tuple k (f a b)

infixr 2 composeTransformers as ~~

-- | Run two producers together.
joinProducers :: forall o1 o2 m a. MonadRec m => Producer o1 m a -> Producer o2 m a -> Producer (Tuple o1 o2) m a
joinProducers = fuseWith \f (Emit o1 a) (Emit o2 b) -> Emit (Tuple o1 o2) (f a b)

infixr 3 joinProducers as /\

-- | Run two consumers together
joinConsumers :: forall i1 i2 m a. MonadRec m => Consumer i1 m a -> Consumer i2 m a -> Consumer (Tuple i1 i2) m a
joinConsumers = fuseWith \f (Await k1) (Await k2) -> Await \(Tuple i1 i2) -> f (k1 i1) (k2 i2)

infixr 3 joinConsumers as \/
