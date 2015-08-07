-- | This module defines types and functions for working with coroutines.
-- | Coroutines are defined based on some underlying functor, which means that
-- | the same machinery can be used for coroutines which emit values, await values,
-- | fork, join, or any combination.

module Control.Coroutine
  ( Co(), Process()
  , liftCo
  , hoistCo, interpret, bimapCo
  , loop
  , runCo, runProcess
  , fuseWith
  , Emit(..), Producer(), emit, producer
  , Await(..), Consumer(), await, consumer
  , Transform(..), Transformer(), transform
  , ($$), ($~), (~$), (~~)
  ) where

import Prelude

import Data.Maybe
import Data.Tuple
import Data.Exists
import Data.Either
import qualified Data.Bifunctor as B
import qualified Data.Profunctor as P
import Data.Identity
import Data.Functor (($>))

import Control.Bind ((<=<))
import Control.Monad.Trans
import Control.Monad.Rec.Class

-- | Instead of implementing `bind` directly, we capture the bind using this data structure, to
-- | evaluate later.
data Bound f m b a = Bound (Unit -> Co f m a) (a -> Co f m b)

-- | Capture a `bind` operation for the `Co` monad.
bound :: forall f m a b. (Unit -> Co f m a) -> (a -> Co f m b) -> Co f m b
bound m f = Bind (mkExists (Bound m f))

-- | A coroutine whose commands are given by the functor `f`, with side effects
-- | at each step given by the monad `m`.
data Co f m a = Co (Unit -> m (Either a (f (Co f m a)))) | Bind (Exists (Bound f m a))

-- | Unpack a `Co`, exposing the first step of the computation.
resume :: forall f m a. (Functor f, MonadRec m) => Co f m a -> m (Either a (f (Co f m a)))
resume = tailRecM go
  where
  go :: Co f m a -> m (Either (Co f m a) (Either a (f (Co f m a))))
  go (Co f) = map Right (f unit)
  go (Bind e) = runExists (\(Bound m f) ->
    case m unit of
      Co m -> do
        e <- m unit
        case e of
          Left a -> return (Left (f a))
          Right fc -> return (Right (Right (map (\h -> h >>= f) fc)))
      Bind e1 -> runExists (\(Bound m1 f1) -> return (Left (bind (m1 unit) (\z -> f1 z >>= f)))) e1) e

-- | A `Process` is a `Co`routine which only has side effects, and supports no commands.
type Process = Co Identity

instance functorCo :: (Functor f, Functor m) => Functor (Co f m) where
  map f (Co m) = Co \_ -> map (B.bimap f (map (map f))) (m unit)
  map f (Bind e) = runExists (\(Bound a k) -> bound a (map f <<< k)) e

instance applyCo :: (Functor f, Monad m) => Apply (Co f m) where
  apply = ap

instance applicativeCo :: (Functor f, Monad m) => Applicative (Co f m) where
  pure a = Co \_ -> pure (Left a)

instance bindCo :: (Functor f, Monad m) => Bind (Co f m) where
  bind (Bind e) f = runExists (\(Bound a k) -> bound a (\x -> bound (\_ -> k x) f)) e
  bind a f = bound (\_ -> a) f

instance monadCo :: (Functor f, Monad m) => Monad (Co f m)

instance monadTransCo :: (Functor f) => MonadTrans (Co f) where
  lift ma = Co \_ -> map Left ma

instance monadRecCo :: (Functor f, Monad m) => MonadRec (Co f m) where
  tailRecM f = go
    where
    go s = do
      e <- f s
      case e of
        Left s1 -> go s1
        Right a -> return a

-- | Lift a command from the functor `f` into a one-step `Co`routine.
liftCo :: forall f m a. (Functor f, Monad m) => f a -> Co f m a
liftCo fa = Co \_ -> return (Right (map pure fa))

-- | Change the underlying `Monad` for a `Co`routine.
hoistCo :: forall f m n a. (Functor f, Functor n) => (forall a. m a -> n a) -> Co f m a -> Co f n a
hoistCo = bimapCo id

-- | Change the functor `f` for a `Co`routine.
interpret :: forall f g m a. (Functor f, Functor m) => (forall a. f a -> g a) -> Co f m a -> Co g m a
interpret nf = bimapCo nf id

-- | Change the functor `f` and the underlying `Monad` for a `Co`routine.
bimapCo :: forall f g m n a. (Functor f, Functor n) => (forall a. f a -> g a) -> (forall a. m a -> n a) -> Co f m a -> Co g n a
bimapCo nf nm (Bind e) = runExists (\(Bound a f) -> bound (bimapCo nf nm <<< a) (bimapCo nf nm <<< f)) e
bimapCo nf nm (Co m) = Co \_ -> map (nf <<< map (bimapCo nf nm)) <$> nm (m unit)

-- | Loop until the computation returns a `Just`.
loop :: forall f m a b. (Functor f, Monad m) => Co f m (Maybe a) -> Co f m a
loop me = tailRecM (\_ -> map (maybe (Left unit) Right) me) unit

-- | Run a `Co`routine to completion.
runCo :: forall f m a. (Functor f, MonadRec m) => (forall a. f a -> m a) -> Co f m a -> m a
runCo interp = tailRecM (go <=< resume)
  where
  go :: Either a (f (Co f m a)) -> m (Either (Co f m a) a)
  go (Left a) = return (Right a)
  go (Right fc) = do
    c <- interp fc
    return (Left c)

-- | Run a `Process` to completion.
runProcess :: forall m a. (MonadRec m) => Process m a -> m a
runProcess = runCo (return <<< runIdentity)

-- | Fuse two `Co`routines.
fuseWith :: forall f g h m a. (Functor f, Functor g, Functor h, MonadRec m) =>
                              (forall a b c. (a -> b -> c) -> f a -> g b -> h c) ->
                              Co f m a ->
                              Co g m a ->
                              Co h m a
fuseWith zap fs gs = Co \_ -> go (Tuple fs gs)
  where
  go :: Tuple (Co f m a) (Co g m a) -> m (Either a (h (Co h m a)))
  go (Tuple fs gs) = do
    e1 <- resume fs
    e2 <- resume gs
    case zap Tuple <$> e1 <*> e2 of
      Left a -> return (Left a)
      Right o -> return (Right (map (\t -> Co \_ -> go t) o))

-- | A generating functor for emitting output values.
data Emit o a = Emit o a

instance bifunctorEmit :: B.Bifunctor Emit where
  bimap f g (Emit o a) = Emit (f o) (g a)

instance functorEmit :: Functor (Emit o) where
  map = B.rmap

-- | A type synonym for a `Co`routine which only emits values.
type Producer o = Co (Emit o)

-- | Emit an output value.
emit :: forall m o. (Monad m) => o -> Producer o m Unit
emit o = liftCo (Emit o unit)

-- | Create a `Producer` by providing a monadic function that produces values.
-- |
-- | The function should return a value of type `r` at most once, when the
-- | `Producer` is ready to close.
producer :: forall o m r. (Monad m) => m (Either o r) -> Producer o m r
producer recv = loop do
  e <- lift recv
  case e of
    Left o -> emit o $> Nothing
    Right r -> return (Just r)

-- | A generating functor for awaiting input values.
newtype Await i a = Await (i -> a)

instance profunctorAwait :: P.Profunctor Await where
  dimap f g (Await k) = Await (P.dimap f g k)

instance functorAwait :: Functor (Await i) where
  map = P.rmap

-- | A type synonym for a `Co`routine which only awaits values.
type Consumer i = Co (Await i)

-- | Await an input value.
await :: forall m i. (Monad m) => Consumer i m i
await = liftCo (Await id)

-- | Create a `Consumer` by providing a handler function which consumes values.
-- |
-- | The handler function should return a value of type `r` at most once, when the
-- | `Consumer` is ready to close.
consumer :: forall i m r. (Monad m) => (i -> m (Maybe r)) -> Consumer i m r
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
transform :: forall m i o. (Monad m) => (i -> o) -> Transformer i o m Unit
transform f = liftCo (Transform \i -> Tuple (f i) unit)

-- | Connect a producer and a consumer.
($$) :: forall o m a. (MonadRec m) => Producer o m a -> Consumer o m a -> Process m a
($$) = fuseWith \f (Emit e a) (Await c) -> Identity (f a (c e))

-- | Transform a producer.
($~) :: forall i o m a. (MonadRec m) => Producer i m a -> Transformer i o m a -> Producer o m a
($~) = fuseWith \f (Emit i a) (Transform t) -> case t i of Tuple o b -> Emit o (f a b)

-- | Transform a consumer.
(~$) :: forall i o m a. (MonadRec m) => Transformer i o m a -> Consumer o m a -> Consumer i m a
(~$) = fuseWith \f (Transform t) (Await g) -> Await \i -> case t i of Tuple o a -> f a (g o)

-- | Compose transformers
(~~) :: forall i j k m a. (MonadRec m) => Transformer i j m a -> Transformer j k m a -> Transformer i k m a
(~~) = fuseWith \f (Transform g) (Transform h) -> Transform \i -> case g i of Tuple j a -> case h j of Tuple k b -> Tuple k (f a b)
