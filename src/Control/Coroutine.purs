-- | This module defines types and functions for working with coroutines.
-- | Coroutines are defined based on some underlying functor, which means that
-- | the same machinery can be used for coroutines which emit values, await values,
-- | fork, join, or any combination.

module Control.Coroutine
  ( Co(..)
  , Process()
  , unCo
  , hoistCo
  , stateful
  , loop
  , repeatedly
  , liftCo
  , runCo
  , runProcess
  , fuse
  , Emit(..)
  , Producer()
  , emit
  , Await(..)
  , Consumer()
  , await
  , emitAwait
  , feed
  , (>~>)
  , (<~<)
  ) where
      
import Prelude

import Data.Maybe
import Data.Either
import Data.Bifunctor (bimap)
import Data.Identity

import Control.Bind ((<=<))
import Control.Monad.Trans
import Control.Monad.Rec.Class

-- | A coroutine whose commands are given by the functor `f`, with side effects
-- | at each step given by the monad `m`.
newtype Co f m a = Co (Unit -> m (Either a (f (Co f m a))))

-- | Unpack a `Co`, exposing the first step of the computation.
unCo :: forall f m a. Co f m a -> m (Either a (f (Co f m a)))
unCo (Co f) = f unit

-- | A `Process` is a `Co`routine which only has side effects, and supports no commands.
type Process = Co Identity

instance functorCo :: (Functor f, Functor m) => Functor (Co f m) where
  map f c = Co \_ -> map (bimap f (map (map f))) (unCo c)
  
instance applyCo :: (Functor f, Monad m) => Apply (Co f m) where
  apply = ap
  
instance applicativeCo :: (Functor f, Monad m) => Applicative (Co f m) where
  pure a = Co \_ -> pure (Left a)
  
instance bindCo :: (Functor f, Monad m) => Bind (Co f m) where
  bind c f = Co \_ -> unCo c >>= go
    where
    go :: Either _ (f (Co f m _)) -> _ (Either _ (f (Co f m _)))
    go (Left a) = unCo (f a)
    go (Right fc) = return (Right (map (>>= f) fc))
  
instance monadCo :: (Functor f, Monad m) => Monad (Co f m)
  
instance monadTransCo :: (Functor f) => MonadTrans (Co f) where
  lift ma = Co \_ -> map Left ma

-- | Change the underlying `Monad` for a `Co`routine.
hoistCo :: forall f m n a. (Functor f, Functor n) => (forall a. m a -> n a) -> Co f m a -> Co f n a
hoistCo n co = Co \_ -> map (map (hoistCo n)) <$> n (unCo co)

-- | Construct a `Co`routine from a stateful updater function.
stateful :: forall f m a s. (Functor f, Monad m) => (s -> Co f m (Either a s)) -> s -> Co f m a
stateful f = go
  where
  go :: s -> Co f m a
  go s = do
    e <- f s
    case e of
      Left a -> return a
      Right s1 -> go s1
  
-- | Loop until the computation returns a `Just`.
loop :: forall f m a b. (Functor f, Monad m) => Co f m (Maybe a) -> Co f m a
loop me = stateful (\_ -> map (maybe (Right unit) Left) me) unit

-- | Loop indefinitely.
repeatedly :: forall f m a b. (Functor f, Monad m) => Co f m a -> Co f m b
repeatedly = loop <<< map (const Nothing)
    
-- | Lift a command from the functor `f` into a one-step `Co`routine.
liftCo :: forall f m a. (Functor f, Monad m) => f a -> Co f m a
liftCo fa = Co \_ -> return (Right (map pure fa))
  
-- | Run a `Co`routine to completion.
runCo :: forall f m a. (Functor f, MonadRec m) => (forall a. f a -> m a) -> Co f m a -> m a
runCo interp = tailRecM (go <=< unCo)
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
fuse :: forall f g m a. (Monad m) => (forall a b c. (a -> b -> c) -> f a -> g b -> c) -> 
                                     Co f m a -> 
                                     Co g m a -> 
                                     Process m a
fuse zap fs gs = Co \_ -> do
  e1 <- unCo fs
  e2 <- unCo gs
  case zap (fuse zap) <$> e1 <*> e2 of
    Left a -> return (Left a)
    Right c -> unCo c

-- | A generating functor for emitting output values.
data Emit o a = Emit o a

instance functorEmit :: Functor (Emit o) where
  map f (Emit o a) = Emit o (f a)
  
-- | A type synonym for a `Co`routine which only emits values.
type Producer o = Co (Emit o)

-- | Emit an output value.
emit :: forall m o. (Monad m) => o -> Producer o m Unit
emit o = liftCo (Emit o unit)

-- | A generating functor for awaiting input values.
newtype Await i a = Await (i -> a)

instance functorAwait :: Functor (Await i) where
  map f (Await k) = Await (f <<< k)
  
-- | A type synonym for a `Co`routine which only awaits values.
type Consumer i = Co (Await i)

-- | Await an input value.
await :: forall m i. (Monad m) => Consumer i m i
await = liftCo (Await id)

-- | Fuse the `Emit` and `Await` functors.
emitAwait :: forall e a b c. (a -> b -> c) -> Emit e a -> Await e b -> c
emitAwait f (Emit e a) (Await c) = f a (c e)

-- | Feed the values produced by a producer into a consumer.
feed :: forall e m a. (Monad m) => Producer e m a -> Consumer e m a -> Process m a 
feed = fuse emitAwait

-- | Infix version of `feed`.
(>~>) :: forall e m a. (Monad m) => Producer e m a -> Consumer e m a -> Process m a 
(>~>) = feed

-- | Infix version of `flip feed`.
(<~<) :: forall e m a. (Monad m) => Consumer e m a -> Producer e m a -> Process m a 
(<~<) = flip feed