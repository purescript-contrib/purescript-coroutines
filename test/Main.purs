module Test.Main where

import Prelude

import Data.Maybe
import Data.Either
import Data.Functor (($>))

import Control.Coroutine
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Trans
import Control.Monad.Rec.Class

nats :: forall m. (Monad m) => Producer Int m Unit
nats = go 0
  where
  go i = do
    emit i
    go (i + 1)
    
printer :: forall a. Consumer String (Eff _) Unit
printer = consumer \s -> log s $> Nothing
    
showing :: forall a m. (Show a, Monad m) => Transformer a String m Unit
showing = forever (transform show)

coshowing :: CoTransformer String Int (Eff _) Unit
coshowing = go 0
  where
  go i = do
    cotransform i log
    go (i + 1)

main =
  runProcess (showing `fuseCoTransform` coshowing)
  -- runProcess (nats $~ showing $$ printer)
