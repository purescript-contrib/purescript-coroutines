module Test.Main where

import Prelude

import Data.Either

import Control.Coroutine
import Control.Monad.Aff
import Control.Monad.Aff.Console
import Control.Monad.Trans

producer :: forall m a. (Monad m) => Producer Int m Unit
producer = stateful inc 0
  where
  inc i = do
    return (Right (i + 1))
    
consumer :: forall a. Consumer Int (Aff _) Unit
consumer = repeatedly do
  s <- await
  lift (print s)

main = launchAff $ runProcess (producer >~> consumer)