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
    emit i
    return (Right (i + 1))

-- | We hoist the consumer with `Aff`'s `later` function
-- | to make sure we don't run out of stack.
consumer :: forall a. Consumer Int (Aff _) Unit
consumer = hoistCo later $ repeatedly do
  s <- await
  lift (print s)

main = launchAff $ runProcess (producer >~> consumer)