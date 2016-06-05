module Test.Main where

import Prelude

import Control.Coroutine (Transformer, Consumer, Producer, runProcess, transform, consumer, emit, (~$), ($$), (/\))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Rec.Class (forever)
import Data.Functor (($>))
import Data.Maybe (Maybe(..))

nats :: forall m. Monad m => Producer Int m Unit
nats = go 0
  where
  go i = do
    emit i
    go (i + 1)

printer :: forall eff. Consumer String (Eff (console :: CONSOLE | eff)) Unit
printer = consumer \s -> log s $> Nothing

showing :: forall a m. (Show a, Monad m) => Transformer a String m Unit
showing = forever (transform show)

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = runProcess (nats /\ nats $$ showing ~$ printer)
