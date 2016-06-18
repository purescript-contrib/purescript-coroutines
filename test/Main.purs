module Test.Main where

import Prelude

import Control.Coroutine (Transformer, CoTransformer, Consumer, Producer,
                          runProcess, transform, fuseCoTransform, cotransform, consumer, emit,
                          (~$), ($$), (/\))
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

coshowing :: CoTransformer String Int (Eff _) Unit
coshowing = go 0
  where
  go i = do
    cotransform i log
    go (i + 1)

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main =
  runProcess (showing `fuseCoTransform` coshowing)
  -- runProcess (nats $~ showing $$ printer)
  -- runProcess (nats /\ nats $$ showing ~$ printer)
