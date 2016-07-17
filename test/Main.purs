module Test.Main where

import Prelude

import Control.Coroutine (Transformer, CoTransformer, Consumer, Producer,
                          runProcess, transform, fuseCoTransform, cotransform,
                          await, emit, (~$), ($~), ($$), (/\))
import Control.Monad.Aff (Aff, later', launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans (lift)
import Data.Functor (($>))
import Data.Maybe (Maybe(..))

nats :: forall eff. Producer Int (Aff eff) Unit
nats = go 0
  where
  go i = do
    emit i
    lift (later' 10 (pure unit)) -- 10ms delay
    go (i + 1)

printer :: forall eff. Consumer String (Aff (console :: CONSOLE | eff)) Unit
printer = forever do
  s <- await
  lift (liftEff (log s))
  pure Nothing

showing :: forall a m. (Show a, Monad m) => Transformer a String m Unit
showing = forever (transform show)

coshowing :: forall eff. CoTransformer String Int (Aff (console :: CONSOLE | eff)) Unit
coshowing = go 0
  where
  go i = do
    o <- cotransform i
    lift (liftEff (log o))
    go (i + 1)

main :: forall eff. Eff (err :: EXCEPTION, console :: CONSOLE | eff) Unit
main = void $ launchAff $
  -- runProcess (showing `fuseCoTransform` coshowing)))
  -- runProcess ((nats $~ showing) $$ printer)
  runProcess (nats /\ nats $$ showing ~$ printer)
