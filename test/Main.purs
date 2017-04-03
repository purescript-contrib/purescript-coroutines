module Test.Main where

import Prelude

import Control.Coroutine as Co
import Control.Coroutine (($$), ($~), (~$), (/\))
import Control.Monad.Aff (Aff, delay, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)

nats :: forall eff. Co.Producer Int (Aff eff) Unit
nats = go 0
  where
  go i = do
    Co.emit i
    lift (delay (wrap 10.0)) -- 10ms delay
    go (i + 1)

printer :: forall eff. Co.Consumer String (Aff (console :: CONSOLE | eff)) Unit
printer = forever do
  s <- Co.await
  lift (liftEff (log s))
  pure Nothing

showing :: forall a m. Show a => Monad m => Co.Transformer a String m Unit
showing = forever (Co.transform show)

coshowing :: forall eff. Co.CoTransformer String Int (Aff (console :: CONSOLE | eff)) Unit
coshowing = go 0
  where
  go i = do
    o <- Co.cotransform i
    lift (liftEff (log o))
    go (i + 1)

main :: forall eff. Eff (exception :: EXCEPTION, console :: CONSOLE | eff) Unit
main = void $ launchAff do
  Co.runProcess (showing `Co.fuseCoTransform` coshowing)
  Co.runProcess ((nats $~ showing) $$ printer)
  Co.runProcess (nats /\ nats $$ showing ~$ printer)
