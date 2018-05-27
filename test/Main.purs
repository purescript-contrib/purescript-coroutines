module Test.Main where

import Prelude

import Control.Coroutine (($$), ($~), (~$), (/\))
import Control.Coroutine as Co
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff)
import Effect.Class (liftEffect)
import Effect.Console (log)

nats :: Co.Producer Int Aff Unit
nats = go 0
  where
  go i = do
    Co.emit i
    lift (delay (wrap 10.0)) -- 10ms delay
    go (i + 1)

printer :: Co.Consumer String Aff Unit
printer = forever do
  s <- Co.await
  lift (liftEffect (log s))
  pure Nothing

showing :: forall a m. Show a => Monad m => Co.Transformer a String m Unit
showing = forever (Co.transform show)

coshowing :: Co.CoTransformer String Int Aff Unit
coshowing = go 0
  where
  go i = do
    o <- Co.cotransform i
    lift (liftEffect (log o))
    go (i + 1)

main :: Effect Unit
main = void $ launchAff do
  Co.runProcess (showing `Co.fuseCoTransform` coshowing)
  Co.runProcess ((nats $~ showing) $$ printer)
  Co.runProcess (nats /\ nats $$ showing ~$ printer)
