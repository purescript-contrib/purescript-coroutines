module Test.Main where

import Prelude

import Control.Alternative (guard)
import Control.Coroutine (foldlProducer, foldrProducer, unfoldrProducer, ($$), ($~), (/\), (~$))
import Control.Coroutine as Co
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Array ((..))
import Data.Int (even)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff)
import Effect.Class (liftEffect)
import Effect.Console (log)

limit :: Int
limit = 100

nats :: Co.Producer Int Aff Unit
nats = Co.loop (go 0)
  where
  go i =
    if i >= limit then
      pure (Just unit)
    else do
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

showingCount
  :: forall a m. Show a => Monad m => Co.Transformer (Tuple Int a) String m Unit
showingCount = forever (Co.transform \(Tuple c a) -> show c <> ": " <> show a)

coshowing :: Co.CoTransformer String Int Aff Unit
coshowing = Co.loop (go 0)
  where
  go i =
    if i > limit then
      pure (Just unit)
    else do
      o <- Co.cotransform i
      lift (liftEffect (log o))
      go (i + 1)

collatz :: Int -> Co.Producer Int Aff Unit
collatz = unfoldrProducer \n -> do
  guard $ n /= 1
  let next a = Just (Tuple a a)
  next if even n then n / 2 else (3 * n + 1)

counting :: forall a m. Monad m => Co.Transformer a (Tuple Int a) m Unit
counting = Co.transformWithState 1 \counter a ->
  Tuple (counter + 1) (Tuple counter a)

main :: Effect Unit
main = void $ launchAff do
  Co.runProcess (showing `Co.fuseCoTransform` coshowing)
  Co.runProcess ((nats $~ showing) $$ printer)
  Co.runProcess (nats /\ nats $$ showing ~$ printer)
  Co.runProcess (collatz 39 $$ counting ~$ showingCount ~$ printer)
  Co.runProcess (foldrProducer (1 .. 3) $$ showing ~$ printer) -- 1, 2, 3
  Co.runProcess (foldlProducer (1 .. 3) $$ showing ~$ printer) -- 3, 2, 1
