module Sound.SC3.Server.Reactive
  (
    accumulateChange
  , accumulate
  ) where

import           Reactive (Behavior, Change(..), Event)
import qualified Reactive as R
import Sound.SC3.Server.Connection (Connection)
import Sound.SC3.Server.Monad

accumulateChange :: (b -> a -> Server (Change a)) -> a -> Event b -> Server (Behavior a)
accumulateChange f a0 e = do
    c <- connection
    return $ R.accumulateIOChange (\b a -> runServer (f b a) c) a0 e

accumulate :: (b -> a -> Server a) -> a -> Event b -> Server (Behavior a)
accumulate f = accumulateChange (\b a -> fmap Change $ f b a)
