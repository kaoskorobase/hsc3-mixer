module Sound.SC3.Mixer where

import           Reactive hiding (accumulate)
import qualified Sound.SC3.Server.Allocator.Range as Range
import           Sound.SC3.Server.Monad
import           Sound.SC3.Server.Reactive
import           Sound.SC3.Server.Resource
import           Sound.OpenSoundControl

data Redirect = Redirect {
    redirectGroup :: NodeId
  } deriving (Show)

data Fader = Fader {
    faderGroup :: NodeId
  , faderSynth :: NodeId
  } deriving (Show)

data Strip = Strip {
    group :: Group
  , inputGroup :: Group
  -- , preFaderRedirect :: Redirect
  -- , fader :: Fader
  -- , postFaderRedirect :: Redirect
  -- , bus :: Bus
  } deriving (Show)

data Command = Command

mkStrip :: Server Strip
mkStrip = do
    r <- rootNode
    async' immediately $ do
        g <- g_new AddToTail (Group r)
        ig <- g_new AddToTail g
        return $ Strip g ig

-- data Mixer = Mixer

-- mixer :: Event Command -> Server (Behavior Mixer)
-- mixer = undefined

-- data Mixer = Mixer {
--     channels :: [(String, Channel)]
--   }

-- main = putStrLn "MixiMixi"
