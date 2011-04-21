module Sound.SC3.Mixer where

import           Reactive hiding (accumulate)
import qualified Sound.SC3.Server.Allocator.Range as Range
import           Sound.SC3.Server.Reactive
import           Sound.SC3.Server.Monad.Command
import           Sound.OpenSoundControl (immediately)

data Redirect = Redirect {
    redirectGroup :: NodeId
  } deriving (Show)

data Fader = Fader {
    faderGroup :: Group
  , faderSynth :: Synth
  } deriving (Show)

data Strip = Strip {
    group :: Group
  , inputGroup :: Group
  -- , preFaderRedirect :: Redirect
  -- , fader :: Fader
  -- , postFaderRedirect :: Redirect
  , bus :: AudioBus
  } deriving (Show)

data Command = Command

mkStrip :: Server Strip
mkStrip = do
    b <- newAudioBus 2
    exec immediately $ do
        g <- g_new_ AddToTail
        ig <- g_new AddToTail g
        return $ Strip g ig b

-- data Mixer = Mixer

-- mixer :: Event Command -> Server (Behavior Mixer)
-- mixer = undefined

-- data Mixer = Mixer {
--     channels :: [(String, Channel)]
--   }

-- main = putStrLn "MixiMixi"
