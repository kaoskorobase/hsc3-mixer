module Sound.SC3.Mixer where

import           Control.Applicative
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

-- data MultiChannelSynthDef = MultiChannelSynthDef String (Int -> UGen) [(Int, SynthDef)]
-- 
-- mkFader :: MonadIO m => Group -> AudioBus -> ServerT m Fader
-- mkFader parent = do
--     immediately !> do
--         g <- g_new AddToTail parent
--         d <- d_new "fader" faderDef `whenDone` immediately $ do
--             s <- s_new d AddToTail g []
--             sync
--             return $ Fader g s
        
mkStrip :: Server Strip
mkStrip = do
    b <- newAudioBus 2
    exec immediately $ do
        g <- g_new_ AddToTail
        ig <- g_new AddToTail g
        return $ pure $ Strip g ig b

-- data Mixer = Mixer

-- mixer :: Event Command -> Server (Behavior Mixer)
-- mixer = undefined

-- data Mixer = Mixer {
--     channels :: [(String, Channel)]
--   }

-- main = putStrLn "MixiMixi"
