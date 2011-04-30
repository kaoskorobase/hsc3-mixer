module Sound.SC3.Mixer where

import           Control.Applicative
import           Control.Monad (liftM)
import           Control.Monad.IO.Class (MonadIO)
import           Reactive hiding (accumulate)
import           Sound.SC3.Mixer.SynthDefs
import           Sound.SC3.Server.Reactive
import           Sound.SC3.Server.Monad.Command
import           Sound.OpenSoundControl (immediately)

data Redirect = Redirect {
    redirectGroup :: Group
  } deriving (Show)

data Fader = Fader {
    faderGroup :: Group
  , faderSynth :: Synth
  } deriving (Show)

data Strip = Strip {
    bus :: AudioBus
  , group :: Group
  , inputGroup :: Group
  , preFaderRedirect :: Redirect
  , fader :: Fader
  , postFaderRedirect :: Redirect
  } deriving (Show)

data Command = Command

mkRedirect :: MonadIO m => Group -> AudioBus -> SendT m Redirect
mkRedirect g _ = liftM Redirect (g_new AddToTail g)

mkFader :: MonadIO m => Group -> AudioBus -> SendT m (Deferred Fader)
mkFader parent bus = do
    g <- g_new AddToTail parent
    d_new "fader" (faderDef (numChannels bus)) `whenDone` \d -> do
        s <- s_new d AddToTail g [ ("bus", fromIntegral (busId bus)) ]
        return $ pure $ Fader g s

data OutputBus = Hardware Int Int | Private Int deriving (Eq, Show)

mkStrip :: MonadIO m => OutputBus -> SendT m (Deferred Strip)
mkStrip o = do
    b <- case o of
            Hardware n i -> outputBus n i
            Private n    -> newAudioBus n
    g <- g_new_ AddToTail
    ig <- g_new AddToTail g
    r1 <- mkRedirect g b
    f <- mkFader g b
    r2 <- mkRedirect g b
    return $ pure (Strip b g ig r1) <*> f <*> pure r2

setLevel :: MonadIO m => Double -> Strip -> SendT m ()
setLevel x s = n_set (faderSynth (fader s)) [("level", x)]

setMute :: MonadIO m => Bool -> Strip -> SendT m ()
setMute x s = n_set (faderSynth (fader s)) [("mute", fromIntegral (fromEnum x :: Int))]

play :: MonadIO m => Strip -> SynthDef -> AddAction -> [(String, Double)] -> SendT m Synth
play s d a xs = s_new d a (inputGroup s) (("out", fromIntegral (busId (bus s))):xs)

-- data Mixer = Mixer

-- mixer :: Event Command -> Server (Behavior Mixer)
-- mixer = undefined

-- data Mixer = Mixer {
--     channels :: [(String, Channel)]
--   }

-- main = putStrLn "MixiMixi"
