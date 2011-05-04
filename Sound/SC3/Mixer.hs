module Sound.SC3.Mixer where

import           Control.Applicative
import           Control.Monad (liftM)
import           Control.Monad.IO.Class (MonadIO)
-- import           Reactive hiding (accumulate)
import           Sound.SC3.Mixer.SynthDefs
-- import           Sound.SC3.Server.Reactive
import           Sound.SC3.Server.Monad.Command
import           Sound.OpenSoundControl (immediately)

data Redirect = Redirect {
    redirectGroup :: Group
  } deriving (Show)

data Fader = Fader {
    faderGroup :: Group
  , faderSynth :: Synth
  , faderLevel :: Double
  , faderMute :: Bool
  } deriving (Show)

data Strip = Strip {
    bus :: AudioBus
  , group :: Group
  , inputGroup :: Group
  , preFaderRedirect :: Redirect
  , fader :: Fader
  , postFaderRedirect :: Redirect
  } deriving (Show)

mkRedirect :: MonadIO m => Group -> AudioBus -> SendT m Redirect
mkRedirect g _ = liftM Redirect (g_new AddToTail g)

mkFader :: MonadIO m => Group -> AudioBus -> Async m Fader
mkFader parent bus = do
    d_new "fader" (faderDef (numChannels bus)) `whenDone` \d -> do
        g <- g_new AddToTail parent
        s <- s_new d AddToTail g [ ("bus", fromIntegral (busId bus)) ]
        return $ Fader g s 0 False

mkStrip :: MonadIO m => Int -> SendT m (Deferred m Strip)
mkStrip n = do
    b <- newAudioBus n
    g <- g_new_ AddToTail
    ig <- g_new AddToTail g
    r1 <- mkRedirect g b
    f <- async $ mkFader g b
    r2 <- mkRedirect g b
    return $ pure (Strip b g ig r1) <*> f <*> pure r2

setLevel :: MonadIO m => Double -> Strip -> SendT m Strip
setLevel x s = do
    n_set (faderSynth (fader s)) [("level", x)]
    return $ s { fader = (fader s) { faderLevel = x} }

setMute :: MonadIO m => Bool -> Strip -> SendT m Strip
setMute x s = do
    n_set (faderSynth (fader s)) [("mute", fromIntegral (fromEnum x :: Int))]
    return $ s { fader = (fader s) { faderMute = x} }

play :: MonadIO m => Strip -> SynthDef -> AddAction -> [(String, Double)] -> SendT m Synth
play s d a xs = s_new d a (inputGroup s) (("out", fromIntegral (busId (bus s))):xs)

data Connection = Connection

class ExternalNode a where
    isExternal :: a -> Bool

data InputNode =
    InputNode AudioBus
  | InputStrip Strip

instance ExternalNode InputNode where
    isExternal (InputNode _) = True
    isExternal _ = False

data OutputNode =
    OutputNode AudioBus
  | OutputStrip Strip

instance ExternalNode OutputNode where
    isExternal (OutputNode _) = True
    isExternal _ = False

inputNodeBus :: InputNode -> AudioBus
inputNodeBus (InputNode b) = b
inputNodeBus (InputStrip s) = bus s

inputNodeGroup :: MonadIdAllocator m => InputNode -> m Group
inputNodeGroup (InputNode _) = rootNode
inputNodeGroup (InputStrip s) = return (group s)

outputNodeBus :: OutputNode -> AudioBus
outputNodeBus (OutputNode b) = b
outputNodeBus (OutputStrip s) = bus s

-- instance Eq MixerNode where
--     (HardwareNode b1) == (HardwareNode b2) = b1 == b2
--     (StripNode s1) == (StripNode s2) = bus s1 == bus s2
--     _ == _ = False

connect :: MonadIO m => (InputNode, Int) -> (OutputNode, Int) -> Async m Synth
connect (input, j) (output, k) =
    d_new "patchCord" patchCord `whenDone` \d -> do
        g <- inputNodeGroup input
        s <- s_new d AddToTail g [ ("in", fromIntegral (busId (inputNodeBus input) + fromIntegral j))
                                 , ("out", fromIntegral (busId (outputNodeBus output) + fromIntegral k)) ]
        return s

-- data Mixer = Mixer

-- mixer :: Event Command -> Server (Behavior Mixer)
-- mixer = undefined

-- data Mixer = Mixer {
--     channels :: [(String, Channel)]
--   }

-- main = putStrLn "MixiMixi"
