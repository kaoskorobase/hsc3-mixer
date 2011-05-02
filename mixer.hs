import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO)
import           Sound.SC3.Server.Process.Monad
import           Reactive hiding (accumulate)
import qualified Reactive as R
import           Sound.SC3.Mixer
import           Sound.SC3.Server.Monad.Command
import           Sound.SC3.Server.Notification
import           Sound.SC3.Server.Reactive
import qualified Sound.SC3.Server.Command as C
import           Sound.OpenSoundControl (Datum(..), OSC(..), immediately)
import qualified Sound.OpenSoundControl as OSC
import qualified Sound.OpenSoundControl.Transport.UDP as OSC
import           System.Random

dt = 0.00125
dur = 0.2
gate = 0.2
lat = 0.03

playDefault :: MonadIO m => Strip -> ServerT m ()
playDefault s = do
    t <- liftIO $ OSC.utcr
    f <- liftIO $ randomRIO (399,401)
    synth <- OSC.UTCr (t + lat) ~> play s d_default AddToTail [("freq", f)]
    fork $ do
        let t' = t + dur
        liftIO $ OSC.pauseThreadUntil t'
        OSC.UTCr (t' + lat) ~> s_release (-1 - gate) synth
    return ()

mkOSCServer :: String -> Int -> Prepare (Event OSC, Prepare ())
mkOSCServer host port = do
    t <- OSC.udpServer host port
    src <- newEventSource
    return (fromEventSource src, loop t src)
    where
        loop t src = do
            osc <- OSC.recv t
            fire src osc
            loop t src

address :: String -> Event OSC -> Event OSC
address cmd = R.filter f
    where
        f (Message cmd' _) = cmd == cmd'
        f _ = False

oscFloatB :: String -> Double -> Event OSC -> Behavior Double
oscFloatB cmd x0 = R.accumulate f x0
    where
        f (Message cmd' [d]) x
            | cmd == cmd' =
                case d of
                    Float x' -> x'
                    Double x' -> x'
                    Int x' -> fromIntegral x'
                    _ -> x
        f _ x = x

bang :: (Double -> Double -> Bool) -> Behavior Double -> Event ()
bang f = R.filterChanges . fmap snd . changes . R.accumulate' g (Nothing, undefined) . changes
    where
        g x (Nothing, _) = (Just x, Keep)
        g x (Just x', _) = (Just x, if f x' x then Change () else Keep)

threshold :: Double -> Double -> Double -> Bool
threshold t x' x = x' < t && x > t || x < t && x' > t

toggle :: Event () -> Behavior Bool
toggle = R.accumulate' (const not) False

data StripInput m = StripInput {
    level :: Behavior Double
  , mute  :: Behavior Bool
  , event :: Event (Strip -> ServerT m ())
  }

strip :: Int -> StripInput IO -> ServerT IO (Behavior Strip)
strip n i = do
    s0 <- immediately !> mkStrip n
    o <- outputBus 2 0
    immediately !> do
        async $ connect (InputStrip s0, 0) (OutputNode o, 0)
        async $ connect (InputStrip s0, 1) (OutputNode o, 1)
    accumulate f s0 (changes ((,) <$> level i <*> mute i) `merge` event i)
    where
        f e s = do
            case e of
                Left (l, m) -> immediately ~> do { setLevel l s ; setMute m s }
                Right g -> g s
            return s

main :: IO ()
main = do
    (osc, srv) <- mkOSCServer "127.0.0.1" 7800
    -- withDefaultSynth $ do
    withDefaultInternal $ do
        -- dumpOSC TextPrinter
        -- sync immediately $ send $ C.dumpOSC C.TextPrinter
        -- b <- accumulate (\t _ -> playDefault t) () osc
        let i = StripInput {
            level = oscFloatB "/midi/cc3/1" 0 osc
          , mute = toggle $ bang (threshold 0.5) $ oscFloatB "/midi/cc4/1" 0 osc
          , event = (const playDefault) `fmap` address "/midi/cc5/1" osc
          }
        b <- strip 2 i
        liftIO $ reactimate $ fmap (const (return ())) $ changes b
        -- liftIO $ reactimate $ fmap print $ changes $ level i
        liftIO srv
