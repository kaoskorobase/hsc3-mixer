{-# LANGUAGE Arrows, DeriveDataTypeable #-}
import           Control.Applicative
import           Control.Arrow
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Maybe
import           Data.Monoid
import           Data.Signal.SF as SF
import           Data.Typeable
import           Sound.SC3.Server.Process.Monad
import           Sound.SC3.Mixer
import           Sound.SC3.Server.Monad.Command
import           Sound.SC3.Server.Notification
import qualified Sound.SC3.Server.Command as C
import           Sound.OpenSoundControl (Datum(..), OSC(..), immediately)
import qualified Sound.OpenSoundControl as OSC
import qualified Sound.OpenSoundControl.Transport.UDP as OSC
import           System.Random

dt = 0.00125
dur = 0.02
gate = 0.2
lat = 0.03

playDefault :: MonadIO m => Double -> Strip -> ServerT m ()
playDefault _ s = do
    t <- liftIO $ OSC.utcr
    f <- liftIO $ randomRIO (399,401)
    synth <- OSC.UTCr (t + lat) ~> play s d_default AddToTail [("freq", f)]
    fork $ do
        -- let gate = dur/2
        let t' = t + gate
        liftIO $ OSC.pauseThreadUntil t'
        OSC.UTCr (t' + lat) ~> s_release (-1 - gate) synth
    return ()

mkOSCServer :: String -> Int -> (OSC -> IO ()) -> IO ()
mkOSCServer host port put = do
    t <- OSC.udpServer host port
    chan <- newChan
    void $ forkIO $ loop t
    where loop t = OSC.recv t >>= put >> loop t

address :: Monad m => String -> SF m (Event OSC) (Event OSC)
address cmd = SF.filter f
    where
        f (Message cmd' _) = cmd == cmd'
        f _ = False

-- filterMaybe :: Event (Maybe a) -> Event a
-- filterMaybe = fmap fromJust . R.filter isJust

oscFloatE :: Monad m => String -> SF m (Event OSC) (Event Double)
oscFloatE cmd = address cmd >>> arr (join . fmap f)
    where
        f (Message _ [d]) =
            case d of
                Float x' -> pure x'
                Double x' -> pure x'
                Int x' -> pure (fromIntegral x')
                _ -> mzero
        f _ = mzero

oscFloatB :: Monad m => String -> Double -> SF m (Event OSC) Double
oscFloatB cmd x0 = oscFloatE cmd >>> stepper x0

mapAccum :: MonadFix m => (acc -> x -> (acc, y)) -> acc -> SF m x (acc, y)
mapAccum f acc0 = SF (tf acc0)
    where
        tf acc x = let (acc', y) = f acc x in return ((acc', y), SF (tf acc'))

mapAccum_ :: MonadFix m => (acc -> x -> (acc, y)) -> acc -> SF m x y
mapAccum_ f acc0 = mapAccum f acc0 >>> arr snd

bang :: MonadFix m => (Double -> Double -> Bool) -> SF m Double Bool
-- bang f = SF.scanl g (Nothing, False) >>> arr snd -- mapAccum_ g Nothing
--     where
--         g (Nothing  , _) x = (Just x, False)
--         g ((Just x'), _) x = (Just x, f x' x)
bang f = mapAccum_ g Nothing
    where
        g Nothing x   = (Just x, False)
        g (Just x') x = (Just x, f x' x)

threshold :: Double -> Double -> Double -> Bool
threshold t x' x = x' < t && x > t || x < t && x' > t

-- toggle :: SF m Bool Event () -> Event Bool
-- toggle = accumE False . fmap (const not)

data StripInput m = StripInput {
    level :: Event Double
  , mute  :: Event Bool
  , playE  :: Event (Strip -> ServerT m ())
  }

data StripEvent m =
    Level Double
  | Mute Bool
  | Play (Strip -> ServerT m ())

instance Show (StripEvent m) where
    show (Level x) = "Level " ++ show x
    show (Mute x)  = "Mute " ++ show x
    show (Play _)  = "Play"

strip :: MonadIO m => Int -> SF (ServerT m) (Event (StripEvent m)) Strip
strip n = arr (fmap f) >>> accumM s0
    where
        s0 = do
            s0 <- immediately !> mkStrip n
            o <- outputBus 2 0
            immediately !> do
                async $ connect (InputStrip s0, 0) (OutputNode o, 0)
                async $ connect (InputStrip s0, 1) (OutputNode o, 1)
            return s0
    -- return $ accumE (return s0) (fmap f ((Level <$> level i) `mappend` (Mute <$> mute i) `mappend` (Play <$> event i)))
        f e s = do
            case e of
                Level l -> immediately ~> setLevel l s
                Mute m  -> immediately ~> setMute m s
                Play g  -> g s >> return s

class HasTimer a where
    timer :: a -> Maybe Double

instance HasTimer a => HasTimer (Event a) where
    timer = join . fmap timer . eventToMaybe

run :: Monad m => m a -> (b -> m ()) -> SF m a b -> m ()
run get put sf = do
    a <- get
    (b, sf') <- runSF sf a
    put b
    run get put sf'

stripInput :: (MonadFix m, MonadIO m) => SF (ServerT m) (Event OSC) (Event (StripEvent m))
stripInput = proc osc -> do
    level <- arr (fmap Level) <<< oscFloatE "/midi/cc3/1" -< osc
    -- mute  <- arr (fmap Mute) <<< edge <<< bang (threshold 0.5) <<< oscFloatB "/midi/cc4/1" 0 -< osc
    mute  <- arr (fmap (Mute . (>0.5))) <<< oscFloatE "/midi/cc4/1" -< osc
    playE <- arr (fmap Play) <<< arr (fmap playDefault) <<< oscFloatE "/midi/cc5/1" -< osc
    returnA -< (level `merge` mute `merge` playE)

main :: IO ()
-- main = return ()
main = do
    -- withDefaultSynth $ do
    src <- newChan
    mkOSCServer "127.0.0.1" 7800 (writeChan src)
    withDefaultSynth $ do
    -- withDefaultInternal $ do
        -- immediately ~> dumpOSC TextPrinter
        -- sync immediately $ send $ C.dumpOSC C.TextPrinter
        -- b <- accumulate (\t _ -> playDefault t) () osc
        -- osc <- liftM (fmap unOSC) $ fromEventSource src
        let s = stripInput >>> strip 2
        run (fmap Event $ liftIO $ readChan src) (liftIO.print) s
        -- liftIO $ reactimate $ fmap print $ changes $ level i
