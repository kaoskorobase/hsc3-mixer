import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Fix (MonadFix(..))
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Class (lift)
import           FRP.Elerea.Param as FRP
import           Sound.SC3.Server.Process.Monad
import           Sound.SC3.Mixer
import           Sound.SC3.Server.Monad.Command
import           Sound.SC3.Server.Notification
import qualified Sound.SC3.Server.Command as C
import           Sound.OpenSoundControl (Datum(..), OSC(..), immediately)
import qualified Sound.OpenSoundControl as OSC
import qualified Sound.OpenSoundControl.Transport.UDP as OSC
import           System.Random
import           Debug.Trace

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

mkOSCServer :: String -> Int -> IO (Signal (Maybe OSC), IO ())
mkOSCServer host port = do
    t <- OSC.udpServer host port
    (sig, snk) <- external Nothing
    return (sig, OSC.recv t >>= snk . Just)

filterMaybe :: (a -> Bool) -> Signal (Maybe a) -> Signal (Maybe a)
filterMaybe f = fmap g
    where
        g Nothing  = Nothing
        g (Just a) = if f a then Just a else Nothing

address :: String -> Signal (Maybe OSC) -> Signal (Maybe OSC)
address cmd = filterMaybe f
    where
        f (Message cmd' _) = cmd == cmd'
        f _ = False

oscFloatB :: String -> Double -> Signal (Maybe OSC) -> SignalGen p (Signal Double)
oscFloatB cmd x0 = transfer x0 (const f)
    where
        f m x =
            case m of
                Just osc ->
                    case osc of
                        Message cmd' [d] ->
                            if cmd == cmd'
                                then case d of
                                        Float x'  -> x'
                                        Double x' -> x'
                                        Int x'    -> fromIntegral x'
                                        _ -> x
                                else x
                        _ -> x
                Nothing -> x

bang :: (Double -> Double -> Bool) -> Double -> Signal Double -> SignalGen p (Signal Bool)
bang f x0 x = do
    x' <- delay x0 x
    return $ f <$> x' <*> x
-- R.filterChanges
--             =<< R.map snd . changes
--             =<< R.accumulate g (Nothing, undefined) (changes b)
    -- where
    --     g x x' _ = (Just x, False)
    --     g x x' _ = (Just x, f x' x)

threshold :: Double -> Double -> Double -> Bool
threshold t x' x = x' < t && x > t || x < t && x' > t

toggle :: Signal Bool -> SignalGen p (Signal Bool)
toggle = transfer False (const (const not))

data StripInput = StripInput {
    level :: Signal Double
  , mute  :: Signal Bool
  , event :: Signal (Maybe (Strip -> Server ()))
  }

zipS :: Signal a -> Signal b -> Signal (a, b)
zipS a b = (,) <$> a <*> b

zip3S :: Signal a -> Signal b -> Signal c -> Signal (a, b, c)
zip3S a b c = (,,) <$> a <*> b <*> c

strip :: Int -> StripInput -> SignalGen Connection (Signal Strip)
strip n i = do
    s0 <- do
        s <- immediately !> mkStrip n
        o <- outputBus 2 0
        immediately !> do
            async $ connect (InputStrip s, 0) (OutputNode o, 0)
            async $ connect (InputStrip s, 1) (OutputNode o, 1)
        return s
    c <- connection
    transferIO s0 (runServerT f c) $ zip3S (level i) (mute i) (event i)
    where
        f (l, m, g) s = do
            liftIO $ print (l, m)
            s' <- immediately ~> do { setLevel l s >>= setMute m }
            maybe (return ()) ($s') g
            liftIO $ print (faderLevel (fader s), faderLevel (fader s'))
            return s'

main :: IO ()
-- main = return ()
main = do
    (osc, sampleInput) <- mkOSCServer "127.0.0.1" 7800
    withDefaultSynth $ do
    -- withDefaultInternal $ do
        -- immediately ~> dumpOSC TextPrinter
        -- sync immediately $ send $ C.dumpOSC C.TextPrinter
        -- b <- accumulate (\t _ -> playDefault t) () osc
        let gen = do
            i_level <- oscFloatB "/midi/cc3/1" 0 osc
            i_mute  <- bang (threshold 0.5) 0 =<< oscFloatB "/midi/cc4/1" 0 osc
            i_event <- liftM (fmap . fmap $ const playDefault) (return $ address "/midi/cc5/1" osc)
            let i = StripInput i_level i_mute i_event
            strip 2 i
        let gen2 = oscFloatB "/midi/cc3/1" 0 osc
        -- reactimate =<< R.map (liftIO.print) =<< strip 2 i
        sampleOutput <- liftIO $ start gen2
        liftIO $ putStrLn "Starting network"
        -- liftIO $ reactimate $ fmap print $ changes $ level i
        forever $ sampleInput >> sampleOutput >>= liftIO . print
