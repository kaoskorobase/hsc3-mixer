import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO)
import           Sound.SC3.Server.Process.Monad
import           Reactive hiding (accumulate)
import           Sound.SC3.Mixer
import           Sound.SC3.Server.Monad.Command
import           Sound.SC3.Server.Notification
import           Sound.SC3.Server.Reactive
import qualified Sound.SC3.Server.Command as C
import           Sound.OpenSoundControl (immediately)
import qualified Sound.OpenSoundControl as OSC
 
dt = 0.00125
dur = 0.2
gate = 0.2
lat = 0.03
 
playDefault :: MonadIO m => Double -> ServerT m ()
playDefault t = do
    r <- rootNode
    synth <- OSC.UTCr (t + lat) ~> s_new d_default AddToTail r []
    fork $ do
        let t' = t + dur
        liftIO $ OSC.pauseThreadUntil t'
        OSC.UTCr (t' + lat) ~> s_release (-1 - gate) synth
    return ()
 
inputLoop :: EventSource Double -> Double -> IO ()
inputLoop src t = do
    fire src t
    let t' = t + dt
    OSC.pauseThreadUntil t'
    inputLoop src t'
 
mainR = do
    src <- newEventSource
    forkIO . inputLoop src =<< OSC.utcr
    -- withDefaultInternal $ do
    withDefaultSynth $ do
        -- dumpOSC TextPrinter
        -- sync immediately $ send $ C.dumpOSC C.TextPrinter
        b <- accumulate (\t _ -> playDefault t) () (fromEventSource src)
        liftIO $ reactimate $ fmap return $ changes b
        liftIO $ threadDelay (truncate (300e6))
 
ioLoop t = do
    playDefault t
    let t' = t + dt
    liftIO $ OSC.pauseThreadUntil t'
    ioLoop t'
 
mainIO = do
    withDefaultInternal $ do
    -- withDefaultSynth $ do
        immediately ~> dumpOSC TextPrinter
        -- mkStrip >>= liftIO . print
        -- send immediately sync
        t <- liftIO $ OSC.utcr
        let t' = t + 5
        (b0, (g, ig, b)) <- OSC.UTCr t' !> do
            b0 <- async $ b_alloc 1024 1
            x <- asyncM $ b_alloc 2048 2 `whenDone` \b -> do
                async $ b_free b `whenDone` \_ -> do
                    g <- g_new_ AddToTail
                    ig <- g_new AddToTail g
                    return (g, ig, b)
            async $ b_alloc 4096 3 `whenDone` \_ -> do
                g <- rootNode
                s_new (d_named "default") AddToTail g []
            return $ (,) <$> b0 <*> x
        info <- immediately !> do
            ig <- n_query g
            ib <- b_query b
            ib0 <- b_query b0
            return $ pure (,,) <*> ig <*> ib <*> ib0
        liftIO $ print info
        immediately !> status >>= liftIO . print
        waitFor (C.g_queryTree [(0, True)]) (hasAddress "/g_queryTree.reply") >>= liftIO . print
        liftIO $ OSC.pauseThread 5
        -- ioLoop =<< liftIO utcr
 
main :: IO ()
main = mainIO
