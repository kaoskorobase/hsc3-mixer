import Control.Monad (forever)
import Sound.SC3.Server.Process.Monad
import Reactive hiding (accumulate)
import Sound.SC3.Server.Reactive
import Sound.SC3.Server.Monad.Command
import qualified Sound.SC3.Server.Monad as M
import qualified Sound.SC3.Server.Notification as N
import qualified Sound.SC3.Server.Command as C
import Sound.OpenSoundControl (Time(..), immediately, pauseThreadUntil, utcr)
import Control.Concurrent
import Sound.SC3.Mixer

playDefault t = do
    let dur = 1
        lat = 0.03
    r <- rootNode
    synth <- exec (UTCr (t+lat)) $ s_new d_default AddToTail r []
    fork $ do
        let t' = t + dur
        liftIO $ pauseThreadUntil t'
        exec (UTCr (t' + lat)) $ s_release 0 synth
    return ()

dt = 0.0125

inputLoop src t = do
    fire src t
    let t' = t + dt
    pauseThreadUntil t'
    inputLoop src t'

mainR = do
    src <- newEventSource
    forkIO . inputLoop src =<< utcr
    withDefaultInternal $ do
    -- withDefaultSynth $ do
        -- sync immediately $ send $ C.dumpOSC C.TextPrinter
        b <- accumulate (\t _ -> playDefault t) () (fromEventSource src)
        liftIO $ reactimate $ fmap return $ changes b
        liftIO $ threadDelay (truncate (300e6))

ioLoop t = do
    playDefault t
    let t' = t + dt
    liftIO $ pauseThreadUntil t'
    ioLoop t'

mainIO = do
    withDefaultInternal $ do
    -- withDefaultSynth $ do
        dumpOSC TextPrinter
        -- mkStrip >>= liftIO . print
        -- send immediately sync
        t <- liftIO utcr
        let t' = UTCr (t+5)
        (g, ig, b) <- exec immediately $ do
            b_alloc 1024 1 `whenDone` immediately $ \b -> do
                x <- b_free b `whenDone` t' $ \() -> do
                    g <- g_new_ AddToTail
                    ig <- g_new AddToTail g
                    sync
                    return (g, ig, b)
                return x
        n_query g >>= liftIO . print
        b_query b >>= liftIO . print
        status >>= liftIO . print
        waitFor (C.g_queryTree [(0, True)]) (N.hasAddress "/g_queryTree.reply") >>= liftIO . print
        -- ioLoop =<< liftIO utcr

main :: IO ()
main = mainIO
