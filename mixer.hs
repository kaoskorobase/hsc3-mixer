import Control.Monad (forever)
import Sound.SC3.Server.Process.Monad
import Reactive hiding (accumulate)
import Sound.SC3.Server.Reactive
import Sound.SC3.Server.Resource
import qualified Sound.SC3.Server.Command as C
import Sound.OpenSoundControl (Time(..), immediately, pauseThreadUntil, utcr)
import Control.Concurrent
import Sound.SC3.Mixer

playDefault t = do
    let dur = 1
        lat = 0.03
    r <- rootNode
    synth <- async (UTCr (t+lat)) $ s_new "default" AddToTail r []
    fork $ do
        let t' = t + dur
        liftIO $ pauseThreadUntil t'
        async (UTCr (t' + lat)) $ s_release 0 synth
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
        -- sync immediately $ send $ C.dumpOSC C.TextPrinter
        -- mkStrip >>= liftIO . print
        ioLoop =<< liftIO utcr

main :: IO ()
main = mainR
