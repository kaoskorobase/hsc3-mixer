import Control.Monad (forever)
import Sound.SC3.Server.Process.Monad
import Reactive hiding (accumulate)
import Sound.SC3.Server.Reactive
import Sound.SC3.Server.Monad
import Sound.SC3.Server.Command hiding (sync)
import Sound.OpenSoundControl
import Control.Concurrent
import Sound.SC3.Mixer

playDefault = do
    nid <- alloc nodeId
    async $ s_new "default" (fromIntegral nid) AddToTail 0 []
    fork $ do
        liftIO $ threadDelay (truncate (1e6))
        async $ n_set1 (fromIntegral nid) "gate" 0
        free nodeId nid
    return ()

dt = 0.0125

mainR = do
    src <- newEventSource
    forkIO $ forever $ do
        fire src ()
        threadDelay (truncate (dt * 1e6))
    withDefaultInternal $ do
        b <- accumulate (\_ _ -> playDefault) () (fromEventSource src)
        liftIO $ reactimate $ fmap return $ changes b
        liftIO $ threadDelay (truncate (300e6))

mainIO = do
    withDefaultInternal $ do
        sync $ dumpOSC TextPrinter
        -- playDefault
        mkStrip >>= liftIO . print
        liftIO $ threadDelay (truncate (dt * 1000e6))

main :: IO ()
main = mainIO
