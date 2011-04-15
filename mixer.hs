import Control.Monad (forever)
import Sound.SC3.Server.Process.Monad
import Reactive hiding (accumulate)
import Sound.SC3.Server.Reactive
import Sound.SC3.Server.Resource
import qualified Sound.SC3.Server.Command as C
import Sound.OpenSoundControl (immediately)
import Control.Concurrent
import Sound.SC3.Mixer

playDefault = do
    r <- rootNode
    synth <- async immediately $ s_new "default" AddToTail r []
    fork $ do
        liftIO $ threadDelay (truncate (1e6))
        async immediately $ s_release 0 synth
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
        sync immediately $ send $ C.dumpOSC C.TextPrinter
        -- playDefault
        mkStrip >>= liftIO . print
        liftIO $ threadDelay (truncate (dt * 1000e6))

main :: IO ()
main = mainR
