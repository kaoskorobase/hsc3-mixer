import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Fix (MonadFix(..))
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Class (lift)
import           FRP.Elerea.Simple as FRP
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

-- mkOSCServer :: MonadIO m => String -> Int -> m (Signal m (Maybe OSC), m ())
mkOSCServer host port = do
    t <- liftIO $ OSC.udpServer host port
    (sig, snk) <- external Nothing
    return (sig, liftIO (OSC.recv t) >>= snk . Just)

-- oscFloatB :: (Functor m, MonadFix m, MonadIO m) => String -> Double -> Signal m (Maybe OSC) -> SignalGen m (Signal m Double)
oscFloatB cmd x0 = transfer x0 f
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
main :: IO ()
main = do
    -- withDefaultSynth $ do
        (osc, sampleInput) <- mkOSCServer "127.0.0.1" 7800
        -- let gen = oscFloatB "/midi/cc3/1" 0 osc
        let gen = delay Nothing osc
        sampleOutput <- start gen
        liftIO $ putStrLn "Starting network"
        forever $ sampleInput >> sampleOutput >>= liftIO . print
