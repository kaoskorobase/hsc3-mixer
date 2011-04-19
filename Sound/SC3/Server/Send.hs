{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sound.SC3.Server.Send
  (
    SendT
  , Async
  , mkAsync
  , lift
  , sendOSC
  , whenDone
  , async
  , sync
  , send
  ) where

import           Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.Trans.Class as Trans
import           Control.Monad.Trans.State (StateT(..))
import qualified Control.Monad.Trans.State as State
import qualified Data.Foldable as Seq
import           Data.Sequence (Seq, (|>), (><))
import qualified Data.Sequence as Seq
import           Sound.SC3.Server.Monad (ServerT)
import qualified Sound.SC3.Server.Monad as M
import           Sound.SC3.Server.State (SyncId)
import qualified Sound.SC3.Server.State as State
import qualified Sound.SC3.Server.Command as C
import qualified Sound.SC3.Server.Notification as N
import           Sound.OpenSoundControl (OSC(..), Time)

data State = State {
    buildOSC :: Seq OSC
  , syncs    :: Seq SyncId
  } deriving (Eq, Show)

newtype SendT m a = SendT (StateT State (ServerT m) a)
                    deriving (Functor, Monad)

runSendT :: Monad m => Time -> SendT m a -> ServerT m (a, Maybe OSC, Seq SyncId)
runSendT t (SendT m) = do
    (a, s) <- runStateT m (State Seq.empty Seq.empty)
    let osc = if Seq.null (buildOSC s)
                then Nothing
                else Just (Bundle t (Seq.toList (buildOSC s)))
    return (a, osc, syncs s)

lift :: Monad m => ServerT m a -> SendT m a
lift = SendT . Trans.lift

sendOSC :: Monad m => OSC -> SendT m ()
sendOSC osc = SendT $ State.modify $ \s -> s { buildOSC = buildOSC s |> osc }

data Async a = Async a (Maybe OSC -> OSC)

-- | Create an asynchronous command.
--
-- The completion message will be appended at the end of the message.
mkAsync :: Monad m => a -> OSC -> SendT m (Async a)
mkAsync a msg = return (Async a f)
    where
        f Nothing   = msg
        f (Just cm) = C.withCM msg cm

-- | Execute an asynchronous command
async :: Monad m => SendT m (Async a) -> Time -> SendT m a
async a t = whenDone a t return

-- | Execute a SendT action after the asynchronous command has finished.
whenDone :: Monad m => SendT m (Async a) -> Time -> (a -> SendT m b) -> SendT m b
whenDone m t f = do
    (Async a toOSC) <- m
    (b, osc, sids) <- lift $ runSendT t (f a)
    sendOSC (toOSC osc)
    SendT $ State.modify $ \s -> s { syncs = syncs s >< sids }
    return b

-- | Add a synchronization barrier.
sync :: MonadIO m => SendT m ()
sync = do
    sid <- lift $ M.alloc State.syncIdAllocator
    SendT $ State.modify $ \s -> s { syncs = syncs s |> sid }
    sendOSC (C.sync (fromIntegral sid))

-- | Run the SendT action and return the result.
send :: MonadIO m => Time -> SendT m a -> ServerT m a
send t m = do
    (a, osc, sids) <- runSendT t m
    maybe (return []) (flip M.syncWithAll (map N.synced (Seq.toList sids))) osc
    Seq.mapM_ (M.free State.syncIdAllocator) sids
    return a
