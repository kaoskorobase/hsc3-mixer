{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sound.SC3.Server.Send
  ( SendT
  , Async
  , liftServer
  , sendMsg
  , mkAsync
  , mkAsync_
  , mkAsyncCM
  , whenDone
  , async
  , sync
  , exec
  ) where

import           Control.Arrow (second)
import           Control.Monad (liftM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
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
  , syncIds  :: Seq SyncId
  } deriving (Eq, Show)

-- | Representation of a server-side action (or sequence of actions).
newtype SendT m a = SendT (StateT State (ServerT m) a)
                    deriving (Functor, Monad)

-- | Execute a SendT action, returning the result, an OSC bundle and a list of sync ids.
runSendT :: Monad m => Time -> SendT m a -> ServerT m (a, Maybe OSC, Seq SyncId)
runSendT t (SendT m) = do
    (a, s) <- runStateT m (State Seq.empty Seq.empty)
    let osc = if Seq.null (buildOSC s)
                then Nothing
                else Just (Bundle t (Seq.toList (buildOSC s)))
    return (a, osc, syncIds s)

-- | Modify the state in a SendT action.
modify :: Monad m => (State -> State) -> SendT m ()
modify = SendT . State.modify

-- | Lift a ServerT action into SendT.
--
-- This is potentially unsafe and should only be used for the allocation of server resources. Lifting actions that rely on communication and synchronization primitives will not work as expected.
liftServer :: Monad m => ServerT m a -> SendT m a
liftServer = SendT . Trans.lift

-- | Send an OSC message.
--
-- An error is signaled when attempting to send a bundle (@scsynth@ doesn't support nested bundles).
sendMsg :: Monad m => OSC -> SendT m ()
sendMsg osc =
    case osc of
        Message _ _ -> modify $ \s -> s { buildOSC = buildOSC s |> osc }
        _ -> error "sendMsg: Cannot nest bundles"

-- | Representation of an asynchronous server command.
--
-- Asynchronous commands are executed asynchronously with respect to other server commands. There are two different ways of synchronizing with an asynchronous command: using 'whenDone' for server-side synchronization or 'sync' for client-side synchronization.
data Async m a = Async (ServerT m (a, (Maybe OSC -> OSC)))

-- | Create an asynchronous command.
--
-- The first return value should be a server resource allocated on the client, the second a function that, given a completion packet, returns an OSC packet that asynchronously allocates the resource on the server.
mkAsync :: ServerT m (a, (Maybe OSC -> OSC)) -> Async m a
mkAsync = Async

-- | Create an asynchronous command from a side effecting OSC function.
mkAsync_ :: Monad m => (Maybe OSC -> OSC) -> Async m ()
mkAsync_ f = mkAsync $ return ((), f)

-- | Create an asynchronous command.
--
-- The completion message will be appended at the end of the returned message.
mkAsyncCM :: Monad m => ServerT m (a, OSC) -> Async m a
mkAsyncCM = mkAsync . liftM (second f)
    where
        f msg Nothing   = msg
        f msg (Just cm) = C.withCM msg cm

-- | Execute an asynchronous command asynchronously.
async :: Monad m => Async m a -> SendT m a
async (Async m) = do
    (a, f) <- liftServer m
    sendMsg (f Nothing)
    return a

-- | Execute an server-side action after the asynchronous command has finished.
--
-- The corresponding server commands are scheduled at a time @t@ in the future.
whenDone :: Monad m => Async m a -> Time -> (a -> SendT m b) -> SendT m b
whenDone (Async m) t f = do
    (a, appendCompletion) <- liftServer m
    (b, osc, sids) <- liftServer $ runSendT t (f a)
    sendMsg (appendCompletion osc)
    modify $ \s -> s { syncIds = syncIds s >< sids }
    return b

-- | Add a synchronization barrier.
sync :: MonadIO m => SendT m ()
sync = do
    sid <- liftServer $ M.alloc State.syncIdAllocator
    modify $ \s -> s { syncIds = syncIds s |> sid }
    sendMsg (C.sync (fromIntegral sid))

-- | Run the SendT action and return the result.
exec :: MonadIO m => Time -> SendT m a -> ServerT m a
exec t m = do
    (a, osc, sids) <- runSendT t m
    -- liftIO $ print osc
    -- liftIO $ print (Seq.toList sids)
    maybe (return ()) (flip M.waitForAll_ (map N.synced (Seq.toList sids))) osc
    Seq.mapM_ (M.free State.syncIdAllocator) sids
    return a
