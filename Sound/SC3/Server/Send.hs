{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sound.SC3.Server.Send
  (
    SendT
  , Async
  , mkAsync
  , mkAsync_
  , unsafeServer
  , sendOSC
  , whenDone
  , async
  , sync
  , sync'
  , exec
  ) where

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
-- This is potentially unsafe and should only be used for the implementation of server resources. Lifting actions that rely on synchronization will not work as expected.
unsafeServer :: Monad m => ServerT m a -> SendT m a
unsafeServer = SendT . Trans.lift

-- | Send an OSC packet.
--
-- Bundles are flattened into the resulting bundle, because the SuperCollider server doesn't handle nested bundles.
sendOSC :: Monad m => OSC -> SendT m ()
sendOSC osc@(Message _ _) = modify $ \s -> s { buildOSC = buildOSC s |> osc }
sendOSC osc@(Bundle _ _)  = modify $ \s -> s { buildOSC = buildOSC s >< Seq.fromList (flatten osc) }
    where
        flatten msg@(Message _ _) = [msg]
        flatten (Bundle _ xs) = concatMap flatten xs

-- | Representation of an asynchronous server command.
--
-- Asynchronous commands are executed asynchronously with respect to other server commands. There are two different ways of synchronizing with an asynchronous command: using 'whenDone' for server-side synchronization or 'sync' for client-side synchronization.
data Async m a = Async (SendT m (a, (Maybe OSC -> OSC)))

-- | Create an asynchronous command.
--
-- The completion message will be appended at the end of the message.
mkAsync :: Monad m => SendT m (a, OSC) -> Async m a
mkAsync m = Async $ do
    (a, osc) <- m
    return (a, f osc)
    where
        f msg Nothing   = msg
        f msg (Just cm) = C.withCM msg cm

-- | Create an asynchronous command from a pure OSC message.
mkAsync_ :: Monad m => OSC -> Async m ()
mkAsync_ osc = mkAsync $ return ((), osc)

-- | Execute an server-side action after the asynchronous command has finished.
--
-- The corresponding server commands are scheduled at a time @t@ in the future.
whenDone :: Monad m => Async m a -> Time -> (a -> SendT m b) -> Async m b
whenDone (Async m) t f = Async $ do
    (a, g) <- m
    (b, osc, sids) <- unsafeServer $ runSendT t (f a)
    modify $ \s -> s { syncIds = syncIds s >< sids }
    let g' p = case p of
                Nothing -> g osc
                Just msg@(Message _ _) ->
                    case osc of
                        Nothing -> g (Just msg)
                        Just (Bundle t xs) -> g (Just (Bundle t (xs ++ [msg])))
                        Just (Message _ _) -> error "whenDone: cannot append to message"
                Just (Bundle _ _) -> error "whenDone: cannot append bundle"
    return (b, g')

-- | Execute an asynchronous command asynchronously.
async :: Monad m => Async m a -> SendT m a
async (Async m) = do
    (a, f) <- m
    sendOSC (f Nothing)
    return a

-- | Synchronize with the completion of an asynchronous command.
sync :: MonadIO m => Async m a -> SendT m a
sync (Async m) = do
    (a, f) <- m
    sid <- unsafeServer $ M.alloc State.syncIdAllocator
    modify $ \s -> s { syncIds = syncIds s |> sid }
    sendOSC (f Nothing)
    sendOSC (C.sync (fromIntegral sid))
    return a

-- | Synchronize with the completion of an asynchronous command's completion bundle.
sync' :: MonadIO m => Async m a -> SendT m a
sync' (Async m) = do
    (a, f) <- m
    sid <- unsafeServer $ M.alloc State.syncIdAllocator
    modify $ \s -> s { syncIds = syncIds s |> sid }
    sendOSC (f (Just (C.sync (fromIntegral sid))))
    return a

-- | Run the SendT action and return the result.
exec :: MonadIO m => Time -> SendT m a -> ServerT m a
exec t m = do
    (a, osc, sids) <- runSendT t m
    -- liftIO $ print osc
    -- liftIO $ print (Seq.toList sids)
    maybe (return []) (flip M.syncWithAll (map N.synced (Seq.toList sids))) osc
    Seq.mapM_ (M.free State.syncIdAllocator) sids
    return a
