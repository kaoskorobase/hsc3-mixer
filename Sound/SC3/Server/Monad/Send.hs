{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sound.SC3.Server.Monad.Send
  ( SendT
  , Async
  , Deferred
  , module Control.Applicative
  , liftServer
  , sendMsg
  , mkAsync
  , mkAsync_
  , mkAsyncCM
  , after
  , finally
  , whenDone
  , async
  , exec
  , (!>)
  , execPure
  , (~>)
  ) where

import           Control.Applicative
import           Control.Arrow (second)
import           Control.Monad (liftM, unless, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Trans.Class as Trans
import           Control.Monad.Trans.State.Strict (StateT(..))
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Seq
import           Data.Sequence (Seq, (|>), (><))
import qualified Data.Sequence as Seq
import           Sound.SC3.Server.Monad (ServerT)
import qualified Sound.SC3.Server.Monad as M
import           Sound.SC3.Server.State (SyncId)
import qualified Sound.SC3.Server.State as State
import qualified Sound.SC3.Server.Command as C
import           Sound.SC3.Server.Notification (Notification)
import qualified Sound.SC3.Server.Notification as N
import           Sound.OpenSoundControl (OSC(..), Time, immediately)

data SyncState = NoSync | NeedsSync | HasSync deriving (Eq, Show)

data State m = State {
    buildOSC      :: Seq OSC
  , notifications :: [Notification ()]
  , cleanup       :: ServerT m ()
  , syncState     :: SyncState
  }

-- | Representation of a server-side action (or sequence of actions).
newtype SendT m a = SendT (StateT (State m) (ServerT m) a)
                    deriving (Functor, Monad)

-- | Execute a SendT action, returning the result and the final state.
runSendT :: Monad m => SyncState -> SendT m a -> ServerT m (a, State m)
runSendT s (SendT m) = State.runStateT m (State Seq.empty [] (return ()) s)

-- | 
gets :: Monad m => (State m -> a) -> SendT m a
gets = SendT . State.gets

-- | Modify the state in a SendT action.
modify :: Monad m => (State m -> State m) -> SendT m ()
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

-- | Representation of a deferred server resource.
--
-- Deferred resources can only be inspected from with a 'whenDone' action or after the 'exec' has been called to execute the 'SendT' action.
--
-- Deferred is has 'Applicative' and 'Functor' instances, so that complex values can be built from simple ones.
newtype Deferred a = Deferred a deriving (Show)

instance Functor Deferred where
    fmap f (Deferred a) = Deferred (f a)

instance Applicative Deferred where
    pure = Deferred
    (<*>) (Deferred f) (Deferred a) = Deferred (f a)

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

-- | Add a synchronization barrier.
maybeSync :: MonadIO m => SendT m ()
maybeSync = do
    s <- gets syncState
    case s of
        NeedsSync -> do
            sid <- liftServer $ M.alloc State.syncIdAllocator
            sendMsg (C.sync (fromIntegral sid))
            after (N.synced sid) (M.free State.syncIdAllocator sid)
        _ -> return ()

-- | Register a cleanup action, to be executed after a notification has been received.
after :: Monad m => Notification a -> ServerT m () -> SendT m ()
after n m = modify $ \s -> s { notifications = fmap (const ()) n : notifications s
                             , cleanup = cleanup s >> m }

-- | Register a cleanup action, to be executed after all asynchronous commands and notification have finished.
finally :: Monad m => ServerT m () -> SendT m ()
finally m = modify $ \s -> s { cleanup = cleanup s >> m }

-- | Execute an server-side action after the asynchronous command has finished.
--
-- The corresponding server commands are scheduled at a time @t@ in the future.
whenDone :: MonadIO m => Async m a -> Time -> (a -> SendT m (Deferred b)) -> SendT m (Deferred b)
whenDone (Async m) t f = do
    (a, g) <- liftServer m
    (b, s) <- liftServer $ runSendT NeedsSync $ do { b <- f a ; maybeSync ; return b }
    sendMsg $ g (Just (Bundle t (Seq.toList (buildOSC s))))
    modify $ \s' -> s' {
        notifications = notifications s' ++ notifications s
      , cleanup = cleanup s' >> cleanup s
      , syncState = HasSync }
    return b

-- | Execute an asynchronous command asynchronously.
async :: MonadIO m => Async m a -> SendT m (Deferred a)
async (Async m) = do
    (a, g) <- liftServer m
    sendMsg (g Nothing)
    modify $ \s -> case syncState s of
                    HasSync -> s
                    _       -> s { syncState = NeedsSync }
    return $ pure a

-- | Run the 'SendT' action and return the result.
--
-- All asynchronous commands and their nested continuations are guaranteed to
-- have completed when this function returns.
exec :: MonadIO m => Time -> SendT m (Deferred a) -> ServerT m a
exec t m = do
    (Deferred a, s) <- runSendT NoSync $ do { a <- m ; maybeSync ; return a }
    unless (Seq.null (buildOSC s)) $ do
        -- liftIO $ print (Bundle t (Seq.toList (buildOSC s)))
        -- liftIO $ print (Seq.toList sids)
        M.waitForAll_ (Bundle t (Seq.toList (buildOSC s)))
                      (notifications s)
    cleanup s
    return a

-- | Infix operator version of 'exec'.
(!>) :: MonadIO m => Time -> SendT m (Deferred a) -> ServerT m a
(!>) = exec

-- | Run a 'SendT' action that returns a pure result.
--
-- All asynchronous commands and their nested continuations are guaranteed to
-- have completed when this function returns.
execPure :: MonadIO m => Time -> SendT m a -> ServerT m a
execPure t m = exec t (m >>= return . pure)

-- | Infix operator version of 'execPure'.
(~>) :: MonadIO m => Time -> SendT m a -> ServerT m a
(~>) = execPure
