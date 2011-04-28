{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This module provides abstractions for constructing bundles for server
-- resource allocation in a type safe manner. In particular, the exposed types
-- and functions make sure that asynchronous command results cannot be used
-- before they have been allocated on the server.
--
-- TODO: Real usage example
--
-- > (b0, (g, ig, b)) <- immediately !> do
-- > b0 <- async $ b_alloc 1024 1
-- > x <- b_alloc 1024 1 `whenDone` immediately $ \b -> do
-- >     b_free b `whenDone` OSC.UTCr t' $ \() -> do
-- >         g <- g_new_ AddToTail
-- >         ig <- g_new AddToTail g
-- >         return $ pure (g, ig, b)
-- > return $ (,) <$> b0 <*> x
module Sound.SC3.Server.Monad.Send
  ( SendT
  , Async
  , Deferred
  , module Control.Applicative
  , mkAsync
  , mkAsync_
  , mkAsyncCM
  , Cleanup
  , after
  , after_
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
import           Control.Monad (liftM)
import           Control.Monad.IO.Class (MonadIO(..))
import qualified Control.Monad.Trans.Class as Trans
import           Control.Monad.Trans.State.Strict (StateT(..))
import qualified Control.Monad.Trans.State.Strict as State
import           Data.IORef
import           Sound.SC3.Server.Monad (MonadIdAllocator, ServerT)
import qualified Sound.SC3.Server.Monad as M
import qualified Sound.SC3.Server.State as State
import qualified Sound.SC3.Server.Command as C
import           Sound.SC3.Server.Notification (Notification)
import qualified Sound.SC3.Server.Notification as N
import           Sound.OpenSoundControl (OSC(..), Time)

-- | Synchronisation state.
data SyncState =
    NoSync      -- ^ No synchronisation barrier needed.
  | NeedsSync   -- ^ Need to add a synchronisation barrier to the current context.
  | HasSync     -- ^ Synchronisation barrier already present in the current context.
  deriving (Eq, Ord, Show)

-- | Internal state used for constructing bundles from 'SendT' actions.
data State m = State {
    buildOSC      :: [OSC]                  -- ^ Currrent list of OSC messages.
  , notifications :: [Notification (IO ())] -- ^ Current list of notifications to synchronise on.
  , cleanup       :: ServerT m ()           -- ^ Cleanup action to deallocate resources.
  , syncState     :: SyncState              -- ^ Synchronisation barrier state.
  }

-- | Construct a 'SendT' state with a given synchronisation state.
mkState :: Monad m => SyncState -> State m
mkState = State [] [] (return ())

-- | Push an OSC packet.
pushOSC :: OSC -> State m -> State m
pushOSC osc s = s { buildOSC = osc : buildOSC s }

-- | Get the list of OSC packets.
getOSC :: State m -> [OSC]
getOSC = reverse . buildOSC

-- | Update the synchronisation state.
setSyncState :: SyncState -> State m -> State m
setSyncState ss s | ss > syncState s = s { syncState = ss }
                  | otherwise        = s

-- | Representation of a server-side action (or sequence of actions).
newtype SendT m a = SendT (StateT (State m) (ServerT m) a)
                    deriving (Applicative, Functor, Monad)

instance MonadIO m => MonadIdAllocator (SendT m) where
    rootNodeId = liftServer M.rootNodeId
    alloc = liftServer . M.alloc
    free a = liftServer . M.free a
    allocMany a = liftServer . M.allocMany a
    freeMany a = liftServer . M.freeMany a
    allocRange a = liftServer . M.allocRange a
    freeRange a = liftServer . M.freeRange a

-- | Bundles are flattened into the resulting bundle because @scsynth@ doesn't
-- support nested bundles.
instance Monad m => MonadSendOSC (SendT m) where
    send osc@(Message _ _) = modify (pushOSC osc)
    send (Bundle _ xs)     = mapM_ send xs

-- | Execute a SendT action, returning the result and the final state.
runSendT :: Monad m => SyncState -> SendT m a -> ServerT m (a, State m)
runSendT s (SendT m) = State.runStateT m (mkState s)

-- | Get a value from the state.
gets :: Monad m => (State m -> a) -> SendT m a
gets = SendT . State.gets

-- | Modify the state in a SendT action.
modify :: Monad m => (State m -> State m) -> SendT m ()
modify = SendT . State.modify

-- | Lift a ServerT action into SendT.
--
-- This is potentially unsafe and should only be used for the allocation of
-- server resources. Lifting actions that rely on communication and
-- synchronisation primitives will not work as expected.
liftServer :: Monad m => ServerT m a -> SendT m a
liftServer = SendT . Trans.lift

-- | Representation of an asynchronous server command.
--
-- Asynchronous commands are executed asynchronously with respect to other
-- server commands. There are two different ways of synchronising with an
-- asynchronous command: using 'whenDone' for server-side synchronisation or
-- observing the result of a 'SendT' action after calling 'exec'.
newtype Async m a = Async (ServerT m (a, (Maybe OSC -> OSC)))

-- | Representation of a deferred server resource.
--
-- Deferred resource values can only be observed a return value of the 'SendT'
-- action after 'exec' has been called.
--
-- Deferred is has 'Applicative' and 'Functor' instances, so that complex
-- values can be built from simple ones.
newtype Deferred a = Deferred (IO a)

-- | Construct a deferred value from an IO action.
deferredIO :: IO a -> Deferred a
deferredIO = Deferred

instance Functor Deferred where
    fmap f (Deferred a) = Deferred (f `fmap` a)

instance Applicative Deferred where
    pure = Deferred . pure
    (<*>) (Deferred f) (Deferred a) = Deferred (f <*> a)

-- | Create an asynchronous command.
--
-- The first return value should be a server resource allocated on the client,
-- the second a function that, given a completion packet, returns an OSC packet
-- that asynchronously allocates the resource on the server.
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

-- | Add a synchronisation barrier.
maybeSync :: MonadIO m => SendT m ()
maybeSync = do
    s <- gets syncState
    case s of
        NeedsSync -> do
            sid <- liftServer $ M.alloc State.syncIdAllocator
            send (C.sync (fromIntegral sid))
            after_ (N.synced sid) (M.free State.syncIdAllocator sid)
        _ -> return ()

-- | Cleanup action newtype wrapper.
newtype Cleanup m a = Cleanup (ServerT m a)
                      deriving (Applicative, MonadIdAllocator, Functor, Monad)

-- | Register a cleanup action, to be executed after a notification has been
-- received and return the deferred notification result.
after :: MonadIO m => Notification a -> Cleanup m () -> SendT m (Deferred a)
after n (Cleanup m) = do
    v <- liftServer $ liftIO $ newIORef (error "BUG: after: uninitialized IORef")
    modify $ \s -> s { notifications = fmap (writeIORef v) n : notifications s
                     , cleanup = cleanup s >> m }
    return $ deferredIO (readIORef v)

-- | Register a cleanup action, to be executed after a notification has been
-- received and ignore the notification result.
after_ :: Monad m => Notification a -> Cleanup m () -> SendT m ()
after_ n (Cleanup m) = modify $ \s -> s { notifications = fmap (const (return ())) n : notifications s
                                        , cleanup = cleanup s >> m }

-- | Register a cleanup action, to be executed after all asynchronous commands
-- and notification have finished.
finally :: Monad m => Cleanup m () -> SendT m ()
finally (Cleanup m) = modify $ \s -> s { cleanup = cleanup s >> m }

-- | Execute an server-side action after the asynchronous command has finished.
--
-- The corresponding server commands are scheduled at a time @t@ in the future.
whenDone :: MonadIO m => Async m a -> Time -> (a -> SendT m (Deferred b)) -> SendT m (Deferred b)
whenDone (Async m) t f = do
    (a, g) <- liftServer m
    (b, s) <- liftServer $ runSendT NeedsSync $ do { b <- f a ; maybeSync ; return b }
    send $ g (Just (Bundle t (getOSC s)))
    modify $ \s' -> s' {
        notifications = notifications s' ++ notifications s
      , cleanup = cleanup s' >> cleanup s
      , syncState = HasSync }
    return b

-- | Execute an asynchronous command asynchronously.
async :: MonadIO m => Async m a -> SendT m (Deferred a)
async (Async m) = do
    (a, g) <- liftServer m
    send (g Nothing)
    modify $ setSyncState NeedsSync
    return $ pure a

-- | Run the 'SendT' action and return the result.
--
-- All asynchronous commands and notifications are guaranteed to have finished
-- when this function returns.
exec :: MonadIO m => Time -> SendT m (Deferred a) -> ServerT m a
exec t m = do
    (Deferred a, s) <- runSendT NoSync $ do { a <- m ; maybeSync ; return a }
    case getOSC s of
        [] -> return ()
        osc -> M.waitForAll (Bundle t osc) (notifications s) >>= liftIO . sequence_
    cleanup s
    liftIO a

-- | Infix operator version of 'exec'.
(!>) :: MonadIO m => Time -> SendT m (Deferred a) -> ServerT m a
(!>) = exec

-- | Run a 'SendT' action that returns a pure result.
--
-- All asynchronous commands and notifications are guaranteed to have finished
-- when this function returns.
execPure :: MonadIO m => Time -> SendT m a -> ServerT m a
execPure t m = exec t (m >>= return . pure)

-- | Infix operator version of 'execPure'.
(~>) :: MonadIO m => Time -> SendT m a -> ServerT m a
(~>) = execPure
