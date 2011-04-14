{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sound.SC3.Server.Resource
  (
    SendT
  , defer
  , async'
  , sync'
  , Node(..)
  , AddAction(..)
  , Synth(..)
  , s_new
  , Group(..)
  , g_new
  , g_deepFree
  , g_freeAll
  , g_head
  , g_tail
  , Bus
  ) where

import Control.Monad (liftM, unless)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (WriterT(..), tell)
import           Data.DList (DList)
import qualified Data.DList as DList
import Sound.SC3.Server.Monad hiding (bufferId, busId, nodeId)
import           Sound.SC3.Server.Allocator.Range (Range)
import qualified Sound.SC3.Server.State as State
import           Sound.SC3.Server.Command (AddAction(..))
import qualified Sound.SC3.Server.Command as C
import Sound.OpenSoundControl (OSC(..), Time)

newtype SendT m a = SendT { unSendT :: WriterT (DList OSC) (ServerT m) a }
                    deriving (Functor, Monad)

liftServer :: Monad m => ServerT m a -> SendT m a
liftServer = SendT . lift

send :: Monad m => OSC -> SendT m ()
send = SendT . tell . DList.singleton

defer :: Monad m => SendT m a -> ServerT m (a, [OSC])
defer (SendT w) = do
    (a, l) <- runWriterT w
    return (a, DList.toList l)

async' :: MonadIO m => Time -> SendT m a -> ServerT m a
async' t s = do
    (a, l) <- defer s
    unless (null l) $ async (Bundle t l)
    return a

sync' :: MonadIO m => Time -> SendT m a -> ServerT m a
sync' t s = do
    (a, l) <- defer s
    sync (Bundle t l)
    return a

class Node a where
    nodeId :: a -> NodeId

newtype Synth = Synth NodeId deriving (Eq, Ord, Show)

instance Node Synth where
    nodeId (Synth nid) = nid

s_new :: MonadIO m => String -> AddAction -> Group -> [(String, Double)] -> SendT m Synth
s_new n a g xs = do
    nid <- liftServer $ alloc State.nodeId
    send $ C.s_new n (fromIntegral nid) a (fromIntegral (nodeId g)) xs
    return $ Synth nid

newtype Group = Group NodeId deriving (Eq, Ord, Show)

instance Node Group where
    nodeId (Group nid) = nid

g_new :: MonadIO m => AddAction -> Group -> SendT m Group
g_new a p = do
    nid <- liftServer $ alloc State.nodeId
    send $ C.g_new [(fromIntegral nid, a, fromIntegral (nodeId p))]
    return $ Group nid

g_deepFree :: MonadIO m => Group -> SendT m ()
g_deepFree g = send $ C.g_deepFree [fromIntegral (nodeId g)]

g_freeAll :: MonadIO m => Group -> SendT m ()
g_freeAll g = send $ C.g_freeAll [fromIntegral (nodeId g)]

g_head :: (Node n, MonadIO m) => Group -> n -> SendT m ()
g_head g n = send $ C.g_head [(fromIntegral (nodeId g), fromIntegral (nodeId n))]

g_tail :: (Node n, MonadIO m) => Group -> n -> SendT m ()
g_tail g n = send $ C.g_tail [(fromIntegral (nodeId g), fromIntegral (nodeId n))]

newtype Buffer = Buffer { bufferId :: BufferId } deriving (Eq, Ord, Show)

newtype Bus = Bus { busId :: Range BusId } deriving (Eq, Show)
