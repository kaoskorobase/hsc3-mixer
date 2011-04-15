{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sound.SC3.Server.Resource
  ( Server
  , ServerT
  , BufferId
  , BusId
  , NodeId
  , fork
  , MonadIO
  , liftIO
  , SendT
  , send
  , defer
  , async
  , sync
  , syncWith
  , Resource(..)
  , Node(..)
  , n_free
  , AddAction(..)
  , Synth(..)
  , s_new
  , s_new_
  , s_release
  , Group(..)
  , rootNode
  , g_new
  , g_new_
  , g_deepFree
  , g_freeAll
  , g_head
  , g_tail
  , Bus
  , busId
  , numChannels
  , AudioBus(..)
  , newAudioBus
  , ControlBus(..)
  , newControlBus
  ) where

import Control.Applicative
import Control.Monad (liftM, unless, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (WriterT(..), tell)
import           Data.DList (DList)
import qualified Data.DList as DList
import           Sound.SC3 (Rate(..))
import           Sound.SC3.Server.Monad (Server, ServerT, BufferId, BusId, NodeId, fork)
import qualified Sound.SC3.Server.Monad as M
import           Sound.SC3.Server.Allocator.Range (Range)
import qualified Sound.SC3.Server.Allocator.Range as Range
import qualified Sound.SC3.Server.State as State
import           Sound.SC3.Server.Command (AddAction(..))
import qualified Sound.SC3.Server.Command as C
import qualified Sound.SC3.Server.Notification as N
import Sound.OpenSoundControl (OSC(..), Time, immediately)

-- ====================================================================
-- SendT

newtype SendT m a = SendT { unSendT :: WriterT (DList (Either OSC (ServerT m ()))) (ServerT m) a }
                    deriving (Functor, Monad)

liftServer :: Monad m => ServerT m a -> SendT m a
liftServer = SendT . lift

send :: Monad m => OSC -> SendT m ()
send = SendT . tell . DList.singleton . Left

sendList :: Monad m => [OSC] -> SendT m ()
sendList = mapM_ send

cleanup :: Monad m => ServerT m () -> SendT m ()
cleanup = SendT . tell . DList.singleton . Right

defer :: Monad m => Time -> SendT m a -> ServerT m (a, (OSC, ServerT m ()))
defer t (SendT w) = do
    (a, dl) <- runWriterT w
    let l = DList.toList dl
    return (a, (Bundle t [ x | Left x <- l ], sequence_ [ x | Right x <- l ]))

async :: MonadIO m => Time -> SendT m a -> ServerT m a
async t s = do
    (a, (osc, action)) <- defer t s
    M.async osc
    action
    return a

sync :: MonadIO m => Time -> SendT m a -> ServerT m a
sync t s = do
    (a, (osc, action)) <- defer t s
    M.sync osc
    action
    return a

syncWith :: MonadIO m => Time -> SendT m a -> N.Notification b -> ServerT m (a, b)
syncWith t s n = do
    (a, (osc, action)) <- defer t s
    b <- M.syncWith osc n
    action
    return (a, b)

-- ====================================================================
-- Resource

class Resource a where
    free :: MonadIO m => a -> ServerT m ()

-- ====================================================================
-- Node

class Node a where
    nodeId :: a -> NodeId

n_free :: (Node a, MonadIO m) => a -> SendT m ()
n_free n = do
    send $ C.n_free [fromIntegral (nodeId n)]
    cleanup $ M.free M.nodeIdAllocator (nodeId n)

-- ====================================================================
-- Synth

newtype Synth = Synth NodeId deriving (Eq, Ord, Show)

instance Node Synth where
    nodeId (Synth nid) = nid

instance Resource Synth where
    free = async immediately . n_free

s_new :: MonadIO m => String -> AddAction -> Group -> [(String, Double)] -> SendT m Synth
s_new n a g xs = do
    nid <- liftServer $ M.alloc M.nodeIdAllocator
    send $ C.s_new n (fromIntegral nid) a (fromIntegral (nodeId g)) xs
    return $ Synth nid

s_new_ :: MonadIO m => String -> AddAction -> [(String, Double)] -> SendT m Synth
s_new_ n a xs = liftServer rootNode >>= \g -> s_new n a g xs

s_release :: (Node a, MonadIO m) => Double -> a -> SendT m ()
s_release r n = do
    sendList [ C.n_set1 nid "gate" r
             , C.s_noid [nid] ]
    cleanup $ M.free M.nodeIdAllocator (nodeId n)
    where nid = fromIntegral (nodeId n)

-- ====================================================================
-- Group

newtype Group = Group NodeId deriving (Eq, Ord, Show)

instance Node Group where
    nodeId (Group nid) = nid

instance Resource Group where
    free = async immediately . n_free

rootNode :: MonadIO m => ServerT m Group
rootNode = liftM Group M.rootNodeId

g_new :: MonadIO m => AddAction -> Group -> SendT m Group
g_new a p = do
    nid <- liftServer $ M.alloc State.nodeIdAllocator
    send $ C.g_new [(fromIntegral nid, a, fromIntegral (nodeId p))]
    return $ Group nid

g_new_ :: MonadIO m => AddAction -> SendT m Group
g_new_ a = liftServer rootNode >>= g_new a

g_deepFree :: MonadIO m => Group -> SendT m ()
g_deepFree g = send $ C.g_deepFree [fromIntegral (nodeId g)]

g_freeAll :: MonadIO m => Group -> SendT m ()
g_freeAll g = send $ C.g_freeAll [fromIntegral (nodeId g)]

g_head :: (Node n, MonadIO m) => Group -> n -> SendT m ()
g_head g n = send $ C.g_head [(fromIntegral (nodeId g), fromIntegral (nodeId n))]

g_tail :: (Node n, MonadIO m) => Group -> n -> SendT m ()
g_tail g n = send $ C.g_tail [(fromIntegral (nodeId g), fromIntegral (nodeId n))]

-- ====================================================================
-- Buffer

newtype Buffer = Buffer { bufferId :: BufferId } deriving (Eq, Ord, Show)

instance Resource Buffer where
    free = M.free M.bufferIdAllocator . bufferId

-- ====================================================================
-- Bus

class Bus a where
    rate :: a -> Rate
    busIdRange :: a -> Range BusId

busId :: Bus a => a -> BusId
busId = Range.begin . busIdRange

numChannels :: Bus a => a -> Int
numChannels = Range.size . busIdRange

newtype AudioBus = AudioBus { audioBusId :: Range BusId } deriving (Eq, Show)

newAudioBus :: MonadIO m => Int -> ServerT m AudioBus
newAudioBus = liftM AudioBus . M.allocRange M.audioBusIdAllocator

instance Bus AudioBus where
    rate _ = AR
    busIdRange = audioBusId

instance Resource AudioBus where
    free = M.freeRange M.audioBusIdAllocator . audioBusId

newtype ControlBus = ControlBus { controlBusId :: Range BusId } deriving (Eq, Show)

newControlBus :: MonadIO m => Int -> ServerT m ControlBus
newControlBus = liftM ControlBus . M.allocRange M.controlBusIdAllocator

instance Bus ControlBus where
    rate _ = KR
    busIdRange = controlBusId

instance Resource ControlBus where
    free = M.freeRange M.controlBusIdAllocator . controlBusId
