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
  -- * Master controls
  , status
  , PrintLevel(..)
  , dumpOSC
  -- * Synth definitions
  -- , d_recv
  -- , d_load
  -- , d_loadDir
  , d_named
  , d_default
  , d_new
  , d_free
  -- * Resources
  , Resource(..)
  -- ** Nodes
  , Node(..)
  , n_free
  , n_query
  , AddAction(..)
  -- *** Synths
  , Synth(..)
  , s_new
  , s_new_
  , s_release
  -- *** Groups
  , Group(..)
  , rootNode
  , g_new
  , g_new_
  , g_deepFree
  , g_freeAll
  , g_head
  , g_tail
  , g_dumpTree
  -- ** Buffers
  , Buffer
  , b_alloc
  , b_read
  , HeaderFormat(..)
  , SampleFormat(..)
  , b_write
  , b_free
  , b_zero
  , b_query
  -- ** Buses
  , Bus
  , busId
  , numChannels
  , AudioBus(..)
  , newAudioBus
  , ControlBus(..)
  , newControlBus
  ) where

import qualified Codec.Digest.SHA as SHA
import           Control.Applicative
import           Control.Arrow (first)
import           Control.Monad (liftM, unless, void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Sound.SC3 (Rate(..), Synthdef, UGen)
import           Sound.SC3.Server.Monad (Server, ServerT, BufferId, BusId, NodeId, fork)
import qualified Sound.SC3.Server.Monad as M
import           Sound.SC3.Server.Allocator.Range (Range)
import qualified Sound.SC3.Server.Allocator.Range as Range
import qualified Sound.SC3.Server.State as State
import qualified Sound.SC3.Server.Synthdef as Synthdef
import           Sound.SC3.Server.Command (AddAction(..), PrintLevel(..))
import qualified Sound.SC3.Server.Command as C
import qualified Sound.SC3.Server.Command.Completion as C
import qualified Sound.SC3.Server.Notification as N
import           Sound.SC3.Server.Send
import           Sound.OpenSoundControl (OSC(..), Time, immediately)

-- ====================================================================
-- Master controls

status :: MonadIO m => ServerT m N.Status
status = M.waitFor C.status N.status_reply

dumpOSC :: MonadIO m => PrintLevel -> ServerT m ()
dumpOSC p = M.sync (C.dumpOSC p)

-- ====================================================================
-- Resource

class Resource a where
    free :: MonadIO m => a -> ServerT m ()

-- ====================================================================
-- Synth definitions

newtype SynthDef = SynthDef {
    name  :: String
  } deriving (Eq, Show)

d_named :: String -> SynthDef
d_named = SynthDef

d_default :: SynthDef
d_default = d_named "default"

graphName = SHA.showBSasHex . SHA.hash SHA.SHA512 . B.pack . show . Synthdef.synth

d_new :: Monad m => String -> UGen -> Async m SynthDef
d_new prefix ugen = mkAsync $ return (sd, C.d_recv (Synthdef.synthdef (name sd) ugen))
    where sd = SynthDef (prefix ++ "-" ++ graphName ugen)

-- | Remove definition once all nodes using it have ended.
d_free :: Monad m => SynthDef -> SendT m ()
d_free = sendOSC . C.d_free . (:[]) . name

-- -- | Install a bytecode instrument definition.
-- d_recv :: MonadIO m => Synthdef -> Async m ()
-- d_recv = mkAsync_ . C.d_recv
-- 
-- -- | Load an instrument definition from a named file.
-- d_load :: MonadIO m => FilePath -> Async m ()
-- d_load = mkAsync_ . C.d_load
-- 
-- -- | Load a directory of instrument definitions files.
-- d_loadDir :: MonadIO m => FilePath -> Async m ()
-- d_loadDir = mkAsync_ . C.d_loadDir

-- ====================================================================
-- Node

class Node a where
    nodeId :: a -> NodeId

n_free :: (Node a, MonadIO m) => a -> SendT m ()
n_free n = do
    sendOSC $ C.n_free [fromIntegral (nodeId n)]
    unsafeServer $ M.free M.nodeIdAllocator (nodeId n)

n_query :: (Node a, MonadIO m) => a -> ServerT m N.NodeNotification
n_query n = M.waitFor (C.n_query [(fromIntegral (nodeId n))]) (N.n_info (nodeId n))

-- ====================================================================
-- Synth

newtype Synth = Synth NodeId deriving (Eq, Ord, Show)

instance Node Synth where
    nodeId (Synth nid) = nid

instance Resource Synth where
    free = exec immediately . n_free

s_new :: MonadIO m => SynthDef -> AddAction -> Group -> [(String, Double)] -> SendT m Synth
s_new d a g xs = do
    nid <- unsafeServer $ M.alloc M.nodeIdAllocator
    sendOSC $ C.s_new (name d) (fromIntegral nid) a (fromIntegral (nodeId g)) xs
    return $ Synth nid

s_new_ :: MonadIO m => SynthDef -> AddAction -> [(String, Double)] -> SendT m Synth
s_new_ d a xs = unsafeServer rootNode >>= \g -> s_new d a g xs

s_release :: (Node a, MonadIO m) => Double -> a -> SendT m ()
s_release r n = do
    sendOSC (C.n_set1 (fromIntegral nid) "gate" r)
    unsafeServer $ M.free M.nodeIdAllocator nid
    where nid = nodeId n

-- ====================================================================
-- Group

newtype Group = Group NodeId deriving (Eq, Ord, Show)

instance Node Group where
    nodeId (Group nid) = nid

instance Resource Group where
    free = exec immediately . n_free

rootNode :: MonadIO m => ServerT m Group
rootNode = liftM Group M.rootNodeId

g_new :: MonadIO m => AddAction -> Group -> SendT m Group
g_new a p = do
    nid <- unsafeServer $ M.alloc State.nodeIdAllocator
    sendOSC $ C.g_new [(fromIntegral nid, a, fromIntegral (nodeId p))]
    return $ Group nid

g_new_ :: MonadIO m => AddAction -> SendT m Group
g_new_ a = unsafeServer rootNode >>= g_new a

g_deepFree :: Monad m => Group -> SendT m ()
g_deepFree g = sendOSC $ C.g_deepFree [fromIntegral (nodeId g)]

g_freeAll :: Monad m => Group -> SendT m ()
g_freeAll g = sendOSC $ C.g_freeAll [fromIntegral (nodeId g)]

g_head :: (Node n, Monad m) => Group -> n -> SendT m ()
g_head g n = sendOSC $ C.g_head [(fromIntegral (nodeId g), fromIntegral (nodeId n))]

g_tail :: (Node n, Monad m) => Group -> n -> SendT m ()
g_tail g n = sendOSC $ C.g_tail [(fromIntegral (nodeId g), fromIntegral (nodeId n))]

g_dumpTree :: Monad m => [(Group, Bool)] -> SendT m ()
g_dumpTree = sendOSC . C.g_dumpTree . map (first (fromIntegral . nodeId))

-- ====================================================================
-- Buffer

newtype Buffer = Buffer { bufferId :: BufferId } deriving (Eq, Ord, Show)

instance Resource Buffer where
    free = M.free M.bufferIdAllocator . bufferId

b_alloc :: MonadIO m => Int -> Int -> Async m Buffer
b_alloc n c = mkAsync $ do
    bid <- M.alloc State.bufferIdAllocator
    return (Buffer bid, C.b_alloc (fromIntegral bid) n c)

b_read :: MonadIO m => Buffer -> FilePath -> Maybe Int -> Maybe Int -> Maybe Int -> Bool -> Async m ()
b_read (Buffer bid) path fileOffset numFrames bufferOffset leaveOpen =
    mkAsync_ $ C.b_read
                (fromIntegral bid) path
                (maybe 0 id fileOffset)
                (maybe (-1) id numFrames)
                (maybe 0 id bufferOffset)
                leaveOpen

data HeaderFormat =
    Aiff
  | Next
  | Wav
  | Ircam
  | Raw
  deriving (Enum, Eq, Read, Show)

data SampleFormat =
    PcmInt8
  | PcmInt16
  | PcmInt24
  | PcmInt32
  | PcmFloat
  | PcmDouble
  | PcmMulaw
  | PcmAlaw
  deriving (Enum, Eq, Read, Show)

headerFormatString :: HeaderFormat -> String
headerFormatString Aiff  = "aiff"
headerFormatString Next  = "next"
headerFormatString Wav   = "wav"
headerFormatString Ircam = "ircam"
headerFormatString Raw   = "raw"

sampleFormatString :: SampleFormat -> String
sampleFormatString PcmInt8   = "int8"
sampleFormatString PcmInt16  = "int16"
sampleFormatString PcmInt24  = "int24"
sampleFormatString PcmInt32  = "int32"
sampleFormatString PcmFloat  = "float"
sampleFormatString PcmDouble = "double"
sampleFormatString PcmMulaw  = "mulaw"
sampleFormatString PcmAlaw   = "alaw"

b_write :: MonadIO m => Buffer -> FilePath -> HeaderFormat -> SampleFormat -> Maybe Int -> Maybe Int -> Bool -> Async m ()
b_write (Buffer bid) path headerFormat sampleFormat fileOffset numFrames leaveOpen =
    mkAsync_ $ C.b_write
                (fromIntegral bid) path
                (headerFormatString headerFormat)
                (sampleFormatString sampleFormat)
                (maybe 0 id fileOffset)
                (maybe (-1) id numFrames)
                leaveOpen

b_free :: MonadIO m => Buffer -> Async m ()
b_free b = mkAsync $ do
    let bid = bufferId b
    M.free State.bufferIdAllocator bid
    return ((), C.b_free (fromIntegral bid))

b_zero :: MonadIO m => Buffer -> Async m ()
b_zero (Buffer bid) = mkAsync_ (C.b_zero (fromIntegral bid))

b_query :: MonadIO m => Buffer -> ServerT m N.BufferInfo
b_query (Buffer bid) = C.b_query [fromIntegral bid] `M.waitFor` N.b_info bid

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
