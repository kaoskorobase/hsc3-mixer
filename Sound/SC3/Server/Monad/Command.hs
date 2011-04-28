{-# LANGUAGE FlexibleInstances
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses #-}
module Sound.SC3.Server.Monad.Command
  (
  -- * Master controls
    status
  , PrintLevel(..)
  , dumpOSC
  -- * Synth definitions
  , SynthDef(name)
  -- , d_recv
  -- , d_load
  -- , d_loadDir
  , d_named
  , d_default
  , d_new
  , d_free
  -- * Resources
  -- ** Nodes
  , Node(..)
  , AddAction(..)
  , n_after
  , n_before
  , n_fill
  , n_free
  , BusMapping(..)
  , n_query_
  , n_query
  , n_run_
  , n_set
  , n_setn
  , n_trace
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
  , Bus(..)
  , busId
  , numChannels
  , AudioBus(..)
  , newHardwareBus
  , newAudioBus
  , ControlBus(..)
  , newControlBus
  -- * ServerT monad
  , module Sound.SC3.Server.Monad
  -- * SendT monad
  , module Sound.SC3.Server.Monad.Send
  ) where

import qualified Codec.Digest.SHA as SHA
import           Control.Arrow (first)
import           Control.Monad (liftM)
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Char8 as B
import           Sound.SC3 (Rate(..), UGen)
import qualified Sound.SC3.Server.Allocator.Range as Range
import           Sound.SC3.Server.Monad hiding (sync, unsafeSync)
import qualified Sound.SC3.Server.Monad as M
import           Sound.SC3.Server.Monad.Send
import qualified Sound.SC3.Server.State as State
import qualified Sound.SC3.Server.Synthdef as Synthdef
import           Sound.SC3.Server.Command (AddAction(..), PrintLevel(..))
import qualified Sound.SC3.Server.Command as C
import qualified Sound.SC3.Server.Command.Completion as C
import qualified Sound.SC3.Server.Notification as N
import           Sound.OpenSoundControl (OSC(..))

-- ====================================================================
-- Utils

-- | Construct a function suitable for 'mkAsync'.
mkC :: a -> (OSC -> a) -> (Maybe OSC -> a)
mkC f _ Nothing    = f
mkC _ f (Just osc) = f osc

-- ====================================================================
-- Master controls

status :: MonadIO m => ServerT m N.Status
status = M.waitFor C.status N.status_reply

dumpOSC :: MonadIO m => PrintLevel -> ServerT m ()
dumpOSC p = M.sync (C.dumpOSC p)

-- ====================================================================
-- Synth definitions

newtype SynthDef = SynthDef {
    name  :: String
  } deriving (Eq, Show)

d_named :: String -> SynthDef
d_named = SynthDef

d_default :: SynthDef
d_default = d_named "default"

graphName = SHA.showBSasHex . SHA.hash SHA.SHA256 . B.pack . show . Synthdef.synth

d_new :: Monad m => String -> UGen -> Async m SynthDef
d_new prefix ugen = mkAsync $ return (sd, f)
    where
        -- FIXME: Synthdef names can only be 31 bytes long!
        -- sd = SynthDef (prefix ++ "-" ++ graphName ugen)
        sd = SynthDef prefix
        f osc = (mkC C.d_recv C.d_recv' osc) (Synthdef.synthdef (name sd) ugen)

-- | Remove definition once all nodes using it have ended.
d_free :: Monad m => SynthDef -> SendT m ()
d_free = sendMsg . C.d_free . (:[]) . name

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

-- | Place node @a@ after node @b@.
n_after :: (Node a, Node b, Monad m) => a -> b -> SendT m ()
n_after a b = sendMsg $ C.n_after [(fromIntegral (nodeId a), fromIntegral (nodeId b))]

-- | Place node @a@ before node @b@.
n_before :: (Node a, Node b, Monad m) => a -> b -> SendT m ()
n_before a b = sendMsg $ C.n_after [(fromIntegral (nodeId a), fromIntegral (nodeId b))]

-- | Fill ranges of a node's control values.
n_fill :: (Node a, Monad m) => a -> [(String, Int, Double)] -> SendT m ()
n_fill n = sendMsg . C.n_fill (fromIntegral (nodeId n))

-- | Delete a node.
n_free :: (Node a, MonadIO m) => a -> SendT m ()
n_free n = do
    sendMsg $ C.n_free [fromIntegral (nodeId n)]
    finally $ M.free M.nodeIdAllocator (nodeId n)

-- | Mapping node controls to buses.
class BusMapping n b where
    -- | Map a node's controls to read from a control bus.
    n_map :: (Node n, Bus b, Monad m) => n -> String -> b -> SendT m ()
    -- | Remove a control's mapping to a control bus.
    n_unmap :: (Node n, Bus b, Monad m) => n -> String -> b -> SendT m ()

instance BusMapping n ControlBus where
    n_map n c b = sendMsg msg
        where
            nid = fromIntegral (nodeId n)
            bid = fromIntegral (busId b)
            msg = if numChannels b > 1
                  then C.n_mapn nid [(c, bid, numChannels b)]
                  else C.n_map  nid [(c, bid)]
    n_unmap n c b = sendMsg msg
        where
            nid = fromIntegral (nodeId n)
            msg = if numChannels b > 1
                  then C.n_mapn nid [(c, -1, numChannels b)]
                  else C.n_map  nid [(c, -1)]

instance BusMapping n AudioBus where
    n_map n c b = sendMsg msg
        where
            nid = fromIntegral (nodeId n)
            bid = fromIntegral (busId b)
            msg = if numChannels b > 1
                  then C.n_mapan nid [(c, bid, numChannels b)]
                  else C.n_mapa  nid [(c, bid)]
    n_unmap n c b = sendMsg msg
        where
            nid = fromIntegral (nodeId n)
            msg = if numChannels b > 1
                  then C.n_mapan nid [(c, -1, numChannels b)]
                  else C.n_mapa  nid [(c, -1)]

-- | Query a node.
n_query_ :: (Node a, Monad m) => a -> SendT m ()
n_query_ n = sendMsg $ C.n_query [fromIntegral (nodeId n)]

-- | Query a node.
n_query :: (Node a, MonadIO m) => a -> ServerT m N.NodeNotification
n_query n = M.waitFor (C.n_query [fromIntegral (nodeId n)]) (N.n_info (nodeId n))

-- | Turn node on or off.
n_run_ :: (Node a, Monad m) => a -> Bool -> SendT m ()
n_run_ n b = sendMsg $ C.n_run [(fromIntegral (nodeId n), b)]

-- | Set a node's control values.
n_set :: (Node a, Monad m) => a -> [(String, Double)] -> SendT m ()
n_set n = sendMsg . C.n_set (fromIntegral (nodeId n))

-- | Set ranges of a node's control values.
n_setn :: (Node a, Monad m) => a -> [(String, [Double])] -> SendT m ()
n_setn n = sendMsg . C.n_setn (fromIntegral (nodeId n))

-- | Trace a node.
n_trace :: (Node a, Monad m) => a -> SendT m ()
n_trace n = sendMsg $ C.n_trace [fromIntegral (nodeId n)]

-- ====================================================================
-- Synth

newtype Synth = Synth NodeId deriving (Eq, Ord, Show)

instance Node Synth where
    nodeId (Synth nid) = nid

s_new :: MonadIO m => SynthDef -> AddAction -> Group -> [(String, Double)] -> SendT m Synth
s_new d a g xs = do
    nid <- M.alloc M.nodeIdAllocator
    sendMsg $ C.s_new (name d) (fromIntegral nid) a (fromIntegral (nodeId g)) xs
    return $ Synth nid

s_new_ :: MonadIO m => SynthDef -> AddAction -> [(String, Double)] -> SendT m Synth
s_new_ d a xs = rootNode >>= \g -> s_new d a g xs

s_release :: (Node a, MonadIO m) => Double -> a -> SendT m ()
s_release r n = do
    sendMsg (C.n_set1 (fromIntegral nid) "gate" r)
    after (N.n_end_ nid) (M.free M.nodeIdAllocator nid)
    where nid = nodeId n

-- ====================================================================
-- Group

newtype Group = Group NodeId deriving (Eq, Ord, Show)

instance Node Group where
    nodeId (Group nid) = nid

rootNode :: MonadIdAllocator m => m Group
rootNode = liftM Group M.rootNodeId

g_new :: MonadIO m => AddAction -> Group -> SendT m Group
g_new a p = do
    nid <- M.alloc State.nodeIdAllocator
    sendMsg $ C.g_new [(fromIntegral nid, a, fromIntegral (nodeId p))]
    return $ Group nid

g_new_ :: MonadIO m => AddAction -> SendT m Group
g_new_ a = rootNode >>= g_new a

g_deepFree :: Monad m => Group -> SendT m ()
g_deepFree g = sendMsg $ C.g_deepFree [fromIntegral (nodeId g)]

g_freeAll :: Monad m => Group -> SendT m ()
g_freeAll g = sendMsg $ C.g_freeAll [fromIntegral (nodeId g)]

g_head :: (Node n, Monad m) => Group -> n -> SendT m ()
g_head g n = sendMsg $ C.g_head [(fromIntegral (nodeId g), fromIntegral (nodeId n))]

g_tail :: (Node n, Monad m) => Group -> n -> SendT m ()
g_tail g n = sendMsg $ C.g_tail [(fromIntegral (nodeId g), fromIntegral (nodeId n))]

g_dumpTree :: Monad m => [(Group, Bool)] -> SendT m ()
g_dumpTree = sendMsg . C.g_dumpTree . map (first (fromIntegral . nodeId))

-- ====================================================================
-- Buffer

newtype Buffer = Buffer { bufferId :: BufferId } deriving (Eq, Ord, Show)

b_alloc :: MonadIO m => Int -> Int -> Async m Buffer
b_alloc n c = mkAsync $ do
    bid <- M.alloc State.bufferIdAllocator
    let f osc = (mkC C.b_alloc C.b_alloc' osc) (fromIntegral bid) n c
    return (Buffer bid, f)

b_read :: MonadIO m =>
    Buffer
 -> FilePath
 -> Maybe Int
 -> Maybe Int
 -> Maybe Int
 -> Bool
 -> Async m ()
b_read (Buffer bid) path
       fileOffset numFrames bufferOffset
       leaveOpen = mkAsync_ f
    where
        f osc = (mkC C.b_read C.b_read' osc)
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

b_write :: MonadIO m =>
    Buffer
 -> FilePath
 -> HeaderFormat
 -> SampleFormat
 -> Maybe Int
 -> Maybe Int
 -> Bool
 -> Async m ()
b_write (Buffer bid) path
        headerFormat sampleFormat
        fileOffset numFrames
        leaveOpen = mkAsync_ f
    where
        f osc = (mkC C.b_write C.b_write' osc)
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
    let f osc = (mkC C.b_free C.b_free' osc) (fromIntegral bid)
    return ((), f)

b_zero :: MonadIO m => Buffer -> Async m ()
b_zero (Buffer bid) = mkAsync_ f
    where
        f osc = (mkC C.b_zero C.b_zero' osc) (fromIntegral bid)

b_query :: MonadIO m => Buffer -> ServerT m N.BufferInfo
b_query (Buffer bid) = C.b_query [fromIntegral bid] `M.waitFor` N.b_info bid

-- ====================================================================
-- Bus

class Bus a where
    rate :: a -> Rate
    busIdRange :: a -> Range BusId
    freeBus :: MonadIdAllocator m => a -> m ()

busId :: Bus a => a -> BusId
busId = Range.begin . busIdRange

numChannels :: Bus a => a -> Int
numChannels = Range.size . busIdRange

newtype AudioBus = AudioBus { audioBusId :: Range BusId } deriving (Eq, Show)

newHardwareBus :: MonadIdAllocator m => Int -> Int -> m AudioBus
newHardwareBus n = return . AudioBus . Range.sized (fromIntegral n) . fromIntegral

newAudioBus :: MonadIdAllocator m => Int -> m AudioBus
newAudioBus = liftM AudioBus . M.allocRange M.audioBusIdAllocator

instance Bus AudioBus where
    rate _ = AR
    busIdRange = audioBusId
    freeBus = M.freeRange M.audioBusIdAllocator . audioBusId

newtype ControlBus = ControlBus { controlBusId :: Range BusId } deriving (Eq, Show)

newControlBus :: MonadIO m => Int -> ServerT m ControlBus
newControlBus = liftM ControlBus . M.allocRange M.controlBusIdAllocator

instance Bus ControlBus where
    rate _ = KR
    busIdRange = controlBusId
    freeBus = M.freeRange M.controlBusIdAllocator . controlBusId
