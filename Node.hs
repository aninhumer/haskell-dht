
{-# LANGUAGE TypeSynonymInstances #-}

module Data.DHT.Node (
    NodeID, nodeID,
    Node, isGood, lastSeen,
    nodeBools, nodeEq, nodeBit, nodeDist) where

import Data.Bits
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.LargeWord (Word160)

class NodeID a where
    nodeID :: a -> Word160

instance (NodeID Word160) where
    nodeID = id

data Node = Node {
    isGood    :: Bool,
    lastSeen  :: POSIXTime,
    getNodeID :: Word160
}

instance NodeID Node where
    nodeID = getNodeID

nodeEq :: (NodeID a, NodeID b) => a -> b -> Bool
nodeEq a b = nodeID a == nodeID b

nodeBit :: (NodeID a) => Int -> a -> Bool
nodeBit i = (`testBit` i) . nodeID

nodeDist :: (NodeID a, NodeID b) => a -> b -> Word160
nodeDist local target = nodeID local `xor` nodeID target

nodeAge :: Node -> IO NominalDiffTime
nodeAge n = do
    now <- getPOSIXTime
    return $ lastSeen n - now

nodeBools :: (NodeID a) => a -> [Bool]
nodeBools node = map (`nodeBit` node) $ reverse [0..159]

