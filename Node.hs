
{-# LANGUAGE TypeSynonymInstances #-}

module Data.DHT.Node
    (Node, NodeID, nodeID, isGood, lastSeen, nodeEq, nodeBit, nodeDist) where

import Data.Bits
import Data.LargeWord (Word160)

class NodeID a where
    nodeID :: a -> Word160

instance (NodeID Word160) where
    nodeID = id

data Node = Node {
    isGood    :: Bool,
    lastSeen  :: Integer,
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


