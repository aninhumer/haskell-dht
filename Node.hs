
module Data.DHT.Node
    (Node, NodeID, isGood, lastSeen, getNodeID, nodeBit, nodeDist) where

import Data.Bits
import Data.LargeWord

type NodeID = Word160

data Node = Node {
    isGood    :: Bool,
    lastSeen  :: Integer,
    getNodeID :: NodeID
}

nodeBit :: Int -> Node -> Bool
nodeBit i = (`testBit` i) . getNodeID

nodeDist :: NodeID -> Node -> Word160
nodeDist local node = local `xor` getNodeID node

