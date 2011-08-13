
{-# LANGUAGE TypeSynonymInstances #-}

module Data.DHT.Node (
    NodeID, nodeID,
    Node, isGood, lastSeen,
    nodeEq, nodeBit, nodeDist, nodeAge, nodeBools, nodeBytes) where

import Data.Bits
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.LargeWord
import Data.ByteString.Lazy (ByteString)
import Data.Binary.Put
import Control.Monad

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
nodeAge n = subtract (lastSeen n) `liftM` getPOSIXTime

nodeBools :: (NodeID a) => a -> [Bool]
nodeBools node = map (`nodeBit` node) $ reverse [0..159]

word160Bytes :: Word160 -> ByteString
word160Bytes word = runPut $ do
    putWord64be . hiHalf . hiHalf $ word
    putWord64be . loHalf . hiHalf $ word
    putWord32be . loHalf $ word

nodeBytes :: (NodeID a) => a -> ByteString
nodeBytes = word160Bytes . nodeID
