
module Data.DHTBucket (BucketTable, addNode) where
import Data.List (partition)
import Data.Bits
import Data.LargeWord (Word160)

type NodeID = Word160

data Node = Node {
    isGood    :: Bool,
    lastSeen  :: Integer,
    getNodeID :: NodeID
    }

nodeBit i = (`testBit` i) . getNodeID

data BucketTable = BucketTable NodeID [[Node]]

addNode :: Node -> BucketTable -> BucketTable
addNode new (BucketTable local bucket) = let
    add _ [] = [[new]]
    add a (b:bs)
        | takeNext        = b : add (a - 1) bs
        | length b < 8    = (new:b) : bs
        | not $ null rest = (new:goods ++ tail rest) : bs
        | null bs         = add a [wides,locals]
        | otherwise       = b:bs
      where
        takeNext = (not . null) bs && nodeBit a new == localBit
        localBit = testBit local a
        (goods, rest) = span isGood b
        (locals, wides) = partition ((== localBit) . nodeBit a) b
    in BucketTable local $ add 159 bucket
