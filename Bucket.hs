
module Data.DHTBucket (BucketTable, addNode, getNear) where
import Data.List (partition, sortBy)
import Data.Bits
import Data.LargeWord (Word160)

type NodeID = Word160

data Node = Node {
    isGood    :: Bool,
    lastSeen  :: Integer,
    getNodeID :: NodeID
    }

nodeBit i = (`testBit` i) . getNodeID

nodeDist :: NodeID -> Node -> Word160
nodeDist local node = local `xor` getNodeID node

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

getNear :: BucketTable -> [Node]
getNear (BucketTable local bs) =
    take 8 . sortBy nodeCompare . concat $ bs
    where nodeCompare n1 n2 = compare (dist n1) (dist n2)
          dist = nodeDist local

