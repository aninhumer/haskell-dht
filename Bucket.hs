
module Data.DHTBucket (BucketTable, addNode, getNear) where
import Data.List
import Data.Bits
import Data.LargeWord (Word160)
import Data.Function (on)

type NodeID = Word160

data Node = Node {
    isGood    :: Bool,
    lastSeen  :: Integer,
    getNodeID :: NodeID
    }

nodeBit i = (`testBit` i) . getNodeID

nodeDist :: NodeID -> Node -> Word160
nodeDist local node = local `xor` getNodeID node

log2 :: Word160 -> Int
log2 n = case findIndex (`testBit` 159) (iterate (*2) n) of
              Just i -> 159 - i
              Nothing -> 0

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
        takeNext = (not . null) bs && isLocal new
        isLocal n = nodeBit a n == testBit local a
        (goods, rest) = span isGood b
        (locals, wides) = partition isLocal b
    in BucketTable local $ add 159 bucket

findNode :: NodeID -> BucketTable -> Maybe Node
findNode nid (BucketTable local bs) =
    find ((== nid) . getNodeID) bucket
    where bucket = case drop index bs of
                        (b:_) -> b
                        [] -> last bs
          index = log2 . complement $ nid `xor` local

getNear :: BucketTable -> [Node]
getNear (BucketTable local bs) =
    take 8 . sortBy (compare `on` nodeDist local) . concat $ bs

