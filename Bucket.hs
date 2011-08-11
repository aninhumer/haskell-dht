
module Data.DHT.Bucket (BucketTable, addNode, getNear) where

import Data.DHT.Node
import Data.List
import Data.Ord (comparing)
import Data.Bits
import Data.LargeWord (Word160)
import Control.Monad

data BucketTable = BucketTable Word160 [[Node]]

allNodes :: BucketTable -> [Node]
allNodes (BucketTable _ bs) = concat bs

addNode :: Node -> BucketTable -> BucketTable
addNode new (BucketTable local bucket) = let
    add _ [] = [[new]]
    add a (b:bs)
        | takeNext        = b : add (a - 1) bs
        | length b < 8    = (new:b) : bs
        | not $ null rest = (new:goods ++ tail rest) : bs
        | null bs         = add a [wides,locals]
        | otherwise       = b:bs
        where takeNext = (not . null) bs && isLocal new
              isLocal n = nodeBit a n == testBit local a
              (goods, rest) = span isGood b
              (locals, wides) = partition isLocal b
    in BucketTable local $ add 159 bucket

findNode :: (NodeID a) => a -> BucketTable -> Maybe Node
findNode target (BucketTable local buckets) =
    find (nodeEq target) bucket
  where
    bucket = case filter fst $ zip bools buckets of
                  ((_,b):_) -> b
                  [] -> last buckets
    bools = nodeBools $ nodeDist local target

getNear :: (NodeID a) => a -> BucketTable -> [Node]
getNear target = take 8 . sortBy (comparing $ nodeDist target) . allNodes

getOld :: BucketTable -> IO [Node]
getOld = filterM (liftM (> 900) . nodeAge) . allNodes
