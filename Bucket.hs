
module Data.DHTBucket (Bucket, addNode) where
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

data BucketTree = BucketTree NodeID Bucket

data Bucket = Branch [Node] Bucket | End

isEnd End = True
isEnd _ = False

addNode :: Node -> BucketTree -> BucketTree
addNode new (BucketTree local bucket) = let
    add _ End = Branch [new] End
    add a (Branch nodes next)
        | takeNext         = Branch nodes (add (a - 1) next)
        | length nodes < 8 = Branch (new:nodes) next
        | not $ null rest  = Branch (new:goods ++ tail rest) next
        | isEnd next       = add a (Branch wides (Branch locals End))
        | otherwise        = Branch nodes next
      where
        takeNext = (not . isEnd) next && nodeBit a new == localBit
        localBit = testBit local a
        (goods, rest) = span isGood nodes
        (locals, wides) = partition ((== localBit) . nodeBit a) nodes
    in BucketTree local $ add 159 bucket
