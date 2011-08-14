
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Data.BData (
    BEncode, BData,
    BDictW, BListW,
    bEntry, bAppend,
    bDataPack ) where

import Data.BEncode
import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.Map hiding (map)
import Control.Monad.Writer

class BData a where
    bWrap :: a -> BEncode

instance BData BEncode where
    bWrap = id

instance BData ByteString where
    bWrap = BString

instance BData String where
    bWrap = BString . pack

instance BData Integer where
    bWrap = BInt

instance BData Int where
    bWrap = BInt . toInteger

instance BData [BEncode] where
    bWrap = BList

instance BData (Map String BEncode) where
    bWrap = BDict

type BDictW = Writer [(String, BEncode)] ()

instance BData BDictW where
    bWrap = BDict . fromList . execWriter

bEntry :: (BData a) => String -> a -> BDictW
bEntry k v = writer ((), [(k, bWrap v)])

instance BData BListW where
    bWrap = BList . execWriter

type BListW = Writer [BEncode] ()

bAppend :: (BData a) => a -> BListW
bAppend v = writer ((), [bWrap v])

bDataPack :: (BData a) => a -> ByteString
bDataPack = bPack . bWrap

