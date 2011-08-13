
{-# LANGUAGE UndecidableInstances, TypeSynonymInstances, FlexibleInstances #-}

module Data.BData (BData, bWrap, bDataPack) where

import Data.BEncode
import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.Map hiding (map)

class BData a where
    bWrap :: a -> BEncode

instance BData BEncode where
    bWrap = id

instance BData ByteString where
    bWrap = BString

instance BData String where
    bWrap = BString . pack

instance (Integral a) => BData a where
    bWrap = BInt . fromIntegral 

instance (BData a) => BData [a] where
    bWrap = BList . map bWrap

instance (BData a) => BData (Map String a) where
    bWrap = BDict . fmap bWrap

instance (BData a) => BData [(String, a)] where
    bWrap = BDict . fmap bWrap . fromList

bDataPack :: (BData a) => a -> ByteString
bDataPack = bPack . bWrap

