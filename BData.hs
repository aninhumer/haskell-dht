
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Data.BData (
    BEncode, BData,
    BDictW, BListW,
    bEntry, bAppend,
    bDataPack ) where

import Data.BEncode
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as M
import Control.Monad.Writer
import Control.Monad.Reader
import Safe (atMay)

class BData a where
    bWrap   :: a -> BEncode
    bUnwrap :: BEncode -> Maybe a

instance BData BEncode where
    bWrap = id
    bUnwrap = Just . id

instance BData BS.ByteString where
    bWrap = BString
    bUnwrap (BString a) = Just a
    bUnwrap _ = Nothing

instance BData String where
    bWrap = BString . BS.pack
    bUnwrap (BString a) = Just $ BS.unpack a
    bUnwrap _ = Nothing

instance BData Integer where
    bWrap = BInt
    bUnwrap (BInt a) = Just a
    bUnwrap _ = Nothing

instance BData Int where
    bWrap = BInt . toInteger
    bUnwrap (BInt a) = Just $ fromInteger a
    bUnwrap _ = Nothing

instance BData [BEncode] where
    bWrap = BList
    bUnwrap (BList a) = Just a
    bUnwrap _ = Nothing

instance BData (M.Map String BEncode) where
    bWrap = BDict
    bUnwrap (BDict a) = Just a
    bUnwrap _ = Nothing

type BDictW = Writer [(String, BEncode)] ()

instance BData BDictW where
    bWrap = BDict . M.fromList . execWriter
    bUnwrap _ = Nothing

bEntry :: (BData a) => String -> a -> BDictW
bEntry k v = writer ((), [(k, bWrap v)])

instance BData BListW where
    bWrap = BList . execWriter
    bUnwrap _ = Nothing

type BListW = Writer [BEncode] ()

bAppend :: (BData a) => a -> BListW
bAppend v = writer ((), [bWrap v])

bDataPack :: (BData a) => a -> BS.ByteString
bDataPack = bPack . bWrap

type BDictR a = Reader BEncode (Maybe a)

bGetEntry :: (BData a) => String -> BDictR a
bGetEntry k = reader extract
    where extract b = do dict <- bUnwrap b
                         entry <- M.lookup k dict
                         bUnwrap entry

bGet :: (BData a) => BDictR a
bGet = reader $ bUnwrap

bGetIndex :: (BData a) => Int -> BDictR a
bGetIndex n = reader extract
    where extract b = do list <- bUnwrap b
                         value <- list `atMay` n
                         bUnwrap value

withBDecode :: BS.ByteString -> BDictR a -> Maybe a
withBDecode body reader =
    bRead body >>= runReader reader 

