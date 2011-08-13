
module DHT.Protocol () where

import Data.DHT.Node
import Data.BEncode
import Data.LargeWord (Word160)
import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.Map
import Data.Binary (encode)
import System.Random
import Control.Monad

type BDictMap = Map String BEncode
type PacketTemplate = BDictMap

prepare :: PacketTemplate -> IO ByteString
prepare packet = do
    token <- liftM encode (randomIO :: IO Int)
    return . format $ insert "t" (BString token) packet
    where format = bPack . BDict

bString :: String -> BEncode
bString = BString . pack

query :: Word160 -> String -> BDictMap -> PacketTemplate
query local head body =
    fromList [ ("y", bString "q"), ("q", bString head), ("a", BDict args ) ]
    where args = body `union` singleton "id" (BString $ nodeBytes local)

reply :: Word160 -> BDictMap -> PacketTemplate
reply local body =
    fromList [ ("y", bString "r"), ("r", BDict args) ]
    where args = body `union` singleton "id" (BString $ nodeBytes local)

error :: Integer -> String -> PacketTemplate
error code msg = fromList [
    ("y", bString "e"),
    ("e", BList [BInt code, bString msg]) ]

ping :: Word160 -> PacketTemplate
ping local = query local "ping" empty

pong :: Word160 -> PacketTemplate
pong local = reply local empty

findNode :: (NodeID a) => Word160 -> a -> PacketTemplate
findNode local target =
    query local "find_node" $ singleton "target" $ BString $ nodeBytes target

getPeers :: Word160 -> ByteString -> PacketTemplate
getPeers local hash =
    query local "get_peers" $ singleton "info_hash" $ BString hash

announcePeer :: Word160 -> ByteString -> Integer -> PacketTemplate
announcePeer local hash port =
    query local "announce_peer" $ fromList [
        ("info_hash", BString hash),
        ("port", BInt port) ]

