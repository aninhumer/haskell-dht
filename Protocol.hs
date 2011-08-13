
module DHT.Protocol () where

import Data.DHT.Node
import Data.BEncode
import Data.LargeWord (Word160)
import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.Map

--Note token still needs to be inserted somewhere

type BDictMap = Map String BEncode
type PacketTemplate = BDictMap

nodeBytes :: (NodeID a) => a -> ByteString
nodeBytes = undefined

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
