
module Protocol.DHT () where

import Data.DHT.Node
import Data.BData
import Data.LargeWord (Word160)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Binary (encode)
import System.Random (randomIO)
import Control.Monad
import Control.Monad.Writer
import Network.Socket (SockAddr)

type BDictPairs = [(String, BEncode)]
type Token = ByteString

type InfoHash = Word160
type Peer = SockAddr

data Message = Query QueryT
             | Reply ReplyT
             | Error Int String

data QueryT = Ping
            | FindNode Word160
            | GetPeers InfoHash
            | AnnouncePeer InfoHash Integer Token

data ReplyT = Pong
            | FoundNode [Node]
            | GotPeers (Either [Node] [Peer]) Token
            | Announced

compactPeer :: Peer -> ByteString
compactPeer = undefined

getName :: QueryT -> String
getName Ping                 = "ping"
getName (FindNode _)         = "find_node"
getName (GetPeers _)         = "get_peers"
getName (AnnouncePeer _ _ _) = "announce_peer"

queryArgs :: QueryT -> BDictW
queryArgs Ping = return ()
queryArgs (FindNode target) =
    bEntry "target" $ nodeBytes target
queryArgs (GetPeers hash) =
    bEntry "info_hash" $ nodeBytes hash
queryArgs (AnnouncePeer hash port token) = do
    bEntry "info_hash" $ nodeBytes hash
    bEntry "port" port
    bEntry "token" token

replyArgs :: ReplyT -> BDictW
replyArgs Pong = return ()
replyArgs (FoundNode nodes) =
    bEntry "nodes" $ BS.concat $ map compactNode nodes
replyArgs (GotPeers (Left nodes) token) =
    bEntry "nodes" $ BS.concat $ map compactNode nodes
replyArgs (GotPeers (Right peers) token) =
    bEntry "peers" $ BS.concat $ map compactPeer peers

packMessage :: Message -> Word160 -> Token -> ByteString
packMessage (Query q) local token = bDataPack $ do
    bEntry "t" token
    bEntry "y" "q"
    bEntry "q" $ getName q
    bEntry "a" $ do
        bEntry "id" $ nodeBytes local
        queryArgs q
packMessage (Reply r) local token = bDataPack $ do
    bEntry "t" token
    bEntry "y" "r"
    bEntry "r" $ do
        bEntry "id" $ nodeBytes local
        replyArgs r
packMessage (Error code msg) _ token = bDataPack $ do
    bEntry "t" token
    bEntry "y" "e"
    bEntry "e" $ do
        bAppend code
        bAppend msg

genToken :: IO ByteString
genToken = liftM encode (randomIO :: IO Int)

encodePacket :: Message -> Word160 -> IO ByteString
encodePacket msg local = liftM (packMessage msg local) genToken

decodePacket :: ByteString -> Either String Message
decodePacket = undefined

