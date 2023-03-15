module Main (main) where

{- The code below was pulled from the network socket page linked below,
 - which had an example for a basic web server
 - Link: https://hackage.haskell.org/package/network-3.1.2.7/docs/Network-Socket.html
 -}

import Control.Exception as E
-- https://stackoverflow.com/questions/3232074/what-is-the-best-way-to-convert-string-to-bytestring
import Data.ByteString.Char8 as BSU
import Data.List.Split as DS
import Map as M
import Network.Socket
import Networking as L
import Parse as P

main :: IO ()
main = do
  runTCPServer Nothing "4700"

runTCPServer :: Maybe HostName -> ServiceName -> IO a
runTCPServer mhost port = withSocketsDo $ do
  let serverMap = M.initializeMap
  addr <- resolve
  E.bracket (open addr) L.close (runOp serverMap)
  where
    resolve = do
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
              }
      Prelude.head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = E.bracketOnError (openSocket addr) L.close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      L.bind sock $ addrAddress addr
      L.listen sock
      return sock

runOp :: ServerMap -> Socket -> IO a
runOp serverMap sock = do
  (conn, _peer) <- L.accept sock
  msg <- L.recv conn
  let (response, currentMap) = P.formatResponse $ prepareMessage msg serverMap
  L.send conn (BSU.pack response)
  print response
  print currentMap
  gracefulClose conn 5000
  runOp currentMap sock
  where
    prepareMessage msg dataMap = P.parsePacket dataMap (DS.splitOn " " (BSU.unpack msg))
