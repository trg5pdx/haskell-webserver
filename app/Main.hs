module Main (main) where

{- The code below was pulled from the network socket page linked below,
 - which had an example for a basic web server
 - Link: https://hackage.haskell.org/package/network-3.1.2.7/docs/Network-Socket.html
 -}

import Control.Exception as E
-- https://stackoverflow.com/questions/3232074/what-is-the-best-way-to-convert-string-to-bytestring
import Data.ByteString.Char8 as BSU
import Map as M
import Network.Socket
import Networking as L
import Parse as P
import System.Timeout (timeout)

main :: IO ()
main = do
  runTCPServer Nothing "4700"

runTCPServer :: Maybe HostName -> ServiceName -> IO a
runTCPServer mhost port = withSocketsDo $ do
  let serverMap = initializeMap
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
  msg <- timeout 10000000 (L.recv conn)
  case msg of
    Nothing -> do
      let _ = L.close sock :: IO ()
      runOp serverMap sock
    Just validMsg ->
      do
        print validMsg
        let (response, currentMap) = P.formatResponse $ prepareMessage validMsg serverMap
        L.send conn (BSU.pack response)
        print response
        print currentMap
        gracefulClose conn 5000
        runOp currentMap sock
  where
    prepareMessage msg dataMap = P.parsePacket dataMap (BSU.unpack msg)
