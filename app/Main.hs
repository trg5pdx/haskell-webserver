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
import System.Environment (getArgs)
import System.Timeout (timeout)

main :: IO ()
main = do
  args <- getArgs
  let (host, port) = getHostPort args
  runWebServer host port

getHostPort :: [String] -> (Maybe HostName, ServiceName)
getHostPort [x, y] = (Just x, y)
getHostPort _ = (Nothing, "4700")

runWebServer :: Maybe HostName -> ServiceName -> IO a
runWebServer mhost port = withSocketsDo $ do
  let serverMap = initializeMap
  addr <- resolve mhost port
  E.bracket (open addr) L.close (mapOperations serverMap)

resolve :: Maybe HostName -> ServiceName -> IO AddrInfo
resolve mhost port = do
  let hints =
        defaultHints
          { addrFlags = [AI_ADDRCONFIG],
            addrSocketType = Stream
          }
  {- addr : _ <- try $
    catch
      (getAddrInfo (Just hints) mhost (Just port))
      (\e -> do
        let _ = print e
        getAddrInfo (Just hints) Nothing (Just "4700")) -}
  addr : _ <- getAddrInfo (Just hints) mhost (Just port)
  return addr

open :: AddrInfo -> IO Socket
open addr = E.bracketOnError (openSocket addr) L.close $ \sock -> do
  setSocketOption sock ReuseAddr 1
  withFdSocket sock setCloseOnExecIfNeeded
  L.bind sock $ addrAddress addr
  L.listen sock
  return sock

mapOperations :: ServerMap -> Socket -> IO a
mapOperations serverMap sock = do
  (conn, _peer) <- L.accept sock
  msg <- timeout 10000000 (L.recv conn)
  case msg of
    Nothing -> do
      let _ = L.close sock :: IO ()
      mapOperations serverMap sock
    Just validMsg ->
      do
        print validMsg
        let (response, currentMap) = P.formatResponse $ prepareMessage validMsg serverMap
        L.send conn (BSU.pack response)
        print response
        print currentMap
        gracefulClose conn 5000
        mapOperations currentMap sock
  where
    prepareMessage msg dataMap = P.parsePacket dataMap (BSU.unpack msg)
