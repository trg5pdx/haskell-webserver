module Main (main) where

import Control.Exception as E
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
  addr <- resolveAddr mhost port
  E.bracket (openConn addr) L.close (mapOperations serverMap)

resolveAddr :: Maybe HostName -> ServiceName -> IO AddrInfo
resolveAddr mhost port = do
  let hints =
        defaultHints
          { addrFlags = [AI_PASSIVE],
            addrSocketType = Stream
          }
  addr : _ <- getAddrInfo (Just hints) mhost (Just port)
  return addr

-- | sets up the connection for accepting clients
openConn :: AddrInfo -> IO Socket
openConn addr = E.bracketOnError (openSocket addr) L.close $ \sock -> do
  setSocketOption sock ReuseAddr 1
  withFdSocket sock setCloseOnExecIfNeeded
  L.bind sock $ addrAddress addr
  L.listen sock
  return sock

-- | waits for requests, processes requests, and sends back a response, closes
-- | conn. and loops with the updated map so new clients see the changes
mapOperations :: ServerMap -> Socket -> IO a
mapOperations serverMap sock = do
  (conn, _peer) <- L.accept sock
  msg <- timeout 10000000 (L.recv conn)
  case msg of
    Nothing -> do
      L.close sock
      mapOperations serverMap sock
    Just validMsg ->
      do
        BSU.putStrLn validMsg
        let (response, currentMap) = P.formatResponse $ prepareMessage validMsg serverMap
        L.send conn (BSU.pack response)
        Prelude.putStrLn response
        print currentMap
        gracefulClose conn 5000
        mapOperations currentMap sock
  where
    prepareMessage msg dataMap = P.parsePacket dataMap (BSU.unpack msg)
