module Main (main) where

{- The code below was pulled from the network socket page linked below,
 - which had an example for a basic web server
 - Link: https://hackage.haskell.org/package/network-3.1.2.7/docs/Network-Socket.html
 -}

import Control.Concurrent
import Control.Exception as E
import Control.Monad (forever, unless, void)
import Data.ByteString as S
-- https://stackoverflow.com/questions/3232074/what-is-the-best-way-to-convert-string-to-bytestring
import Data.ByteString.Char8 as BSU
import Data.List.Split as DS
import Map as M
import Network as L
import Network.Socket
import Parse as P

main :: IO ()
main = do
  chan <- newChan
  writeChan chan M.initializeMap
  runTCPServer Nothing "4700" talk chan

talk :: ServerMap -> Socket -> Chan ServerMap -> IO ()
talk serverMap s chan = do
  msg <- L.recv s
  unless (S.null msg) $ do
    BSU.putStrLn msg
    let newMap = readChan chan
    let result = processMsg msg serverMap
    let (response, currentMap) = P.formatResponse result
    broadcast currentMap
    print response
    print currentMap
    sendLoop s response currentMap chan
  where
    broadcast = writeChan chan
    processMsg packet dataMap = P.parsePacket dataMap (DS.splitOn " " (BSU.unpack packet))
    sendLoop sock msg updatedMap chan = do
      L.send sock (BSU.pack msg)
      talk updatedMap sock chan

-- from the "network-run" package
runTCPServer :: Maybe HostName -> ServiceName -> (ServerMap -> Socket -> Chan ServerMap -> IO a) -> Chan ServerMap -> IO a
runTCPServer mhost port server chan = withSocketsDo $ do
  let serverMap = M.initializeMap
  addr <- resolve
  E.bracket (open addr) L.close (loop serverMap chan)
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
    loop serverMap chan sock = forever $
      E.bracketOnError (L.accept sock) (L.close . fst) $
        \(conn, _peer) ->
          void $
            -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
            -- but 'E.bracketOnError' above will be necessary if some
            -- non-atomic setups (e.g. spawning a subprocess to handle
            -- @conn@) before proper cleanup of @conn@ is your case
            forkFinally (server serverMap conn chan) (const $ gracefulClose conn 5000)
