module Main (main) where

{- The code below was pulled from the network socket page linked below,
 - which had an example for a basic web server
 - Link: https://hackage.haskell.org/package/network-3.1.2.7/docs/Network-Socket.html
 -}

import Control.Concurrent (forkFinally)
import Control.Exception as E
import Control.Monad (forever, unless, void)
import Control.Monad.State
import Data.ByteString as S
-- https://stackoverflow.com/questions/3232074/what-is-the-best-way-to-convert-string-to-bytestring
import Data.ByteString.Char8 as BSU
import Data.List.Split as DS
import Map as M
import Network as L
import Network.Socket
import Parse as P
import qualified Parse as P
import qualified Parse as P
import Network.Socket (gracefulClose)

main :: IO ()
main = do
  runTCPServer Nothing "4700" talk

talk :: ServerMap -> Socket -> IO ()
talk serverMap s = do
  msg <- L.recv s
  unless (S.null msg) $ do
    BSU.putStrLn msg
    let result = processMsg msg serverMap
    let (response, currentMap) = P.formatResponse result
    print response
    print currentMap
    sendLoop s response currentMap
  where
    processMsg packet dataMap = P.parsePacket dataMap (DS.splitOn " " (BSU.unpack packet))
    sendLoop sock msg updatedMap = do
      L.send sock (BSU.pack msg)
      talk updatedMap sock

-- from the "network-run" package
runTCPServer :: Maybe HostName -> ServiceName -> (ServerMap -> Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
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
    runOp serverMap sock = do
      print "howdy"
      (conn, _peer) <- L.accept sock
      msg <- L.recv conn
      let result = P.parsePacket serverMap (DS.splitOn " " (BSU.unpack msg))
      let (response, currentMap) = P.formatResponse result
      L.send conn (BSU.pack response)
      -- L.close sock
      print response
      print currentMap
      gracefulClose conn 5000
      runOp currentMap sock
    loop serverMap sock = forever $
      E.bracketOnError (L.accept sock) (L.close . fst) $
        \(conn, _peer) ->
          void $
            -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
            -- but 'E.bracketOnError' above will be necessary if some
            -- non-atomic setups (e.g. spawning a subprocess to handle
            -- @conn@) before proper cleanup of @conn@ is your case
            forkFinally (server serverMap conn) (const $ gracefulClose conn 5000)
