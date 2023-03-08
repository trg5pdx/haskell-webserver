module Main (main) where

{- The code below was pulled from the network socket page linked below, 
 - which had an example for a basic web server
 - Link: https://hackage.haskell.org/package/network-3.1.2.7/docs/Network-Socket.html
 -}


import Lib as L
import Parse as P
import Map as M
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Network.Socket
import Data.List.Split as DS

-- https://stackoverflow.com/questions/3232074/what-is-the-best-way-to-convert-string-to-bytestring
import Data.ByteString.Char8 as BSU

{-
userInput :: IO () 
userInput = Prelude.getLine >>= \str -> Prelude.putStrLn str
-}

main :: IO ()
main = do
  let serverMap = M.initializeMap 
  runTCPServer Nothing "3000" (talk serverMap)

talk :: MapState -> Socket -> IO () 
talk serverMap s = do
  msg <- L.recv s
  unless (S.null msg) $ do
    BSU.putStrLn msg
    case processMsg msg serverMap of
      Put (value, newMap) -> sendLoop s value newMap
      Get (value, currMap) -> sendLoop s value currMap
      Error (value, currMap) -> sendLoop s value currMap
    -- sendLoop s result serverMap
  where 
    processMsg packet dataMap = P.parsePacket dataMap (DS.splitOn " " (BSU.unpack packet))
    sendLoop sock msg updatedMap = do
      L.send sock (BSU.pack msg)
      talk updatedMap sock

-- from the "network-run" package
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) L.close loop
    -- cmd
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        Prelude.head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = E.bracketOnError (openSocket addr) L.close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        L.bind sock $ addrAddress addr
        L.listen sock
        return sock
    loop sock = forever $ E.bracketOnError (L.accept sock) (L.close . fst)
        $ \(conn, _peer) -> void $
            -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
            -- but 'E.bracketOnError' above will be necessary if some
            -- non-atomic setups (e.g. spawning a subprocess to handle
            -- @conn@) before proper cleanup of @conn@ is your case
            forkFinally (server conn) (const $ gracefulClose conn 5000)
    -- cmd = forever userInput
