module Lib
    ( someFunc
    , Networking
    , Lib.connect
    , Lib.bind
    , Lib.listen
    , Lib.accept
    , Lib.send
    , Lib.recv
    , Lib.close
    ) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Network.Socket as N
import Network.Socket.ByteString as NB (recv, sendAll)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

class Networking s where
  connect :: Socket -> SockAddr -> s ()
  bind :: Socket -> SockAddr -> s ()
  {- may be possible to simplify bind, as I think SockAddr may stay 
 -   consistent between runs
 - -}
  listen :: Socket -> s ()
  accept :: Socket -> s (Socket, SockAddr) 
  -- Come back to accept, as N.accept spits out IO (Socket, SockAddr)
  send :: Socket -> S.ByteString -> s ()
  recv :: Socket -> s (S.ByteString)
  close :: Socket -> s ()

  
instance Networking IO where
  connect sock addr = N.connect sock addr 
  bind sock addr = N.bind sock addr
  listen sock = N.listen sock 1024
  accept = N.accept 
  send sock outData = NB.sendAll sock outData
  recv sock = NB.recv sock 1024
  close = N.close 
  


