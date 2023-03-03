module Lib
    ( Networking
    , Lib.connect
    , Lib.bind
    , Lib.listen
    , Lib.accept
    , Lib.send
    , Lib.recv
    , Lib.close
    ) where

import qualified Data.ByteString as S
import Network.Socket as N
import Network.Socket.ByteString as NB (recv, sendAll)

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
  recv :: Socket -> s S.ByteString
  close :: Socket -> s ()

  
instance Networking IO where
  connect = N.connect 
  bind = N.bind
  listen sock = N.listen sock 1024
  accept = N.accept 
  send = NB.sendAll 
  recv sock = NB.recv sock 1024
  close = N.close 
  


