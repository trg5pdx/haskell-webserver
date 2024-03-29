module Networking
  ( Networking,
    Networking.connect,
    Networking.bind,
    Networking.listen,
    Networking.accept,
    Networking.send,
    Networking.recv,
    Networking.close,
  )
where

import Data.ByteString as S
import Network.Socket as N
import Network.Socket.ByteString as NB (recv, sendAll)

class Networking s where
  connect :: Socket -> SockAddr -> s ()
  bind :: Socket -> SockAddr -> s ()
  listen :: Socket -> s ()
  accept :: Socket -> s (Socket, SockAddr)
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
