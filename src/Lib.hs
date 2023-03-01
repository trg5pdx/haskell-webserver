module Lib
    ( someFunc
    ) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Network.Socket as N
import Network.Socket.ByteString (recv, sendAll)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

class Networking s where
  -- connect :: 
  -- bind ::
  listen :: Socket -> s ()
  -- accept ::
  -- send ::
  -- recv :: 
  -- close ::

  
instance Networking IO where
  listen soc = N.listen soc 1024
