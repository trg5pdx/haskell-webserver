module Parse 
  ( parsePacket
  , handleParse
  ) where

import Data.List as D

parsePacket :: String -> Maybe String
parsePacket packet = do
  xs <- D.stripPrefix "GET " packet
  ys <- D.stripPrefix "/ " xs
  _ <- D.stripPrefix "HTTP" ys
  return "valid packet!"

handleParse :: Maybe String -> String
handleParse xs = case xs of
                   Nothing -> "Error" 
                   Just x -> x
