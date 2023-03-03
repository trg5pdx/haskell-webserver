module Parse 
  ( parsePacket
  , newParsePacket
  , handleParse
  ) where

import Data.List (stripPrefix)
import Data.Maybe

parsePacket :: String -> Maybe String
parsePacket packet = do
  xs <- stripPrefix "GET " packet
  ys <- stripPrefix "/ " xs
  _  <- stripPrefix "HTTP" ys
  return "valid packet!"

newParsePacket :: [String] -> Maybe String
newParsePacket packet = case packet of
  (x:xs) -> if x == "GET" then
              case xs of 
                (y:_) -> return y
                [] -> Nothing
            else
              Nothing
  [] -> Nothing

handleParse :: Maybe String -> String
handleParse = Data.Maybe.fromMaybe "Error"
