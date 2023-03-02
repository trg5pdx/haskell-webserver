module Parse 
  ( parsePacket
  , handleParse
  ) where

import Data.List as D
import Data.List.Split as DS

parsePacket :: String -> Maybe String
parsePacket packet = do
  xs <- D.stripPrefix "GET " packet
  ys <- D.stripPrefix "/ " xs
  _ <- D.stripPrefix "HTTP" ys
  return "valid packet!"

newParsePacket :: [String] -> Maybe String
newParsePacket packet = case packet of
  (x:xs) -> if x == "GET" then
                case xs of 
                  (y:ys) -> return y
                  [] -> Nothing
              else
                Nothing
  [] -> Nothing

handleParse :: Maybe String -> String
handleParse xs = case xs of
                   Nothing -> "Error" 
                   Just x -> x
