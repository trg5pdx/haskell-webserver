module Parse 
  ( parsePacket
  , newParsePacket
  , handleParse
  ) where

import Data.List (stripPrefix)
import Data.Maybe
import Map as M
import Data.Map as Map

parsePacket :: String -> Maybe String
parsePacket packet = do
  xs <- stripPrefix "GET " packet
  ys <- stripPrefix "/ " xs
  _  <- stripPrefix "HTTP" ys
  return "valid packet!"

{- newParsePacket packet = case packet of
  (x:xs) -> if x == "GET" then
              case xs of 
                (y:_) -> return (strip y)
                [] -> Nothing
            if x == "PUT" then
              case xs of
                (y:_) -> return (strip y)
                [] -> Nothing
            else
              Nothing
  [] -> Nothing -}

newParsePacket :: Map String String -> [String] -> Maybe String
newParsePacket webMap (x:y:_) | x == "GET" = Just $ M.getValue webMap (strip y)
                           | x == "PUT" = do
                              let z = M.setValue webMap (strip y) y
                              return "Added!"
                           | otherwise = Nothing 
                            -- Just $ M.getValue map (strip y) -- will be for setting values
  where
    strip (_:xs) = xs
    strip [] = []

newParsePacket _ [] = Nothing
newParsePacket _ [[]] = Nothing
newParsePacket _ (_:_) = Nothing

handleParse :: Maybe String -> String
handleParse = Data.Maybe.fromMaybe "Error"
