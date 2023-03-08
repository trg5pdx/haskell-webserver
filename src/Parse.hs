module Parse 
  ( parsePacket
  , handleParse
  ) where

-- import Data.List (stripPrefix)
import Data.Maybe
import Map as M
import Data.Map as Map

{- parsePacket :: String -> Maybe String
parsePacket packet = do
  xs <- stripPrefix "GET " packet
  ys <- stripPrefix "/ " xs
  _  <- stripPrefix "HTTP" ys
  return "valid packet!" -}

{- parsePacket packet = case packet of
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

notFoundErr :: String
notFoundErr = "HTTP/1.1 404\r\nContent-Length: 0\r\n\n"

parsePacket :: MapState -> [String] -> GlobalMap String 
parsePacket webMap (x:y:_) | x == "GET" = M.getValue webMap (strip y)
                           | x == "PUT" = (M.setValue webMap (strip y) y)
                           | otherwise = M.Error (notFoundErr, webMap)
                            -- Just $ M.getValue map (strip y) -- will be for setting values
  where
    strip (_:xs) = xs
    strip [] = []

parsePacket webMap [] = M.Error (notFoundErr, webMap)
parsePacket webMap [[]] = M.Error (notFoundErr, webMap)
parsePacket webMap (_:_) = M.Error (notFoundErr, webMap)

handleParse :: Maybe String -> String
handleParse = Data.Maybe.fromMaybe "Error"
