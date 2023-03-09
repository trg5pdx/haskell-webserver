module Parse 
  ( parsePacket
  , formatResponse
  ) where

import Data.List.Split (splitOn)
import Map as M

{- 
  currently has a bug on PUT where it won't capture the entire input a user
  enters in due to the list of strings being brought in are expected to be
  split by spaces, will probably have to switch to splitting on line
  so that isn't an issue anymore
-}
parsePacket :: MapState -> [String] -> GlobalMap String 
parsePacket webMap (x:y:_) | x == "GET" = M.getValue webMap (strip y)
                           | x == "PUT" = handlePut (strip y) webMap
                           | otherwise = M.Error ("Bad request", webMap)
                            -- Just $ M.getValue map (strip y) -- will be for setting values
parsePacket webMap _ = M.Error ("Bad request", webMap)

strip :: [a] -> [a]
strip (_:xs) = xs
strip [] = []

handlePut :: String -> MapState -> GlobalMap String
handlePut request current = case splitOn ";" request of
                            [] -> M.Error("Bad request", current)
                            [a, b] -> M.setValue current a b
                            _ -> M.Error("Bad request", current)

formatResponse :: GlobalMap String -> (String, MapState)
formatResponse currentMap = case currentMap of
  Put (value, current) -> ("HTTP/1.1 201 Created\r\nContent-Location: " ++ value, current)
  Get (value, current) -> ("HTTP/1.1 200 OK\r\nContent-Length: " 
                            ++ show (Prelude.length value) 
                            ++ "\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n" 
                            ++ value 
                            ++ "\n",
                        current)
  Error (msg, current) -> if msg == "Not found" 
                      then ("HTTP/1.1 404\r\nContent-Length: "
                            ++ show (Prelude.length msg)
                            ++ "\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n" 
                            ++ "\r\n\r\n"
                            ++ msg
                            ++ "\n", current)
                      else ("HTTP/1.1 400\r\nContent-Length: "
                            ++ show (Prelude.length msg)
                            ++ "\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n" 
                            ++ "\r\n\r\n"
                            ++ msg
                            ++ "\n", current)