module Parse
  ( parsePacket,
    formatResponse,
    responsePacket,
    httpCode,
  )
where

import Data.List.Split (splitOn)
import Map as M

{-
  currently has a bug on PUT where it won't capture the entire input a user
  enters in due to the list of strings being brought in are expected to be
  split by spaces, will probably have to switch to splitting on line
  so that isn't an issue anymore
-}
parsePacket :: ServerMap -> [String] -> GlobalMap String
parsePacket webMap (x : y : _)
  | x == "GET" = M.getValue webMap (strip y)
  | x == "PUT" = handlePut (strip y) webMap
  | otherwise = M.Error ("Bad request", webMap)
-- Just $ M.getValue map (strip y) -- will be for setting values
parsePacket webMap _ = M.Error ("Bad request", webMap)

strip :: [a] -> [a]
strip (_ : xs) = xs
strip [] = []

handlePut :: String -> ServerMap -> GlobalMap String
handlePut request current = case splitOn ";" request of
  [] -> M.Error ("Bad request", current)
  [a, b] -> M.setValue current a b
  _ -> M.Error ("Bad request", current)

-- TODO: Divide this into another function & write tests for it
formatResponse :: GlobalMap String -> (String, ServerMap)
formatResponse currentMap = case currentMap of
  Put (value, current) -> ("HTTP/1.1 201 Created\r\nContent-Location: " ++ value, current)
  Get (value, current) -> (responsePacket value "OK", current)
  Error (msg, current) ->
    if msg == "Not found"
      then (responsePacket msg "NF", current)
      else (responsePacket msg "BR", current)

responsePacket :: String -> String -> String
responsePacket response respType =
  "HTTP/1.1 "
    ++ httpCode respType
    ++ "Content-Length: "
    ++ show (Prelude.length response)
    ++ "\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n"
    ++ response
    ++ "\n"

httpCode :: String -> String
httpCode respType = case respType of
  "OK" -> "200 OK\r\n"
  "NF" -> "404\r\n"
  _ -> "400\r\n"
