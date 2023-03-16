module Parse
  ( parsePacket,
    formatResponse,
    responsePacket,
    httpCode,
    responseType,
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
  | x == "PUT" = handlePut (strip y) webMap M.PLAINTEXT
  | otherwise = M.Error ("Bad request", M.PLAINTEXT, webMap)
-- Just $ M.getValue map (strip y) -- will be for setting values
parsePacket webMap _ = M.Error ("Bad request", M.PLAINTEXT, webMap)

strip :: [a] -> [a]
strip (_ : xs) = xs
strip [] = []

handlePut :: String -> ServerMap -> MapType -> GlobalMap String
handlePut request current requestType = case splitOn ";" request of
  [] -> M.Error ("Bad request", M.PLAINTEXT, current)
  [a, b] -> M.setValue current a b requestType
  _ -> M.Error ("Bad request", M.PLAINTEXT, current)

-- TODO: Divide this into another function & write tests for it
formatResponse :: GlobalMap String -> (String, ServerMap)
formatResponse currentMap = case currentMap of
  Put (value, _, current) -> ("HTTP/1.1 201 Created\r\nContent-Location: " ++ value, current)
  Get (value, valueType, current) -> (responsePacket value "OK" valueType, current)
  Error (msg, valueType, current) ->
    if msg == "Not found"
      then (responsePacket msg "NF" valueType, current)
      else (responsePacket msg "BR" valueType, current)

responsePacket :: String -> String -> MapType -> String
responsePacket response httpMsg dataType =
  "HTTP/1.1 "
    ++ httpCode httpMsg
    ++ "Content-Length: "
    ++ show (Prelude.length response)
    ++ responseType dataType
    ++ response
    ++ "\n"

httpCode :: String -> String
httpCode respType = case respType of
  "OK" -> "200 OK\r\n"
  "NF" -> "404\r\n"
  _ -> "400\r\n"

responseType :: MapType -> String
responseType t = case t of
  HTML -> "\r\nContent-Type: text/html; charset=utf-8\r\n\r\n"
  PLAINTEXT -> "\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n"