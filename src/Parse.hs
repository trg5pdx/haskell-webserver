module Parse
  ( parsePacket,
    newParsePacket,
    formatResponse,
    responsePacket,
    httpCode,
    responseType,
  )
where

import Data.Bits (Bits (xor))
import Data.List.Split (splitOn)
import Map as M

{-
  currently has a bug on PUT where it won't capture the entire input a user
  enters in due to the list of strings being brought in are expected to be
  split by spaces, will probably have to switch to splitting on line
  so that isn't an issue anymore
-}

{-
Info needed for parsing packets:
if its a GET request

if its a PUT request
  - if the put request says its type
    - if it has any data

otherwise, spit out an error with an appropriate message
-}
data ResponseType = GET | PUT | ERROR | OTHER
  deriving (Eq, Show)

-- Key, value, request type, maybe for specifying type of data
type ParseState = (String, String, [String], ResponseType, MapType)

parseHeader :: [String] -> ParseState -> ParseState
parseHeader (x : y : xs) (key, value, ys, rType, NONE)
  | x == "GET" = (strip y, "", xs, GET, NONE)
  | x == "PUT" = (strip y, "", xs, PUT, NONE)
  | x == "Content-Type:" = getType (key, value, ys, rType, NONE) y -- Write helper fn to return map type
  | otherwise = (key, value, ys, rType, NONE)
parseHeader _ pState = pState

getType :: ParseState -> String -> ParseState
getType (key, value, ys, rType, NONE) xs
  | xs == "text/html" = (key, value, ys, rType, HTML)
  | xs == "text/plain" = (key, value, ys, rType, PLAINTEXT)
  | otherwise = ("", "Bad request" ++ xs, [xs], ERROR, NONE)
getType _ _ = ("", "Bad request", [], ERROR, NONE)

-- TODO: pass ParseState through and check it as iterLines loops to check if put is progressing as intended
-- and to catch potential errors
newParsePacket :: ServerMap -> String -> GlobalMap String
newParsePacket webMap packet = do
  let xs = splitOn "\r\n" packet
  iterLines webMap xs ("", "", [], OTHER, NONE)
  where
    -- need to pass through potential put value?
    iterLines currMap (x : xs) (key, value, ys, pType, mType) =
      if x == ""
        then case pType of
          PUT -> newHandlePut key (addLines xs) mType currMap
          _ -> M.Error ("Bad request" ++ key ++ value, M.PLAINTEXT, currMap)
        else do
          let (key1, value1, ys1, pType1, mType1) = parseHeader (splitOn " " x) (key, value, ys, pType, mType)
          case pType1 of
            GET -> getValue currMap key1
            PUT -> iterLines currMap xs (key1, value1, ys1, pType1, mType1)
            _ -> M.Error ("Bad request" ++ key1 ++ value1, M.PLAINTEXT, webMap)
    iterLines currMap [] _ = M.Error ("Bad ", M.PLAINTEXT, currMap)
    addLines (x : xs) = x ++ addLines xs
    addLines [] = []

parsePacket :: ServerMap -> [String] -> GlobalMap String
parsePacket webMap (x : y : _)
  | x == "GET" = M.getValue webMap (strip y)
  | x == "PUT" = handlePut (strip y) webMap M.PLAINTEXT
  | otherwise = M.Error ("Bad request", M.PLAINTEXT, webMap)
parsePacket webMap _ = M.Error ("Bad request", M.PLAINTEXT, webMap)

strip :: [a] -> [a]
strip (_ : xs) = xs
strip [] = []

newHandlePut :: String -> String -> MapType -> ServerMap -> GlobalMap String
newHandlePut key value valueType webMap = case valueType of
  NONE -> M.Error ("Bad request", M.PLAINTEXT, webMap)
  _ -> M.setValue webMap key value valueType

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
  NONE -> "" -- Come back to this, this is bad