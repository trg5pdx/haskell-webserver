module Parse
  ( parsePacket,
    formatResponse,
    responsePacket,
    -- below: only exported for tests
    ParseState,
    parseHeader,
    getType,
    iterLines,
    handleHeaderData,
    handlePut,
    httpCode,
    responseType,
  )
where

import Data.List.Split (splitOn)
import Map as M

-- Key, value, request type, type of value, packet lines, header values
type ParseState = (String, String, ResponseType, MapType)

parseHeader :: [String] -> ParseState -> ParseState
parseHeader (x : y : _) (key, value, rType, NONE)
  | x == "GET" = (strip y, "", GET, NONE)
  | x == "PUT" = (strip y, "", PUT, NONE)
  | x == "Content-Type:" = getType (key, value, rType, NONE) y
parseHeader _ pState = pState

getType :: ParseState -> String -> ParseState
getType (key, value, rType, NONE) xs
  | xs == "text/html" = (key, value, rType, HTML)
  | xs == "text/plain" = (key, value, rType, PLAINTEXT)
getType _ _ = ("", " invalid content type", ERROR, NONE)

parsePacket :: ServerMap -> String -> Response
parsePacket webMap packet = do
  let xs = splitOn "\r\n" packet
  iterLines webMap xs ("", "", OTHER, NONE)

iterLines :: ServerMap -> [String] -> ParseState -> Response
iterLines currMap (x : xs) (key, value, pType, mType)
  | x == "" && pType == PUT = handlePut key xs mType currMap
  | x == "" && pType /= PUT = (ERROR, "Bad request; invalid header data", PLAINTEXT, currMap)
  | otherwise = do
      let newPState = parseHeader (splitOn " " x) (key, value, pType, mType)
      handleHeaderData currMap newPState xs
iterLines currMap [] _ = (ERROR, "Bad request: no data provided", PLAINTEXT, currMap)

strip :: [a] -> [a]
strip (_ : xs) = xs
strip [] = []

handleHeaderData :: ServerMap -> ParseState -> [String] -> Response
handleHeaderData currMap (key, value, pType, mType) xs = case pType of
  GET -> getValue currMap key
  PUT -> iterLines currMap xs (key, value, pType, mType)
  _ -> (ERROR, "Bad request: invalid headers", PLAINTEXT, currMap)

handlePut :: String -> [String] -> MapType -> ServerMap -> Response
handlePut key value valueType webMap = case valueType of
  NONE -> (ERROR, "Bad request: no type provided", PLAINTEXT, webMap)
  _ ->
    if badKeyValueCheck key value
      then (ERROR, "Bad request: no location or value provided", PLAINTEXT, webMap)
      else setValue webMap key (addLines value) valueType
  where
    addLines (x : xs) = x ++ addLines xs
    addLines [] = []

badKeyValueCheck :: String -> [String] -> Bool
badKeyValueCheck key value
  | key == "" = True
  | null value = True
  | value == [""] = True
  | otherwise = False

formatResponse :: Response -> (String, ServerMap)
formatResponse currentMap = case currentMap of
  (PUT, value, _, current) -> ("HTTP/1.1 201 Created\r\nContent-Location: " ++ value, current)
  (GET, value, valueType, current) -> (responsePacket value "OK" valueType, current)
  (_, msg, valueType, current) ->
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
  NONE -> ""