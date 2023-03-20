module Parse
  ( parsePacket,
    formatResponse,
    responsePacket,
    -- below: only exported for tests
    ParseState,
    parseHeader,
    getDataType,
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
parseHeader (x : y : _) (key, value, rType, None)
  | x == "Get" = (strip y, "", Get, None)
  | x == "Put" = (strip y, "", Put, None)
  | x == "Content-Type:" = getDataType (key, value, rType, None) y
parseHeader _ pState = pState

parsePacket :: ServerMap -> String -> Response
parsePacket webMap packet = do
  let xs = splitOn "\r\n" packet
  iterLines webMap xs ("", "", Other, None)

iterLines :: ServerMap -> [String] -> ParseState -> Response
iterLines currMap (x : xs) (key, value, pType, mType)
  | x == "" && pType == Put = handlePut key xs mType currMap
  | x == "" && pType /= Put = (Error, "Bad request; invalid header data", Plaintext, currMap)
  | otherwise = do
      let newPState = parseHeader (splitOn " " x) (key, value, pType, mType)
      handleHeaderData currMap newPState xs
iterLines currMap [] _ = (Error, "Bad request: no data provided", Plaintext, currMap)

strip :: [a] -> [a]
strip (_ : xs) = xs
strip [] = []

handleHeaderData :: ServerMap -> ParseState -> [String] -> Response
handleHeaderData currMap (key, value, pType, mType) xs = case pType of
  Get -> getValue currMap key
  Put -> iterLines currMap xs (key, value, pType, mType)
  _ -> (Error, "Bad request: invalid headers", Plaintext, currMap)

handlePut :: String -> [String] -> MapType -> ServerMap -> Response
handlePut key value valueType webMap = case valueType of
  None -> (Error, "Bad request: no type provided", Plaintext, webMap)
  _ ->
    if badKeyValueCheck key value
      then (Error, "Bad request: no location or value provided", Plaintext, webMap)
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
  (Put, value, _, current) -> ("HTTP/1.1 201 Created\r\nContent-Location: " ++ value, current)
  (Get, value, valueType, current) -> (responsePacket value "OK" valueType, current)
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

getDataType :: ParseState -> String -> ParseState
getDataType (key, value, rType, None) xs
  | xs == "text/html" = (key, value, rType, Html)
  | xs == "text/plain" = (key, value, rType, Plaintext)
getDataType _ _ = ("", " invalid content type", Error, None)

responseType :: MapType -> String
responseType t = case t of
  Html -> "\r\nContent-Type: text/html; charset=utf-8\r\n\r\n"
  Plaintext -> "\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n"
  None -> ""