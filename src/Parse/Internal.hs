module Parse.Internal
  ( ParseState,
    parseHeader,
    getDataType,
    processPacketLines,
    handleHeaderData,
    handlePut,
    httpCode,
    responseType,
  )
where

import Data.List.Split (splitOn)
import Map as M

-- Key, value, request type, type of value
type ParseState = (String, String, ResponseType, MapType)

parseHeader :: [String] -> ParseState -> ParseState
parseHeader (x : y : _) (key, value, rType, None)
  | x == "GET" = (strip y, "", Get, None)
  | x == "PUT" = (strip y, "", Put, None)
  | x == "Content-Type:" = getDataType (key, value, rType, None) y
parseHeader _ pState = pState

processPacketLines :: ServerMap -> [String] -> ParseState -> Response
processPacketLines currMap (x : xs) (key, value, pType, mType)
  | x == "" && pType == Put = handlePut key xs mType currMap
  | x == "" && pType /= Put = (Error, "Bad request; invalid header data", Plaintext, currMap)
  | otherwise = do
      let newPState = parseHeader (splitOn " " x) (key, value, pType, mType)
      handleHeaderData currMap newPState xs
processPacketLines currMap [] _ = (Error, "Bad request: no data provided", Plaintext, currMap)

strip :: [a] -> [a]
strip (_ : xs) = xs
strip [] = []

handleHeaderData :: ServerMap -> ParseState -> [String] -> Response
handleHeaderData currMap (key, value, pType, mType) xs = case pType of
  Get -> getValue currMap key
  Put -> processPacketLines currMap xs (key, value, pType, mType)
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

getDataType :: ParseState -> String -> ParseState
getDataType (key, value, rType, None) xs
  | xs == "text/html" = (key, value, rType, Html)
  | xs == "text/plain" = (key, value, rType, Plaintext)
getDataType _ _ = ("", " invalid content type", Error, None)

httpCode :: String -> String
httpCode respType = case respType of
  "OK" -> "200 OK\r\n"
  "NF" -> "404\r\n"
  _ -> "400\r\n"

responseType :: MapType -> String
responseType t = case t of
  Html -> "\r\nContent-Type: text/html; charset=utf-8\r\n\r\n"
  Plaintext -> "\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n"
  None -> ""