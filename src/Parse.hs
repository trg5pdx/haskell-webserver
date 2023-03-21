module Parse
  ( parsePacket,
    formatResponse,
    responsePacket,
    -- below: only exported for tests
  )
where

import Data.List.Split (splitOn)
import Map as M
import Parse.Internal

-- | Takes a response to a client request and creates a valid HTTP response
formatResponse :: Response -> (String, ServerMap)
formatResponse currentMap = case currentMap of
  (Put, value, _, current) -> ("HTTP/1.1 201 Created\r\nContent-Location: " ++ value, current)
  (Get, value, valueType, current) -> (responsePacket value "OK" valueType, current)
  (_, msg, valueType, current) ->
    if msg == "Not found"
      then (responsePacket msg "NF" valueType, current)
      else (responsePacket msg "BR" valueType, current)

-- | Creates the response message itself using the data the parser obtained
responsePacket :: String -> String -> MapType -> String
responsePacket response httpMsg dataType =
  "HTTP/1.1 "
    ++ httpCode httpMsg
    ++ "Content-Length: "
    ++ show (Prelude.length response)
    ++ responseType dataType
    ++ response
    ++ "\n"

-- | Begins parsing the packet, splits on line and passes result down to helper
-- | functions to deal with them, with it returning the result back to main
parsePacket :: ServerMap -> String -> Response
parsePacket webMap packet = do
  let xs = splitOn "\r\n" packet
  processPacketLines webMap xs ("", "", Other, None)
