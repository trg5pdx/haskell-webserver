module Map 
  ( initializeMap
  , getValue
  , setValue
  , MapState
  , GlobalMap (Put, Get, Error)
  ) where

import Data.Map as Map
-- import Control.Monad.State

{- Go back and read more of section 12.3 in the programming in haskell book,
   that deals with states and is probably important for this piece
 -}

{- 
TODO: had an idea for how to approach this
potentially make a type with three different options
GET (String) with a being the value get is retrieving
PUT (String, map) with a being the value PUT is setting along with the modified map
ERROR (String) with a message coming back to inform the user what went wrong

map would be used within talk in Main.hs to set the new map so any new requests
to newly added keys would return the appropriate values instead of a 404 error
-}

type MapState = Map String String
data GlobalMap a = Put (a, MapState) | Get (a, MapState) | Error (a, MapState)
  deriving (Show)

initializeMap :: MapState 
initializeMap = Map.fromList[("Hello", "there"), ("how", "are you"), ("x", "5")]

{- Got the minimum viable HTTP response from here: 
 - https://stackoverflow.com/questions/33784127/minimal-http-server-reply 
 - -}
getValue :: MapState -> String -> GlobalMap String
getValue dataMap key = case Map.lookup key dataMap of
  Just x -> Get ("HTTP/1.1 200 OK\r\nContent-Length: " ++ show (Prelude.length x) ++ "\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n" ++ x ++ "\n", dataMap)
  Nothing -> Error ("HTTP/1.1 400\r\nContent-Length: 0\r\n\n", dataMap)

setValue :: MapState -> String -> String -> GlobalMap String 
setValue dataMap key value = Put (value, Map.insert key value dataMap)
