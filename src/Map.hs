module Map 
  ( initializeMap
  , getValue
  , setValue
  , MapState
  , GlobalMap
  ) where

import Data.Map as Map
import Control.Monad.State

{- Go back and read more of section 12.3 in the programming in haskell book,
   that deals with states and is probably important for this piece
 -}

{- 
TODO: had an idea for how to approach this
potentially make a type with three different options
GET (a, map) with a being the value get is retrieving along with the map
PUT (a, map) with a being the value PUT is setting along with the modified map
ERROR (a) with a message coming back to inform the user what went wrong

map would be used within talk in Main.hs to set the new map so any new requests
to newly added keys would return the appropriate values instead of a 404 error
-}

type MapState = Map String String
type GlobalMap = State MapState

initializeMap :: MapState 
initializeMap = Map.fromList[("Hello", "there"), ("how", "are you"), ("x", "5")]

{- Got the minimum viable HTTP response from here: 
 - https://stackoverflow.com/questions/33784127/minimal-http-server-reply 
 - -}
getValue :: MapState -> String -> String
getValue dataMap key = case Map.lookup key dataMap of
  Just x -> "HTTP/1.1 200 OK\r\nContent-Length: " ++ show (Prelude.length x) ++ "\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n" ++ x ++ "\n"
  Nothing -> "HTTP/1.1 404\r\nContent-Length: 0\r\n\n"

setValue :: Map String String -> String -> String -> Map String String
setValue dataMap key value = Map.insert key value dataMap
