module Map 
  ( initializeMap
  , getValue
  , setValue
  , MapState
  , GlobalMap (Put, Get, Error)
  ) where

import qualified Data.Map as DM

{- 
An idea for how to approach this
(implemented but needs to be changed)
potentially make a type with three different options
GET (String) with a being the value get is retrieving
PUT (String, map) with a being the value PUT is setting along with the modified map
ERROR (String) with a message coming back to inform the user what went wrong

map would be used within talk in Main.hs to set the new map so any new requests
to newly added keys would return the appropriate values instead of a 404 error
-}

type MapState = DM.Map String String
data GlobalMap a = Put (a, MapState) | Get (a, MapState) | Error (a, MapState)
  deriving (Eq, Show)

initializeMap :: MapState 
initializeMap = DM.fromList[("Hello", "there"), ("how", "are you"), ("x", "5")]

{- Got the minimum viable HTTP response from here: 
 - https://stackoverflow.com/questions/33784127/minimal-http-server-reply 
 - -}
getValue :: MapState -> String -> GlobalMap String
getValue dataMap key = case DM.lookup key dataMap of
  Just x -> Get (x, dataMap)
  Nothing -> Error ("Not found", dataMap)

setValue :: MapState -> String -> String -> GlobalMap String 
setValue dataMap key value = do
  let newMap = DM.insert key value dataMap 
  Put (key, newMap)