module Map
  ( initializeMap,
    getValue,
    setValue,
    ServerMap,
    GlobalMap (Put, Get, Error),
  )
where

import Data.Map as DM

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

{-
data Operation = Put (String, String) | Get String | Error String
  deriving (Eq, Show)
type ServerMap = DM.Map String String
newtype MapTransformer a = MT (ServerMap -> (a, ServerMap))
-}

type ServerMap = DM.Map String String

data GlobalMap a = Put (a, ServerMap) | Get (a, ServerMap) | Error (a, ServerMap)
  deriving (Eq, Show)

initializeMap :: ServerMap
initializeMap = DM.fromList [("Hello", "there"), ("how", "are you"), ("x", "5")]

{- Got the minimum viable HTTP response from here:
 - https://stackoverflow.com/questions/33784127/minimal-http-server-reply
 - -}
getValue :: ServerMap -> String -> GlobalMap String
getValue dataMap key = case DM.lookup key dataMap of
  Just x -> Get (x, dataMap)
  Nothing -> Error ("Not found", dataMap)

setValue :: ServerMap -> String -> String -> GlobalMap String
setValue dataMap key value = do
  let newMap = DM.insert key value dataMap
  Put (key, newMap)