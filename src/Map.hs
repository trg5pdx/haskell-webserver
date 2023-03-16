module Map
  ( initializeMap,
    getValue,
    setValue,
    MapType (HTML, PLAINTEXT),
    ServerMap,
    GlobalMap (Put, Get, Error),
  )
where

{-
  Look into Debug.trace for debugging
  Look into Control.Monad.Trans.State

  also look into IORef if I have trouble with IO later
-}

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

data MapType = HTML | PLAINTEXT
  deriving (Eq, Show)

-- TODO: add type for distinguishing between html/plaintext
type ServerMap = DM.Map String (MapType, String)

data GlobalMap a = Put (a, MapType, ServerMap) | Get (a, MapType, ServerMap) | Error (a, MapType, ServerMap)
  deriving (Eq, Show)

initializeMap :: ServerMap
initializeMap =
  DM.fromList
    [ ("Hello", (PLAINTEXT, "there")),
      ("how", (PLAINTEXT, "are you")),
      ("x", (PLAINTEXT, "5"))
    ]

{- Got the minimum viable HTTP response from here:
 - https://stackoverflow.com/questions/33784127/minimal-http-server-reply
 - -}
getValue :: ServerMap -> String -> GlobalMap String
getValue dataMap key = case DM.lookup key dataMap of
  Just (dataType, value) -> Get (value, dataType, dataMap)
  Nothing -> Error ("Not found", PLAINTEXT, dataMap)

setValue :: ServerMap -> String -> String -> MapType -> GlobalMap String
setValue dataMap key value valueType = do
  let newMap = DM.insert key (valueType, value) dataMap
  Put (key, valueType, newMap)