module Map 
  ( initializeMap
  , getValue
  , setValue
  , MapState
  , GlobalMap (Put, Get, Error)
  ) where

import qualified Data.Map as DM
import Test.HUnit
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
  Just x -> Get ("HTTP/1.1 200 OK\r\nContent-Length: " 
              ++ show (Prelude.length x) 
              ++ "\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n" 
              ++ x 
              ++ "\n", dataMap)
  Nothing -> Error ("HTTP/1.1 404\r\nContent-Length: 0\r\n\n", dataMap)


testGetValue :: Test
testGetValue = "testGetValue" ~:
  TestList [ getValue (DM.fromList[("Hello", "there"), ("y", "this is a test")]) "Hello"
             ~?= Get ("HTTP/1.1 200 OK\r\nContent-Length: 5\r\n"
                     ++ "Content-Type: text/plain: charset=utf-8\r\n\r\n"
                     ++ "there\n",
                     DM.fromList[("Hello", "there"), ("y", "this is a test")]
                    )]

setValue :: MapState -> String -> String -> GlobalMap String 
setValue dataMap key value = do
  let newMap = DM.insert key value dataMap 
  Put ("HTTP/1.1 201 Created\r\nContent-Location: " 
        ++ key 
        ++ "\n", newMap)

testSetValue :: Test
testSetValue = "testGetValue" ~:
  TestList [ setValue (DM.fromList[("Hello", "there"), ("y", "this is a test")]) "Hi" "AAA"
             ~?= Put ("HTTP/1.1 201 Created\r\nContent-Location: Hi\n",
                     DM.fromList[("Hello", "there"), ("y", "this is a test"), ("Hi", "AAA")]
                    )]