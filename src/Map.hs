module Map 
  ( initializeMap
  , getValue
  , setValue
  ) where

import Data.Map as Map

initializeMap :: Map String String
initializeMap = Map.fromList[("Hello", "there"), ("how", "are you"), ("x", "5")]

{- Got the minimum viable HTTP response from here: 
 - https://stackoverflow.com/questions/33784127/minimal-http-server-reply 
 - -}
getValue :: Map String String -> String -> String
getValue dataMap key = case Map.lookup key dataMap of
  Just x -> "HTTP/1.1 200 OK\r\nContent-Length: " ++ show (Prelude.length x) ++ "\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n" ++ x ++ "\n"
  Nothing -> "HTTP/1.1 404\r\nContent-Length: 0\r\n\n"

setValue :: Map String String -> String -> String -> Map String String
setValue dataMap key value = Map.insert key value dataMap
