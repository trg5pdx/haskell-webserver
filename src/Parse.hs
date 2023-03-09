module Parse ( parsePacket ) where

-- import Data.List (stripPrefix)
import Data.Map as DM
import Map as M
import Test.HUnit

notFoundErr :: String
notFoundErr = "HTTP/1.1 400\r\nContent-Length: 0\r\n\n"

parsePacket :: MapState -> [String] -> GlobalMap String 
parsePacket webMap (x:y:_) | x == "GET" = M.getValue webMap (strip y)
                           | x == "PUT" = (M.setValue webMap (strip y) (strip y))
                           | otherwise = M.Error (notFoundErr, webMap)
                            -- Just $ M.getValue map (strip y) -- will be for setting values
  where
    strip (_:xs) = xs
    strip [] = []

parsePacket webMap [] = M.Error (notFoundErr, webMap)
parsePacket webMap [[]] = M.Error (notFoundErr, webMap)
parsePacket webMap (_:_) = M.Error (notFoundErr, webMap)

testParsePacket :: Test
testParsePacket = "testParsePacket" ~:
  TestList [ 
    parsePacket (DM.fromList[("Hello", "there"), ("n", "10")]) ["GET", "/n", "HTTP/1.1"]
             ~?= Get ("HTTP/1.1 200 OK\r\nContent-Length: 2\r\n"
                     ++ "Content-Type: text/plain: charset=utf-8\r\n\r\n"
                     ++ "10\n",
                     DM.fromList[("Hello", "there"), ("n", "10")]
                    ),
    parsePacket (DM.fromList[("Hello", "there"), ("n", "10")]) ["Hi", "this isn't a real request"]
             ~?= Error ("HTTP/1.1 400\r\nContent-Length: 0\r\n\n", 
                    (DM.fromList[("Hello", "there"), ("n", "10")]))
      ]