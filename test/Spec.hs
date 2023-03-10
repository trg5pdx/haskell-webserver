import Map as M
import Parse as P
import Test.HUnit
import Data.Map as DM

main :: IO ()
main = do
  print "testing get value..."
  _ <- runTestTT testGetValue
  print "testing set value..."
  _ <- runTestTT testSetValue
  print "testing parse packet..."
  _ <- runTestTT testParsePacket
  print "testing format response..."
  _ <- runTestTT testFormatResponse
  print "finished test suite."


testGetValue :: Test
testGetValue = "testGetValue" ~:
  TestList [ getValue (DM.fromList[("Hello", "there"), ("y", "this is a test")]) "Hello"
             ~?= Get ("there", 
                DM.fromList[("Hello", "there"), ("y", "this is a test")])]

testSetValue :: Test
testSetValue = "testGetValue" ~:
  TestList [ setValue (DM.fromList[("Hello", "there"), ("y", "this is a test")]) "Hi" "AAA"
             ~?= Put ("Hi", 
                DM.fromList[("Hello", "there"), 
                            ("y", "this is a test"), 
                            ("Hi", "AAA")])]

testParsePacket :: Test
testParsePacket = "testParsePacket" ~:
  TestList [ 
    parsePacket (DM.fromList[("Hello", "there"), ("n", "10")]) ["GET", "/n", "HTTP/1.1"]
             ~?= Get ("10", DM.fromList[("Hello", "there"), ("n", "10")]),
    parsePacket (DM.fromList[("Hello", "there"), ("n", "10")]) ["Hi", "this isn't a real request"]
             ~?= Error ("Bad request", DM.fromList[("Hello", "there"), ("n", "10")]),
    parsePacket (DM.fromList[("Hello", "there"), ("n", "10")]) ["GET", "/testing", "HTTP/1.1"]
             ~?= Error ("Not found", DM.fromList[("Hello", "there"), ("n", "10")])
      ]

testFormatResponse :: Test
testFormatResponse = "testFormatResponse" ~:
  TestList [
    formatResponse (Get ("10", DM.fromList[("Hello", "there"), ("n", "10")]))
      ~?= ("HTTP/1.1 200 OK\r\nContent-Length: 2\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n10\n",
             DM.fromList[("Hello", "there"), ("n", "10")]),
    formatResponse (Put ("y", DM.fromList[("Hello", "there"), ("n", "10"), ("y", "x")]))
      ~?= ("HTTP/1.1 201 Created\r\nContent-Location: y",
             DM.fromList[("Hello", "there"), ("n", "10"), ("y", "x")]),
    formatResponse (Error ("Not found", DM.fromList[("Hello", "there"), ("n", "10"), ("y", "x")]))
      ~?= ("HTTP/1.1 404\r\nContent-Length: 9\r\nContent-Type: text/plain; charset=utf-8\r\n\r\nNot found\n",
             DM.fromList[("Hello", "there"), ("n", "10"), ("y", "x")]),
    formatResponse (Error ("Bad request", DM.fromList[("Hello", "there"), ("n", "10"), ("y", "x")]))
      ~?= ("HTTP/1.1 400\r\nContent-Length: 11\r\nContent-Type: text/plain; charset=utf-8\r\n\r\nBad request\n",
             DM.fromList[("Hello", "there"), ("n", "10"), ("y", "x")])
  ]