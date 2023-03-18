import Data.Map as DM
import Map as M
import Parse as P
import Test.HUnit

main :: IO ()
main = do
  print "testing get value..."
  _ <- runTestTT testGetValue
  print "testing set value..."
  _ <- runTestTT testSetValue
  print "testing parse packet..."
  _ <- runTestTT testParsePacket
  print "testing handle header data..."
  _ <- runTestTT testHandleHeaderData
  print "testing handle put..."
  _ <- runTestTT testHandlePut
  print "testing format response..."
  _ <- runTestTT testFormatResponse
  print "testing response packet..."
  _ <- runTestTT testResponsePacket
  print "testing http code..."
  _ <- runTestTT testHttpCode
  print "testing response type..."
  _ <- runTestTT testResponseType
  print "finished test suite."

testGetValue :: Test
testGetValue =
  "testGetValue"
    ~: TestList
      [ getValue (DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("y", (M.PLAINTEXT, "this is a test"))]) "Hello"
          ~?= Get
            ( "there",
              M.PLAINTEXT,
              DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("y", (M.PLAINTEXT, "this is a test"))]
            )
      ]

testSetValue :: Test
testSetValue =
  "testGetValue"
    ~: TestList
      [ setValue (DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("y", (M.PLAINTEXT, "this is a test"))]) "Hi" "AAA" M.PLAINTEXT
          ~?= Put
            ( "Hi",
              M.PLAINTEXT,
              DM.fromList
                [ ("Hello", (M.PLAINTEXT, "there")),
                  ("y", (M.PLAINTEXT, "this is a test")),
                  ("Hi", (M.PLAINTEXT, "AAA"))
                ]
            )
      ]

testParsePacket :: Test
testParsePacket =
  "testParsePacket"
    ~: TestList
      [ parsePacket (DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))]) "GET /n HTTP/1.1"
          ~?= Get ("10", M.PLAINTEXT, DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))]),
        parsePacket (DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))]) "Hi this isn't a real request"
          ~?= Error ("Bad request", M.PLAINTEXT, DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))]),
        parsePacket (DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))]) "GET /testing HTTP/1.1"
          ~?= Error ("Not found", M.PLAINTEXT, DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))])
      ]

{-
TODO: finish handleHeaderData test cases,
  - write tests for iterLines, getType, parseHeader
-}
testHandleHeaderData :: Test
testHandleHeaderData =
  "testHandleHeaderData"
    ~: TestList
      [ handleHeaderData
          (DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))])
          ("n", "", P.GET, M.NONE)
          [""]
          ~?= Get ("10", M.PLAINTEXT, DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))])
      ]

testHandlePut :: Test
testHandlePut =
  "testHandlePut"
    ~: TestList
      [ handlePut
          "x"
          ["<!Doctype html>", "THIS IS A PAGE", "</html>"]
          M.HTML
          (DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))])
          ~?= Put
            ( "x",
              M.HTML,
              DM.fromList
                [ ("Hello", (M.PLAINTEXT, "there")),
                  ("n", (M.PLAINTEXT, "10")),
                  ("x", (M.HTML, "<!Doctype html>THIS IS A PAGE</html>"))
                ]
            ),
        handlePut
          "blah"
          ["<!DOCTYPE HTML>"]
          M.NONE
          (DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))])
          ~?= Error
            ( "Bad request",
              M.PLAINTEXT,
              DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))]
            )
      ]

testFormatResponse :: Test
testFormatResponse =
  "testFormatResponse"
    ~: TestList
      [ formatResponse (Get ("10", M.PLAINTEXT, DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))]))
          ~?= ( "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n10\n",
                DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))]
              ),
        formatResponse
          ( Put
              ( "y",
                M.PLAINTEXT,
                DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10")), ("y", (M.PLAINTEXT, "x"))]
              )
          )
          ~?= ( "HTTP/1.1 201 Created\r\nContent-Location: y",
                DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10")), ("y", (M.PLAINTEXT, "x"))]
              ),
        formatResponse (Error ("Not found", M.PLAINTEXT, DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10")), ("y", (M.PLAINTEXT, "x"))]))
          ~?= ( "HTTP/1.1 404\r\nContent-Length: 9\r\nContent-Type: text/plain; charset=utf-8\r\n\r\nNot found\n",
                DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10")), ("y", (M.PLAINTEXT, "x"))]
              ),
        formatResponse (Error ("Bad request", M.PLAINTEXT, DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10")), ("y", (M.PLAINTEXT, "x"))]))
          ~?= ( "HTTP/1.1 400\r\nContent-Length: 11\r\nContent-Type: text/plain; charset=utf-8\r\n\r\nBad request\n",
                DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10")), ("y", (M.PLAINTEXT, "x"))]
              )
      ]

testResponsePacket :: Test
testResponsePacket =
  "testFormatResponse"
    ~: TestList
      [ responsePacket "AAA" "OK" M.PLAINTEXT
          ~?= "HTTP/1.1 200 OK\r\nContent-Length: 3\r\nContent-Type: text/plain; charset=utf-8\r\n\r\nAAA\n",
        responsePacket "Not found" "NF" M.PLAINTEXT
          ~?= "HTTP/1.1 404\r\nContent-Length: 9\r\nContent-Type: text/plain; charset=utf-8\r\n\r\nNot found\n"
      ]

testHttpCode :: Test
testHttpCode =
  "testHttpCode"
    ~: TestList
      [ httpCode "OK" ~?= "200 OK\r\n",
        httpCode "NF" ~?= "404\r\n",
        httpCode "BR" ~?= "400\r\n",
        httpCode "AAA" ~?= "400\r\n"
      ]

testResponseType :: Test
testResponseType =
  "testResponseType"
    ~: TestList
      [ responseType M.HTML ~?= "\r\nContent-Type: text/html; charset=utf-8\r\n\r\n",
        responseType M.PLAINTEXT ~?= "\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n",
        responseType M.NONE ~?= ""
      ]