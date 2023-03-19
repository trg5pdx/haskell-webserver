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
  print "testing parse header..."
  _ <- runTestTT testParseHeader
  print "testing get type..."
  _ <- runTestTT testGetType
  print "testing parse packet..."
  _ <- runTestTT testParsePacket
  print "testing iter lines..."
  _ <- runTestTT testIterLines
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
      [ getValue (DM.fromList [("Hello", (PLAINTEXT, "there")), ("y", (PLAINTEXT, "this is a test"))]) "Hello"
          ~?= ( GET,
                "there",
                PLAINTEXT,
                DM.fromList [("Hello", (PLAINTEXT, "there")), ("y", (PLAINTEXT, "this is a test"))]
              ),
        getValue (DM.fromList [("Hello", (PLAINTEXT, "there")), ("y", (PLAINTEXT, "this is a test"))]) "nope"
          ~?= ( ERROR,
                "Not found",
                PLAINTEXT,
                DM.fromList [("Hello", (PLAINTEXT, "there")), ("y", (PLAINTEXT, "this is a test"))]
              )
      ]

testSetValue :: Test
testSetValue =
  "testGetValue"
    ~: TestList
      [ setValue (DM.fromList [("Hello", (PLAINTEXT, "there")), ("y", (PLAINTEXT, "this is a test"))]) "Hi" "AAA" PLAINTEXT
          ~?= ( PUT,
                "Hi",
                PLAINTEXT,
                DM.fromList
                  [ ("Hello", (PLAINTEXT, "there")),
                    ("y", (PLAINTEXT, "this is a test")),
                    ("Hi", (PLAINTEXT, "AAA"))
                  ]
              )
      ]

testParseHeader :: Test
testParseHeader =
  "testParseHeader"
    ~: TestList
      [ parseHeader ["GET", "/test", "HTTP/1.1"] ("", "", OTHER, NONE) ~?= ("test", "", GET, NONE),
        parseHeader ["PUT", "/i.html", "HTTP/1.1"] ("", "", OTHER, NONE) ~?= ("i.html", "", PUT, NONE),
        parseHeader ["Content-Type:", "text/plain"] ("a", "", PUT, NONE) ~?= ("a", "", PUT, PLAINTEXT),
        parseHeader ["Content-Type:", "text/html"] ("b", "", PUT, NONE) ~?= ("b", "", PUT, HTML),
        parseHeader ["Content-Type:", "text/"] ("c", "", PUT, NONE) ~?= ("", " invalid content type", ERROR, NONE),
        parseHeader ["not real"] ("", "", OTHER, NONE) ~?= ("", "", OTHER, NONE)
      ]

testGetType :: Test
testGetType =
  "testGetType"
    ~: TestList
      [ getType ("", "", PUT, NONE) "text/html" ~?= ("", "", PUT, HTML),
        getType ("", "", PUT, NONE) "text/plain" ~?= ("", "", PUT, PLAINTEXT),
        getType ("", "", PUT, NONE) "blahblahblah" ~?= ("", " invalid content type", ERROR, NONE)
      ]

testParsePacket :: Test
testParsePacket =
  "testParsePacket"
    ~: TestList
      [ parsePacket (DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10"))]) "GET /n HTTP/1.1"
          ~?= (GET, "10", PLAINTEXT, DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10"))]),
        parsePacket (DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10"))]) "Hi this isn't a real request"
          ~?= (ERROR, "Bad request: invalid headers", PLAINTEXT, DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10"))]),
        parsePacket (DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10"))]) "GET /testing HTTP/1.1"
          ~?= (ERROR, "Not found", PLAINTEXT, DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10"))])
      ]

testIterLines :: Test
testIterLines =
  "testIterLines"
    ~: TestList
      [ iterLines
          (DM.fromList [("index.html", (HTML, "<!Doctype html><h1>An html page</h1></html>")), ("n", (PLAINTEXT, "10"))])
          ["GET /index.html HTTP/1.1"]
          ("", "", OTHER, NONE)
          ~?= ( GET,
                "<!Doctype html><h1>An html page</h1></html>",
                HTML,
                DM.fromList [("index.html", (HTML, "<!Doctype html><h1>An html page</h1></html>")), ("n", (PLAINTEXT, "10"))]
              ),
        iterLines
          (DM.fromList [("index.html", (HTML, "<!Doctype html>test</html>")), ("n", (PLAINTEXT, "10"))])
          ["PUT /text HTTP/1.1", "Content-Type: text/plain", "", "wow, text!"]
          ("", "", OTHER, NONE)
          ~?= ( PUT,
                "text",
                PLAINTEXT,
                DM.fromList [("index.html", (HTML, "<!Doctype html>test</html>")), ("n", (PLAINTEXT, "10")), ("text", (PLAINTEXT, "wow, text!"))]
              ),
        iterLines
          (DM.fromList [("index.html", (HTML, "<!Doctype html>test</html>")), ("n", (PLAINTEXT, "10"))])
          ["PUT /text HTTP/1.1", "Content-Type: text/plain", "", "wow, text!"]
          ("", "", OTHER, NONE)
          ~?= ( PUT,
                "text",
                PLAINTEXT,
                DM.fromList [("index.html", (HTML, "<!Doctype html>test</html>")), ("n", (PLAINTEXT, "10")), ("text", (PLAINTEXT, "wow, text!"))]
              ),
        iterLines
          (DM.fromList [("index.html", (HTML, "<!Doctype html>test</html>")), ("n", (PLAINTEXT, "10"))])
          ["GET /bleh HTTP/1.1"]
          ("", "", OTHER, NONE)
          ~?= ( ERROR,
                "Not found",
                PLAINTEXT,
                DM.fromList [("index.html", (HTML, "<!Doctype html>test</html>")), ("n", (PLAINTEXT, "10"))]
              ),
        iterLines
          (DM.fromList [("index.html", (HTML, "<!Doctype html>test</html>")), ("n", (PLAINTEXT, "10"))])
          ["wow, this isn't a real request!"]
          ("", "", OTHER, NONE)
          ~?= ( ERROR,
                "Bad request: invalid headers",
                PLAINTEXT,
                DM.fromList [("index.html", (HTML, "<!Doctype html>test</html>")), ("n", (PLAINTEXT, "10"))]
              )
      ]

testHandleHeaderData :: Test
testHandleHeaderData =
  "testHandleHeaderData"
    ~: TestList
      [ handleHeaderData
          (DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10"))])
          ("n", "", GET, NONE)
          [""]
          ~?= (GET, "10", PLAINTEXT, DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10"))]),
        handleHeaderData
          (DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10"))])
          ("a.html", "", PUT, NONE)
          ["Content-Type: text/html", "", "<!Doctype html>", "a", "</html>"]
          ~?= ( PUT,
                "a.html",
                HTML,
                DM.fromList
                  [ ("Hello", (PLAINTEXT, "there")),
                    ("n", (PLAINTEXT, "10")),
                    ("a.html", (HTML, "<!Doctype html>a</html>"))
                  ]
              ),
        handleHeaderData
          (DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10"))])
          ("", "", OTHER, NONE)
          ["html"]
          ~?= ( ERROR,
                "Bad request: invalid headers",
                PLAINTEXT,
                DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10"))]
              ),
        handleHeaderData -- should never happen, but still testing to ensure it pops out an error
          (DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10"))])
          ("", "", ERROR, NONE)
          ["aaa"]
          ~?= (ERROR, "Bad request: invalid headers", PLAINTEXT, DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10"))])
      ]

testHandlePut :: Test
testHandlePut =
  "testHandlePut"
    ~: TestList
      [ handlePut
          "x"
          ["<!Doctype html>", "THIS IS A PAGE", "</html>"]
          HTML
          (DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10"))])
          ~?= ( PUT,
                "x",
                HTML,
                DM.fromList
                  [ ("Hello", (PLAINTEXT, "there")),
                    ("n", (PLAINTEXT, "10")),
                    ("x", (HTML, "<!Doctype html>THIS IS A PAGE</html>"))
                  ]
              ),
        handlePut
          "blah"
          ["<!DOCTYPE HTML>"]
          NONE
          (DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10"))])
          ~?= ( ERROR,
                "Bad request: no type provided",
                PLAINTEXT,
                DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10"))]
              ),
        handlePut
          "y"
          [""]
          PLAINTEXT
          (DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10"))])
          ~?= ( ERROR,
                "Bad request: no location or value provided",
                PLAINTEXT,
                DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10"))]
              ),
        handlePut
          ""
          ["blahblah"]
          PLAINTEXT
          (DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10"))])
          ~?= ( ERROR,
                "Bad request: no location or value provided",
                PLAINTEXT,
                DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10"))]
              )
      ]

testFormatResponse :: Test
testFormatResponse =
  "testFormatResponse"
    ~: TestList
      [ formatResponse (GET, "10", PLAINTEXT, DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10"))])
          ~?= ( "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n10\n",
                DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10"))]
              ),
        formatResponse
          ( PUT,
            "y",
            PLAINTEXT,
            DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10")), ("y", (PLAINTEXT, "x"))]
          )
          ~?= ( "HTTP/1.1 201 Created\r\nContent-Location: y",
                DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10")), ("y", (PLAINTEXT, "x"))]
              ),
        formatResponse (ERROR, "Not found", PLAINTEXT, DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10")), ("y", (PLAINTEXT, "x"))])
          ~?= ( "HTTP/1.1 404\r\nContent-Length: 9\r\nContent-Type: text/plain; charset=utf-8\r\n\r\nNot found\n",
                DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10")), ("y", (PLAINTEXT, "x"))]
              ),
        formatResponse (ERROR, "Bad request", PLAINTEXT, DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10")), ("y", (PLAINTEXT, "x"))])
          ~?= ( "HTTP/1.1 400\r\nContent-Length: 11\r\nContent-Type: text/plain; charset=utf-8\r\n\r\nBad request\n",
                DM.fromList [("Hello", (PLAINTEXT, "there")), ("n", (PLAINTEXT, "10")), ("y", (PLAINTEXT, "x"))]
              )
      ]

testResponsePacket :: Test
testResponsePacket =
  "testFormatResponse"
    ~: TestList
      [ responsePacket "AAA" "OK" PLAINTEXT
          ~?= "HTTP/1.1 200 OK\r\nContent-Length: 3\r\nContent-Type: text/plain; charset=utf-8\r\n\r\nAAA\n",
        responsePacket "Not found" "NF" PLAINTEXT
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
      [ responseType HTML ~?= "\r\nContent-Type: text/html; charset=utf-8\r\n\r\n",
        responseType PLAINTEXT ~?= "\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n",
        responseType NONE ~?= ""
      ]