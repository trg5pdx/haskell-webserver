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
      [ getValue (DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("y", (M.PLAINTEXT, "this is a test"))]) "Hello"
          ~?= Get
            ( "there",
              M.PLAINTEXT,
              DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("y", (M.PLAINTEXT, "this is a test"))]
            ),
        getValue (DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("y", (M.PLAINTEXT, "this is a test"))]) "nope"
          ~?= Error
            ( "Not found",
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

testParseHeader :: Test
testParseHeader =
  "testParseHeader"
    ~: TestList
      [ parseHeader ["GET", "/test", "HTTP/1.1"] ("", "", P.OTHER, M.NONE) ~?= ("test", "", P.GET, M.NONE),
        parseHeader ["PUT", "/i.html", "HTTP/1.1"] ("", "", P.OTHER, M.NONE) ~?= ("i.html", "", P.PUT, M.NONE),
        parseHeader ["Content-Type:", "text/plain"] ("a", "", P.PUT, M.NONE) ~?= ("a", "", P.PUT, M.PLAINTEXT),
        parseHeader ["Content-Type:", "text/html"] ("b", "", P.PUT, M.NONE) ~?= ("b", "", P.PUT, M.HTML),
        parseHeader ["Content-Type:", "text/"] ("c", "", P.PUT, M.NONE) ~?= ("", " invalid content type", P.ERROR, M.NONE),
        parseHeader ["not real"] ("", "", P.OTHER, M.NONE) ~?= ("", "", P.OTHER, M.NONE)
      ]

testGetType :: Test
testGetType =
  "testGetType"
    ~: TestList
      [ getType ("", "", P.PUT, M.NONE) "text/html" ~?= ("", "", P.PUT, M.HTML),
        getType ("", "", P.PUT, M.NONE) "text/plain" ~?= ("", "", P.PUT, M.PLAINTEXT),
        getType ("", "", P.PUT, M.NONE) "blahblahblah" ~?= ("", " invalid content type", P.ERROR, M.NONE)
      ]

testParsePacket :: Test
testParsePacket =
  "testParsePacket"
    ~: TestList
      [ parsePacket (DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))]) "GET /n HTTP/1.1"
          ~?= Get ("10", M.PLAINTEXT, DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))]),
        parsePacket (DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))]) "Hi this isn't a real request"
          ~?= Error ("Bad request: invalid headers", M.PLAINTEXT, DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))]),
        parsePacket (DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))]) "GET /testing HTTP/1.1"
          ~?= Error ("Not found", M.PLAINTEXT, DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))])
      ]

testIterLines :: Test
testIterLines =
  "testIterLines"
    ~: TestList
      [ iterLines
          (DM.fromList [("index.html", (M.HTML, "<!Doctype html><h1>An html page</h1></html>")), ("n", (M.PLAINTEXT, "10"))])
          ["GET /index.html HTTP/1.1"]
          ("", "", P.OTHER, M.NONE)
          ~?= Get
            ( "<!Doctype html><h1>An html page</h1></html>",
              M.HTML,
              DM.fromList [("index.html", (M.HTML, "<!Doctype html><h1>An html page</h1></html>")), ("n", (M.PLAINTEXT, "10"))]
            ),
        iterLines
          (DM.fromList [("index.html", (M.HTML, "<!Doctype html>test</html>")), ("n", (M.PLAINTEXT, "10"))])
          ["PUT /text HTTP/1.1", "Content-Type: text/plain", "", "wow, text!"]
          ("", "", P.OTHER, M.NONE)
          ~?= Put
            ( "text",
              M.PLAINTEXT,
              DM.fromList [("index.html", (M.HTML, "<!Doctype html>test</html>")), ("n", (M.PLAINTEXT, "10")), ("text", (M.PLAINTEXT, "wow, text!"))]
            ),
        iterLines
          (DM.fromList [("index.html", (M.HTML, "<!Doctype html>test</html>")), ("n", (M.PLAINTEXT, "10"))])
          ["PUT /text HTTP/1.1", "Content-Type: text/plain", "", "wow, text!"]
          ("", "", P.OTHER, M.NONE)
          ~?= Put
            ( "text",
              M.PLAINTEXT,
              DM.fromList [("index.html", (M.HTML, "<!Doctype html>test</html>")), ("n", (M.PLAINTEXT, "10")), ("text", (M.PLAINTEXT, "wow, text!"))]
            ),
        iterLines
          (DM.fromList [("index.html", (M.HTML, "<!Doctype html>test</html>")), ("n", (M.PLAINTEXT, "10"))])
          ["GET /bleh HTTP/1.1"]
          ("", "", P.OTHER, M.NONE)
          ~?= Error
            ( "Not found",
              M.PLAINTEXT,
              DM.fromList [("index.html", (M.HTML, "<!Doctype html>test</html>")), ("n", (M.PLAINTEXT, "10"))]
            ),
        iterLines
          (DM.fromList [("index.html", (M.HTML, "<!Doctype html>test</html>")), ("n", (M.PLAINTEXT, "10"))])
          ["wow, this isn't a real request!"]
          ("", "", P.OTHER, M.NONE)
          ~?= Error
            ( "Bad request: invalid headers",
              M.PLAINTEXT,
              DM.fromList [("index.html", (M.HTML, "<!Doctype html>test</html>")), ("n", (M.PLAINTEXT, "10"))]
            )
      ]

testHandleHeaderData :: Test
testHandleHeaderData =
  "testHandleHeaderData"
    ~: TestList
      [ handleHeaderData
          (DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))])
          ("n", "", P.GET, M.NONE)
          [""]
          ~?= Get ("10", M.PLAINTEXT, DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))]),
        handleHeaderData
          (DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))])
          ("a.html", "", P.PUT, M.NONE)
          ["Content-Type: text/html", "", "<!Doctype html>", "a", "</html>"]
          ~?= Put
            ( "a.html",
              M.HTML,
              DM.fromList
                [ ("Hello", (M.PLAINTEXT, "there")),
                  ("n", (M.PLAINTEXT, "10")),
                  ("a.html", (M.HTML, "<!Doctype html>a</html>"))
                ]
            ),
        handleHeaderData
          (DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))])
          ("", "", P.OTHER, M.NONE)
          ["html"]
          ~?= Error ("Bad request: invalid headers", M.PLAINTEXT, DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))]),
        handleHeaderData -- should never happen, but still testing to ensure it pops out an error
          (DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))])
          ("", "", P.ERROR, M.NONE)
          ["aaa"]
          ~?= Error ("Bad request: invalid headers", M.PLAINTEXT, DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))])
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
            ( "Bad request: no type provided",
              M.PLAINTEXT,
              DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))]
            ),
        handlePut
          "y"
          [""]
          M.PLAINTEXT
          (DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))])
          ~?= Error
            ( "Bad request: no location or value provided",
              M.PLAINTEXT,
              DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))]
            ),
        handlePut
          ""
          ["blahblah"]
          M.PLAINTEXT
          (DM.fromList [("Hello", (M.PLAINTEXT, "there")), ("n", (M.PLAINTEXT, "10"))])
          ~?= Error
            ( "Bad request: no location or value provided",
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