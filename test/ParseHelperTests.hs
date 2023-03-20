module ParseHelperTests
  ( testParseHeader,
    testProcessPacketLines,
    testHandleHeaderData,
    testHandlePut,
    testGetDataType,
    testHttpCode,
    testResponseType,
  )
where

import Data.Map as DM
import Map as M
import Parse as P
import Test.HUnit

testParseHeader :: Test
testParseHeader =
  "testParseHeader"
    ~: TestList
      [ parseHeader ["GET", "/test", "HTTP/1.1"] ("", "", Other, None)
          ~?= ("test", "", Get, None),
        parseHeader ["PUT", "/i.html", "HTTP/1.1"] ("", "", Other, None)
          ~?= ("i.html", "", Put, None),
        parseHeader ["Content-Type:", "text/plain"] ("a", "", Put, None)
          ~?= ("a", "", Put, Plaintext),
        parseHeader ["Content-Type:", "text/html"] ("b", "", Put, None)
          ~?= ("b", "", Put, Html),
        parseHeader ["Content-Type:", "text/"] ("c", "", Put, None)
          ~?= ("", " invalid content type", Error, None),
        parseHeader ["not real"] ("", "", Other, None)
          ~?= ("", "", Other, None)
      ]

testProcessPacketLines :: Test
testProcessPacketLines =
  "testProcessPacketLines"
    ~: TestList
      [ processPacketLines
          ( DM.fromList
              [ ("index.html", (Html, "<!Doctype html><h1>An html page</h1></html>")),
                ("n", (Plaintext, "10"))
              ]
          )
          ["GET /index.html HTTP/1.1"]
          ("", "", Other, None)
          ~?= ( Get,
                "<!Doctype html><h1>An html page</h1></html>",
                Html,
                DM.fromList
                  [ ("index.html", (Html, "<!Doctype html><h1>An html page</h1></html>")),
                    ("n", (Plaintext, "10"))
                  ]
              ),
        processPacketLines
          ( DM.fromList
              [ ("index.html", (Html, "<!Doctype html>test</html>")),
                ("n", (Plaintext, "10"))
              ]
          )
          ["PUT /text HTTP/1.1", "Content-Type: text/plain", "", "wow, text!"]
          ("", "", Other, None)
          ~?= ( Put,
                "text",
                Plaintext,
                DM.fromList
                  [ ("index.html", (Html, "<!Doctype html>test</html>")),
                    ("n", (Plaintext, "10")),
                    ("text", (Plaintext, "wow, text!"))
                  ]
              ),
        processPacketLines
          ( DM.fromList
              [ ("index.html", (Html, "<!Doctype html>test</html>")),
                ("n", (Plaintext, "10"))
              ]
          )
          ["PUT /text HTTP/1.1", "Content-Type: text/plain", "", "wow, text!"]
          ("", "", Other, None)
          ~?= ( Put,
                "text",
                Plaintext,
                DM.fromList
                  [ ("index.html", (Html, "<!Doctype html>test</html>")),
                    ("n", (Plaintext, "10")),
                    ("text", (Plaintext, "wow, text!"))
                  ]
              ),
        processPacketLines
          ( DM.fromList
              [ ("index.html", (Html, "<!Doctype html>test</html>")),
                ("n", (Plaintext, "10"))
              ]
          )
          ["GET /bleh HTTP/1.1"]
          ("", "", Other, None)
          ~?= ( Error,
                "Not found",
                Plaintext,
                DM.fromList
                  [ ("index.html", (Html, "<!Doctype html>test</html>")),
                    ("n", (Plaintext, "10"))
                  ]
              ),
        processPacketLines
          ( DM.fromList
              [ ("index.html", (Html, "<!Doctype html>test</html>")),
                ("n", (Plaintext, "10"))
              ]
          )
          ["wow, this isn't a real request!"]
          ("", "", Other, None)
          ~?= ( Error,
                "Bad request: invalid headers",
                Plaintext,
                DM.fromList
                  [ ("index.html", (Html, "<!Doctype html>test</html>")),
                    ("n", (Plaintext, "10"))
                  ]
              )
      ]

testHandleHeaderData :: Test
testHandleHeaderData =
  "testHandleHeaderData"
    ~: TestList
      [ handleHeaderData
          ( DM.fromList
              [ ("Hello", (Plaintext, "there")),
                ("n", (Plaintext, "10"))
              ]
          )
          ("n", "", Get, None)
          [""]
          ~?= ( Get,
                "10",
                Plaintext,
                DM.fromList
                  [ ("Hello", (Plaintext, "there")),
                    ("n", (Plaintext, "10"))
                  ]
              ),
        handleHeaderData
          ( DM.fromList
              [ ("Hello", (Plaintext, "there")),
                ("n", (Plaintext, "10"))
              ]
          )
          ("a.html", "", Put, None)
          ["Content-Type: text/html", "", "<!Doctype html>", "a", "</html>"]
          ~?= ( Put,
                "a.html",
                Html,
                DM.fromList
                  [ ("Hello", (Plaintext, "there")),
                    ("n", (Plaintext, "10")),
                    ("a.html", (Html, "<!Doctype html>a</html>"))
                  ]
              ),
        handleHeaderData
          ( DM.fromList
              [ ("Hello", (Plaintext, "there")),
                ("n", (Plaintext, "10"))
              ]
          )
          ("", "", Other, None)
          ["html"]
          ~?= ( Error,
                "Bad request: invalid headers",
                Plaintext,
                DM.fromList
                  [ ("Hello", (Plaintext, "there")),
                    ("n", (Plaintext, "10"))
                  ]
              ),
        -- should never happen, but still testing to ensure it pops out an error
        handleHeaderData
          ( DM.fromList
              [ ("Hello", (Plaintext, "there")),
                ("n", (Plaintext, "10"))
              ]
          )
          ("", "", Error, None)
          ["aaa"]
          ~?= ( Error,
                "Bad request: invalid headers",
                Plaintext,
                DM.fromList
                  [ ("Hello", (Plaintext, "there")),
                    ("n", (Plaintext, "10"))
                  ]
              )
      ]

testHandlePut :: Test
testHandlePut =
  "testHandlePut"
    ~: TestList
      [ handlePut
          "x"
          ["<!Doctype html>", "THIS IS A PAGE", "</html>"]
          Html
          ( DM.fromList
              [ ("Hello", (Plaintext, "there")),
                ("n", (Plaintext, "10"))
              ]
          )
          ~?= ( Put,
                "x",
                Html,
                DM.fromList
                  [ ("Hello", (Plaintext, "there")),
                    ("n", (Plaintext, "10")),
                    ("x", (Html, "<!Doctype html>THIS IS A PAGE</html>"))
                  ]
              ),
        handlePut
          "blah"
          ["<!Doctype Html>"]
          None
          ( DM.fromList
              [ ("Hello", (Plaintext, "there")),
                ("n", (Plaintext, "10"))
              ]
          )
          ~?= ( Error,
                "Bad request: no type provided",
                Plaintext,
                DM.fromList
                  [ ("Hello", (Plaintext, "there")),
                    ("n", (Plaintext, "10"))
                  ]
              ),
        handlePut
          "y"
          [""]
          Plaintext
          ( DM.fromList
              [ ("Hello", (Plaintext, "there")),
                ("n", (Plaintext, "10"))
              ]
          )
          ~?= ( Error,
                "Bad request: no location or value provided",
                Plaintext,
                DM.fromList
                  [ ("Hello", (Plaintext, "there")),
                    ("n", (Plaintext, "10"))
                  ]
              ),
        handlePut
          ""
          ["blahblah"]
          Plaintext
          ( DM.fromList
              [ ("Hello", (Plaintext, "there")),
                ("n", (Plaintext, "10"))
              ]
          )
          ~?= ( Error,
                "Bad request: no location or value provided",
                Plaintext,
                DM.fromList
                  [ ("Hello", (Plaintext, "there")),
                    ("n", (Plaintext, "10"))
                  ]
              )
      ]

testGetDataType :: Test
testGetDataType =
  "testGetType"
    ~: TestList
      [ getDataType ("", "", Put, None) "text/html"
          ~?= ("", "", Put, Html),
        getDataType ("", "", Put, None) "text/plain"
          ~?= ("", "", Put, Plaintext),
        getDataType ("", "", Put, None) "blahblahblah"
          ~?= ("", " invalid content type", Error, None)
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
      [ responseType Html
          ~?= "\r\nContent-Type: text/html; charset=utf-8\r\n\r\n",
        responseType Plaintext
          ~?= "\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n",
        responseType None
          ~?= ""
      ]