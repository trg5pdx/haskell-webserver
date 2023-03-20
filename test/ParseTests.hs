module ParseTests
  ( testParsePacket,
    testFormatResponse,
    testResponsePacket,
  )
where

import Data.Map as DM
import Map as M
import Parse as P
import Test.HUnit

testParsePacket :: Test
testParsePacket =
  "testParsePacket"
    ~: TestList
      [ parsePacket
          ( DM.fromList
              [ ("Hello", (Plaintext, "there")),
                ("n", (Plaintext, "10"))
              ]
          )
          "GET /n HTTP/1.1"
          ~?= ( Get,
                "10",
                Plaintext,
                DM.fromList
                  [ ("Hello", (Plaintext, "there")),
                    ("n", (Plaintext, "10"))
                  ]
              ),
        parsePacket
          ( DM.fromList
              [ ("Hello", (Plaintext, "there")),
                ("n", (Plaintext, "10"))
              ]
          )
          "Hi this isn't a real request"
          ~?= ( Error,
                "Bad request: invalid headers",
                Plaintext,
                DM.fromList
                  [ ("Hello", (Plaintext, "there")),
                    ("n", (Plaintext, "10"))
                  ]
              ),
        parsePacket
          ( DM.fromList
              [ ("Hello", (Plaintext, "there")),
                ("n", (Plaintext, "10"))
              ]
          )
          "GET /testing HTTP/1.1"
          ~?= ( Error,
                "Not found",
                Plaintext,
                DM.fromList [("Hello", (Plaintext, "there")), ("n", (Plaintext, "10"))]
              )
      ]

testFormatResponse :: Test
testFormatResponse =
  "testFormatResponse"
    ~: TestList
      [ formatResponse
          ( Get,
            "10",
            Plaintext,
            DM.fromList
              [ ("Hello", (Plaintext, "there")),
                ("n", (Plaintext, "10"))
              ]
          )
          ~?= ( "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n10\n",
                DM.fromList
                  [ ("Hello", (Plaintext, "there")),
                    ("n", (Plaintext, "10"))
                  ]
              ),
        formatResponse
          ( Put,
            "y",
            Plaintext,
            DM.fromList
              [ ("Hello", (Plaintext, "there")),
                ("n", (Plaintext, "10")),
                ("y", (Plaintext, "x"))
              ]
          )
          ~?= ( "HTTP/1.1 201 Created\r\nContent-Location: y",
                DM.fromList
                  [ ("Hello", (Plaintext, "there")),
                    ("n", (Plaintext, "10")),
                    ("y", (Plaintext, "x"))
                  ]
              ),
        formatResponse
          ( Error,
            "Not found",
            Plaintext,
            DM.fromList
              [ ("Hello", (Plaintext, "there")),
                ("n", (Plaintext, "10")),
                ("y", (Plaintext, "x"))
              ]
          )
          ~?= ( "HTTP/1.1 404\r\nContent-Length: 9\r\nContent-Type: text/plain; charset=utf-8\r\n\r\nNot found\n",
                DM.fromList
                  [ ("Hello", (Plaintext, "there")),
                    ("n", (Plaintext, "10")),
                    ("y", (Plaintext, "x"))
                  ]
              ),
        formatResponse
          ( Error,
            "Bad request",
            Plaintext,
            DM.fromList
              [ ("Hello", (Plaintext, "there")),
                ("n", (Plaintext, "10")),
                ("y", (Plaintext, "x"))
              ]
          )
          ~?= ( "HTTP/1.1 400\r\nContent-Length: 11\r\nContent-Type: text/plain; charset=utf-8\r\n\r\nBad request\n",
                DM.fromList
                  [ ("Hello", (Plaintext, "there")),
                    ("n", (Plaintext, "10")),
                    ("y", (Plaintext, "x"))
                  ]
              )
      ]

testResponsePacket :: Test
testResponsePacket =
  "testFormatResponse"
    ~: TestList
      [ responsePacket "AAA" "OK" Plaintext
          ~?= "HTTP/1.1 200 OK\r\nContent-Length: 3\r\nContent-Type: text/plain; charset=utf-8\r\n\r\nAAA\n",
        responsePacket "Not found" "NF" Plaintext
          ~?= "HTTP/1.1 404\r\nContent-Length: 9\r\nContent-Type: text/plain; charset=utf-8\r\n\r\nNot found\n"
      ]
