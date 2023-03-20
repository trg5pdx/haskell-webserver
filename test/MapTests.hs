module MapTests
  ( testGetValue,
    testSetValue,
  )
where

import Data.Map as DM
import Map as M
import Test.HUnit

testGetValue :: Test
testGetValue =
  "testGetValue"
    ~: TestList
      [ getValue
          ( DM.fromList
              [ ("Hello", (Plaintext, "there")),
                ("y", (Plaintext, "this is a test"))
              ]
          )
          "Hello"
          ~?= ( Get,
                "there",
                Plaintext,
                DM.fromList
                  [ ("Hello", (Plaintext, "there")),
                    ("y", (Plaintext, "this is a test"))
                  ]
              ),
        getValue
          ( DM.fromList
              [ ("Hello", (Plaintext, "there")),
                ("y", (Plaintext, "this is a test"))
              ]
          )
          "nope"
          ~?= ( Error,
                "Not found",
                Plaintext,
                DM.fromList
                  [ ("Hello", (Plaintext, "there")),
                    ("y", (Plaintext, "this is a test"))
                  ]
              )
      ]

testSetValue :: Test
testSetValue =
  "testGetValue"
    ~: TestList
      [ setValue
          ( DM.fromList
              [ ("Hello", (Plaintext, "there")),
                ("y", (Plaintext, "this is a test"))
              ]
          )
          "Hi"
          "AAA"
          Plaintext
          ~?= ( Put,
                "Hi",
                Plaintext,
                DM.fromList
                  [ ("Hello", (Plaintext, "there")),
                    ("y", (Plaintext, "this is a test")),
                    ("Hi", (Plaintext, "AAA"))
                  ]
              )
      ]