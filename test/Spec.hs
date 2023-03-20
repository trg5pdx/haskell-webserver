import MapTests
import ParseHelperTests
import ParseTests
import Test.HUnit

main :: IO ()
main = do
  print "testing get value..."
  _ <- runTestTT testGetValue
  print "testing set value..."
  _ <- runTestTT testSetValue
  print "testing parse header..."
  _ <- runTestTT testParseHeader
  print "testing get data type..."
  _ <- runTestTT testGetDataType
  print "testing parse packet..."
  _ <- runTestTT testParsePacket
  print "testing iter lines..."
  _ <- runTestTT testProcessPacketLines
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