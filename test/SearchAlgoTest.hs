import SearchAlgorithms
import qualified Data.Text as T

query0 = T.pack "hej dig"
query1 = T.pack "iron pact"
query2 = T.pack "elf"


subject0 = T.pack "hej med dig din store sut"
subject1 = T.pack "iron pact og lindorme. IrOn PaCt'S"
subject2 = T.pack "self hej dig"

testPartial :: IO()
testPartial = do
    print subject0
    print (partialSearch query0 subject0)
    print (partialSearch query1 subject0)
    print (partialSearch query2 subject0)
    print ""
    print subject1
    print (partialSearch query0 subject1)
    print (partialSearch query1 subject1)
    print (partialSearch query2 subject1)
    print ""
    print subject2
    print (partialSearch query0 subject2)
    print (partialSearch query1 subject2)
    print (partialSearch query2 subject2)

testComplete :: IO()
testComplete = do
    print ""
    print subject0
    print (completeSearch query0 subject0)
    print (completeSearch query1 subject0)
    print (completeSearch query2 subject0)
    print ""
    print subject1
    print (completeSearch query0 subject1)
    print (completeSearch query1 subject1)
    print (completeSearch query2 subject1)
    print ""
    print subject2
    print (completeSearch query0 subject2)
    print (completeSearch query1 subject2)
    print (completeSearch query2 subject2)

main :: IO ()
main = do
    testPartial
    testComplete    