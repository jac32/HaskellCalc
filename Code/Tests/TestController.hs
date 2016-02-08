import Test.HUnit
import Tests.BSTTests
import Tests.ValueTests
import Tests.ParseTests

bstTests = TestList [insertTests, retrievalTests, removalTests, smallestNodeTests]

valueTests = TestList [additionValueTests, subtractionValueTests, multiplicationValueTests, divisionValueTests, notValueTests, andValueTests, orValueTests]

parseTests = TestList [arithmeticTests, additionTests, subtractionTests]


allTests = TestList [bstTests, valueTests, parseTests]
