import Test.HUnit
import Tests.BSTTests
import Tests.ValueTests
import Tests.ParseTests

bstTests = TestList [insertTests, retrievalTests, removalTests, smallestNodeTests]

valueTests = TestList [additionValueTests, subtractionValueTests, multiplicationValueTests, 
                       eqValueTests,gtValueTests,ltValueTests,divisionValueTests, notValueTests, andValueTests, orValueTests,
                       factValueTests, modValueTests, sqrtValueTests]

parseTests = TestList [arithmeticTests, additionTests, subtractionTests]


allTests = TestList [bstTests, valueTests, parseTests]
