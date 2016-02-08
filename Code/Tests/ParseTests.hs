import Test.HUnit
import Parser.Parsing
import Struct.Statement
import Struct.Value

testParse :: String -> Either String Stmt
testParse x = case parse pStmt x of
  [(cmd, "")] -> Right cmd
  [(_, x)] -> Left x
  _ -> Left "Nothing could be parsed"



arithmeticTests = TestList $ map TestCase [

  -- Single Value Integer tests
  (assertEqual "Simple single digit integer parse"
   (Right (AEval (Val (I 9))) :: Either String Stmt)
   (testParse "9")),
  (assertEqual "Simple multi digit integer parse"
   (Right (AEval (Val (I 999))) :: Either String Stmt)
   (testParse "999")),

  -- Singlue Value Float tests
  (assertEqual "Simple single digit floating point parse"
   (Right (AEval (Val (F 9.9))) :: Either String Stmt)
    (testParse "9.9")),
  (assertEqual "Simple multiple digit floating point parse A"
   (Right (AEval (Val (F 99.0))) :: Either String Stmt)
    (testParse "99.0")),
  (assertEqual "Simple multiple digit floating point parse B"
   (Right (AEval (Val (F 9.99))) :: Either String Stmt)
    (testParse "9.99")),
  (assertEqual "Simple multiple digit floating point parse C"
   (Right (AEval (Val (F 999.999))) :: Either String Stmt)
    (testParse "999.999")),
  
  
  
  -- Simple Integer Arithmetic tests
  (assertEqual "Simple integer addition parse"
   (Right (AEval (Add (Val (I 2)) (Val (I 3)))) :: Either String Stmt)
   (testParse "2+3")),
  (assertEqual "Simple integer subtraction parse"
   (Right (AEval (Sub (Val (I 3)) (Val (I 2)))) :: Either String Stmt)
   (testParse "3-2")),
  (assertEqual "Simple integer multiplication parse"
   (Right (AEval (Mul (Val (I 4)) (Val (I 2)))) :: Either String Stmt)
   (testParse "4*2")),
    (assertEqual "Simple integer division parse"
   (Right (AEval (Div (Val (I 8)) (Val (I 2)))) :: Either String Stmt)
   (testParse "8/2")),

    -- Simple Float Arithmetic tests 
    (assertEqual "Simple float addition parse"
   (Right (AEval (Add (Val (F 2.0)) (Val (F 3.3)))) :: Either String Stmt)
   (testParse "2.0+3.3")),
  (assertEqual "Simple float subtraction parse"
   (Right (AEval (Sub (Val (F 3.3)) (Val (F 2.2)))) :: Either String Stmt)
   (testParse "3.3-2.2")),
  (assertEqual "Simple float multiplication parse"
   (Right (AEval (Mul (Val (F 4.3)) (Val (F 2.0)))) :: Either String Stmt)
   (testParse "4.3*2.0")),
    (assertEqual "Simple integer division parse"
   (Right (AEval (Div (Val (F 8.8)) (Val (F 4.4)))) :: Either String Stmt)
   (testParse "8.8/4.4"))
    
  ]

arithmeticPrecedenceTests = TestList $ map TestCase [
  -- Tests of multiplication over addition precedence 
  (assertEqual "Multiplication over addition A"
   (Right (AEval (Add (Val (I 3)) (Mul (Val (I 6)) (Val (I 7))))) :: Either String Stmt)
   (testParse "3+6*7")),
  (assertEqual "Multiplication over addition B"
   (Right (AEval (Add  (Mul (Val (I 3)) (Val (I 6))) (Val (I 7)))) :: Either String Stmt)
   (testParse "3*6+7")),

    -- Tests of multiplication over subtraction precedence 
    (assertEqual "Multiplication over addition A"
   (Right (AEval (Sub (Val (I 3)) (Mul (Val (I 6)) (Val (I 7))))) :: Either String Stmt)
   (testParse "3-6*7")),
  (assertEqual "Multiplication over addition B"
   (Right (AEval (Sub  (Mul (Val (I 3)) (Val (I 6))) (Val (I 7)))) :: Either String Stmt)
   (testParse "3*6-7"))
  ]  

  

parseTests = TestList [arithmeticTests, arithmeticPrecedenceTests]
