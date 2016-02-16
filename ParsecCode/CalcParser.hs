module CalcParser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


type Name = String
type ArgNames = [Name]
type ArgVals = [Expr]

data Value = I Integer | D Double | B Bool

instance Show Value where
  show (I x) = show x
  show (D x) = show x
  show (B x) = show x
  
data Expr = Ar AExpr | Bl BExpr deriving (Show)
data BinOp = AO ABinOp | BO BBinOp | RO RBinOp deriving (Show)

-- Types representing Binary expressions and operations
data BExpr = BoolConst Bool
  | Not BExpr
  | BBinary BBinOp BExpr BExpr
  | RBinary RBinOp AExpr AExpr
    deriving (Show)
                 
data BBinOp = And | Or deriving (Show)

data RBinOp = Greater | Less deriving (Show)

-- Types representing Arithmetic expressions and operations
data AExpr = Var String
  | NumConst Value
  | Neg AExpr
  | ABinary ABinOp AExpr AExpr
    deriving (Show)

data ABinOp = Add | Sub | Mul | Div deriving (Show)

data Stmt = Stmts [Stmt]
  | Assign Name Expr
  | If BExpr Stmt Stmt
  | While BExpr Stmt
  | Func Name ArgNames Stmt
  | Exec Name ArgVals
  | Print Expr
    deriving (Show)

languageDef =
  emptyDef { Token.commentStart  = "/*"
           , Token.commentEnd    = "*/"
           , Token.commentLine   = "//"
           , Token.identStart    = letter
           , Token.identLetter   = alphaNum
           , Token.reservedNames = [ "print"
                                   , "defun"
                                   , "if"
                                   , "then"
                                   , "else"
                                   , "while"
                                   , "do"
                                   , "true"
                                   , "false"
                                   , "not"
                                   , "and"
                                   , "or" ]
           , Token.reservedOpNames = [ "+", "-", "*", "/", "=",
                                      "<", ">", "and", "or", "not" ]
           } 
                                      
lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
braces     = Token.braces     lexer
integer    = Token.integer    lexer
float      = Token.float      lexer
semi       = Token.semi       lexer
comma      = Token.comma      lexer
whiteSpace = Token.whiteSpace lexer

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement :: Parser Stmt
statement = braces statement
  <|> statements


statements =  do list <- (sepBy1 statement' semi)
                 return $ if length list == 1 then head list else Stmts list
               
statement' :: Parser Stmt
statement' = funcStmt
  <|> ifStmt
  <|> whileStmt
  <|> printStmt
  <|> try assignStmt <|> execStmt

argNames :: Parser ArgNames
argNames = do list <- (sepBy1 identifier comma)
              return list

argVals :: Parser ArgVals
argVals = do list <- (sepBy1 expression comma)
             return list              


funcStmt :: Parser Stmt
funcStmt = do reserved "defun"
              name <- identifier
              argNames <- parens argNames
              stmt <- statement
              return $ Func name argNames stmt
              
execStmt :: Parser Stmt
execStmt = do name <- identifier
              argVals <- parens argVals
              return $ Exec name argVals

printStmt :: Parser Stmt
printStmt = do reserved "print"
               arg <- parens expression
               return $ Print arg
               

ifStmt :: Parser Stmt
ifStmt = do reserved "if"
            cond <- bExpression
            reserved "then"
            stmt1 <- statement
            reserved "else"
            stmt2 <- statement
            return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt = do reserved "while"
               cond <- bExpression
               stmt <- statement
               return $ While cond stmt
               
assignStmt :: Parser Stmt
assignStmt = do var <- identifier
                reservedOp "="
                expr <- expression
                return $ Assign var expr

expression :: Parser Expr
expression = try (liftM Bl bExpression) <|> (liftM Ar aExpression)

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

aOperators = [ [Prefix (reservedOp "-" >> return (Neg             ))        ]
             , [Infix  (reservedOp "*" >> return (ABinary Mul)) AssocLeft,
                Infix  (reservedOp "/" >> return (ABinary Div  )) AssocLeft]
             , [Infix  (reservedOp "+" >> return (ABinary Add     )) AssocLeft,
                Infix  (reservedOp "-" >> return (ABinary Sub)) AssocLeft]
             ]

bOperators = [ [Prefix (reservedOp "not" >> return (Not        ))          ]
             , [Infix  (reservedOp "and" >> return (BBinary And)) AssocLeft,
                Infix  (reservedOp "or"  >> return (BBinary Or )) AssocLeft]
             ]

aTerm = parens aExpression
  <|> liftM Var identifier
  <|> try (liftM NumConst (liftM D float)) <|> (liftM NumConst (liftM I integer))

bTerm = parens bExpression
  <|> (reserved "true"  >> return (BoolConst True ))
  <|> (reserved "false" >> return (BoolConst False))
  <|> rExpression

rExpression = do a1 <- aExpression
                 op <- relation
                 a2 <- aExpression
                 return $ RBinary op a1 a2

relation = (reservedOp ">" >> return Greater)
  <|> (reservedOp "<" >> return Less)

parseString :: String -> Stmt
parseString str = case parse whileParser "" str of
  Left e -> error $ show e
  Right r -> r

parseFile :: String -> IO Stmt
parseFile file = do program <- readFile file
                    case parse whileParser "" program of
                      Left e -> print e >> fail "parse error"
                      Right r -> return r
