module Expr where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Lexer

---------------------------------------------------------------------------------------------------
-- Generic Expression
---------------------------------------------------------------------------------------------------
data Expr = ExprNat Integer | ExprInt Integer | ExprReal Double | ExprBool Bool |
			ExprID String | ExprFunc String [Expr] |
			ExprUnOp UnOp Expr | ExprBinOp BinOp Expr Expr

instance Show Expr where
	show (ExprNat n) = show n
	show (ExprInt n) = show n
	show (ExprReal n) = show n
	show (ExprBool n) = show n
	show (ExprID s) = show s
	show (ExprFunc s e) = show s ++ show e
	show (ExprUnOp op e) = "(" ++ show op ++ show e ++ ")"
	show (ExprBinOp op e1 e2) = "(" ++ show e1 ++ show op ++ show e2 ++ ")"

---------------------------------------------------------------------------------------------------
-- Unary Operators
---------------------------------------------------------------------------------------------------
data UnOp = Minus | Not
instance Show UnOp where
	show (Not) = "not"
	show (Minus) = "-"

---------------------------------------------------------------------------------------------------
-- Binary Operators
---------------------------------------------------------------------------------------------------
data BinOp = Add | Sub | Mul | Div | And | Or | Equals | Differs | Lt | Gt | Lte | Gte
instance Show BinOp where
	show (Add) = "+"
	show (Sub) = "-"
	show (Mul) = "*"
	show (Div) = "/"
	show (And) = "and"
	show (Or) = "or"
	show (Equals) = "=="
	show (Differs) = "!="
	show (Lt) = "<"
	show (Gt) = ">"
	show (Lte) = "<="
	show (Gte) = ">="

---------------------------------------------------------------------------------------------------
-- Grammar
---------------------------------------------------------------------------------------------------

parseExpr :: Parser Expr
parseExpr = parseOrChain

parseExprLeaf :: Parser Expr
parseExprLeaf = (try parseUnary)
	<|> (try parseLiteral)
	<|> (try parseFuncCall)
	<|> (try parseID)
	<|> (try (parens parseExpr))

---------------------------------------------------------------------------------------------------
-- Grammar - Leaf Terms
---------------------------------------------------------------------------------------------------

parseID :: Parser Expr
parseID = do
	id <- identifier
	return $ ExprID id

parseFuncCall :: Parser Expr
parseFuncCall = do
	id <- identifier
	args <- parseArguments
	return $ ExprFunc id args

parseArguments :: Parser [Expr]
parseArguments = parens (sepBy parseExpr comma)

parseLiteral :: Parser Expr
parseLiteral = parseNumber <|> parseBool

---------------------------------------------------------------------------------------------------
-- Grammar - Numeric Leaf Terms
---------------------------------------------------------------------------------------------------

parseNumber :: Parser Expr
parseNumber = parseNatural <|> parseInteger <|> parseReal

parseNatural :: Parser Expr
parseNatural = do
	n <- natural
	return $ ExprInt n

parseInteger :: Parser Expr
parseInteger = do
	n <- integer
	return $ ExprNat n

parseReal :: Parser Expr
parseReal = do
	n <- float
	return $ ExprReal n

---------------------------------------------------------------------------------------------------
-- Grammar - Boolean Leaf Terms
---------------------------------------------------------------------------------------------------

parseBool :: Parser Expr
parseBool = parseTrue <|> parseFalse

parseTrue :: Parser Expr
parseTrue = do
	reserved "true"
	return $ ExprBool True

parseFalse:: Parser Expr
parseFalse = do
	reserved "false"
	return $ ExprBool False

---------------------------------------------------------------------------------------------------
-- Gramar - Unary Operators
---------------------------------------------------------------------------------------------------

parseUnary :: Parser Expr
parseUnary = parseMinus <|> parseNot

parseMinus :: Parser Expr
parseMinus = do
	op <- try parseMinusOp
	e <- try parseExprLeaf
	return $ ExprUnOp op e

parseMinusOp :: Parser UnOp
parseMinusOp = do
	reservedOp "-"
	return $ Minus

parseNot :: Parser Expr
parseNot = do
	op <- try parseNotOp
	e <- try parseExprLeaf
	return $ ExprUnOp op e

parseNotOp :: Parser UnOp
parseNotOp = do
	reserved "not"
	return $ Not

---------------------------------------------------------------------------------------------------
-- Gramar - Logic Binary Operators
---------------------------------------------------------------------------------------------------

parseOrChain :: Parser Expr
parseOrChain = (try parseOrChainTail) <|> (try parseAndChain)

parseOrChainTail :: Parser Expr
parseOrChainTail = do
	e1 <- parseAndChain
	op <- parseOrOp
	e2 <- parseOrChain
	return $ ExprBinOp op e1 e2

parseOrOp :: Parser BinOp
parseOrOp = do
	reserved "or"
	return $ Or

parseAndChain :: Parser Expr
parseAndChain = (try parseAndChainTail) <|> (try parseEqChain)

parseAndChainTail :: Parser Expr
parseAndChainTail = do
	e1 <- parseEqChain
	op <- parseAndOp
	e2 <- parseAndChain
	return $ ExprBinOp op e1 e2

parseAndOp :: Parser BinOp
parseAndOp = do
	reserved "and"
	return $ And

---------------------------------------------------------------------------------------------------
-- Gramar - Relational Binary Operators
---------------------------------------------------------------------------------------------------

parseEqChain :: Parser Expr
parseEqChain = (try parseEqChainTail) <|> (try parseRelational)

parseEqChainTail :: Parser Expr
parseEqChainTail = do
	e1 <- parseRelational
	op <- parseEqualsOp <|> parseDiffersOp
	e2 <- parseEqChain
	return $ ExprBinOp op e1 e2

parseEqualsOp :: Parser BinOp
parseEqualsOp = do
	reservedOp "=="
	return $ Equals

parseDiffersOp :: Parser BinOp
parseDiffersOp = do
	reservedOp "!="
	return $ Differs

parseRelational :: Parser Expr
parseRelational = (try parseRelationalTail) <|> (try parseAddChain)

parseRelationalTail :: Parser Expr
parseRelationalTail = do
	e1 <- parseAddChain
	op <- parseLtOp <|> parseGtOp <|> parseLteOp <|> parseGteOp
	e2 <- parseAddChain
	return $ ExprBinOp op e1 e2

parseLtOp :: Parser BinOp
parseLtOp = do
	reservedOp "<"
	return $ Lt

parseGtOp :: Parser BinOp
parseGtOp = do
	reservedOp ">"
	return $ Gt

parseLteOp :: Parser BinOp
parseLteOp = do
	reservedOp "<="
	return $ Lte

parseGteOp :: Parser BinOp
parseGteOp = do
	reservedOp ">="
	return $ Gte

---------------------------------------------------------------------------------------------------
-- Gramar - Numeric Binary Operators
---------------------------------------------------------------------------------------------------

parseAddChain :: Parser Expr
parseAddChain = (try parseAddChainTail) <|> (try parseMulChain)

parseAddChainTail :: Parser Expr
parseAddChainTail = do
	e1 <- parseMulChain
	op <- parseAddOp <|> parseSubOp
	e2 <- parseAddChain
	return $ ExprBinOp op e1 e2

parseAddOp :: Parser BinOp
parseAddOp = do
	reservedOp "+"
	return $ Add

parseSubOp :: Parser BinOp
parseSubOp = do
	reservedOp "-"
	return $ Sub

parseMulChain :: Parser Expr
parseMulChain = (try parseMulChainTail) <|> (try parseExprLeaf)

parseMulChainTail :: Parser Expr
parseMulChainTail = do
	e1 <- parseExprLeaf
	op <- parseMulOp <|> parseDivOp
	e2 <- parseMulChain
	return $ ExprBinOp op e1 e2

parseMulOp :: Parser BinOp
parseMulOp = do
	reservedOp "*"
	return $ Mul

parseDivOp :: Parser BinOp
parseDivOp = do
	reservedOp "/"
	return $ Div

