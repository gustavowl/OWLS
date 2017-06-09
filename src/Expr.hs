module Expr where

import Text.ParserCombinators.Parsec
import Text.Parsec.Combinator
import Lexer
import ProgramTree
import qualified Tokens

---------------------------------------------------------------------------------------------------
-- General Expr
---------------------------------------------------------------------------------------------------

parseExpr :: OWLParser Expr
parseExpr = parseBoolExpr

parseExprLeaf :: OWLParser Expr
parseExprLeaf = (try parseFuncCall)
	<|> (try parseReadCall)
	<|> (try parseArrayEl)
	<|> (try parsePtr)
	<|> (try parseField)
	<|> (try parseID)
	<|> (parens parseExpr)

---------------------------------------------------------------------------------------------------
-- General Leaf
---------------------------------------------------------------------------------------------------

parseID :: OWLParser Expr
parseID = do
	name <- identifier
	return $ ID name

parseFuncCall :: OWLParser Expr
parseFuncCall = do
	func <- identifier
	args <- parens $ sepBy parseExpr comma
	return $ FuncCall func args

parseReadCall :: OWLParser Expr
parseReadCall = do
	readToken
	lparen
	rparen
	return $ ReadCall

parseArrayEl :: OWLParser Expr
parseArrayEl = do
	array <- parseID <|> (parens parseExpr)
	size <- brackets parseExpr
	return $ ArrayEl array size

parseField :: OWLParser Expr
parseField = do
	struct <- parseID <|> (parens parseExpr)
	dotToken
	field <- identifier
	return $ Field struct field

parsePtr :: OWLParser Expr
parsePtr = do
	atToken
	ptr <- parseID <|> (parens parseExpr)
	return $ Ptr ptr

---------------------------------------------------------------------------------------------------
-- General Literals
---------------------------------------------------------------------------------------------------

parseChar :: OWLParser Expr
parseChar = do
	s <- cchar
	return $ CharLit s

parseStuffArray :: OWLParser Expr
parseStuffArray = do
	exprs <- braces (sepBy parseExpr comma)
	return $ ArrayLit exprs

parseStuffString :: OWLParser Expr
parseStuffString = do
	s <- sstring
	return $ ArrayLit $ stringToArray s

stringToArray :: String -> [Expr]  
stringToArray [] = []  
stringToArray (x:xs) = (CharLit x) : stringToArray xs

---------------------------------------------------------------------------------------------------
-- Boolean Expr
---------------------------------------------------------------------------------------------------

parseBoolExpr :: OWLParser Expr
parseBoolExpr = parseOrChain

parseBoolLeaf :: OWLParser Expr
parseBoolLeaf = (try parseBoolUnary)
	<|> (try parseBoolean)
	<|> parseExprLeaf

---------------------------------------------------------------------------------------------------
-- Boolean Literal
---------------------------------------------------------------------------------------------------

parseBoolean :: OWLParser Expr
parseBoolean = do
	b <- boolean
	return $ BoolLit b

---------------------------------------------------------------------------------------------------
-- Boolean Unary Operators
---------------------------------------------------------------------------------------------------

parseBoolUnary :: OWLParser Expr
parseBoolUnary = parseNeg

parseNeg :: OWLParser Expr
parseNeg = do
	notToken
	n <- parseBoolLeaf
	return $ BoolNot n

---------------------------------------------------------------------------------------------------
-- Boolean Binary Operators
---------------------------------------------------------------------------------------------------

parseOrChain :: OWLParser Expr
parseOrChain = (try parseOrChainTail) <|> (try parseAndChain)

parseOrChainTail :: OWLParser Expr
parseOrChainTail = do
	e1 <- parseAndChain
	op <- orToken <|> corToken
	e2 <- parseOrChain
	if op == Tokens.Or then
		return $ BoolOr e1 e1
	else
		return $ BoolOrC e1 e2

parseAndChain :: OWLParser Expr
parseAndChain = (try parseAndChainTail) <|> (try parseRelational)

parseAndChainTail :: OWLParser Expr
parseAndChainTail = do
	e1 <- parseRelational
	op <- andToken <|> candToken
	e2 <- parseAndChain
	if op == Tokens.And then
		return $ BoolAnd e1 e1
	else
		return $ BoolAndC e1 e2

---------------------------------------------------------------------------------------------------
-- Boolean Relational Binary Operators
---------------------------------------------------------------------------------------------------

parseRelational :: OWLParser Expr
parseRelational = (try parseRelationalTail) <|> parseNumExpr <|> parseBoolLeaf

parseRelationalTail :: OWLParser Expr
parseRelationalTail = do
	e1 <- parseNumExpr
	op <- greaterToken <|> greaterEqToken <|> lessToken <|> lessEqToken <|> eqToken <|> difToken
	e2 <- parseNumExpr
	if op == Tokens.Equals then
		return $ BoolEq e1 e2
	else if op == Tokens.Not_Equals then
		return $ BoolDif e1 e2
	else if op == Tokens.Greater then
		return $ BoolGt e1 e2
	else if op == Tokens.Less then
		return $ BoolLt e1 e2
	else if op == Tokens.GreaterEq then
		return $ BoolGtEq e1 e2
	else
		return $ BoolLtEq e1 e2

---------------------------------------------------------------------------------------------------
-- Numeric Expr
---------------------------------------------------------------------------------------------------

parseNumExpr :: OWLParser Expr
parseNumExpr = parseAddChain

parseNumLeaf :: OWLParser Expr
parseNumLeaf = (try parseNumUnary)
	<|> (try parseNumber)
	<|> parseExprLeaf

---------------------------------------------------------------------------------------------------
-- Numeric Literal Leaf Terms
---------------------------------------------------------------------------------------------------

parseNumber :: OWLParser Expr
parseNumber = parseNatural <|> parseInteger <|> parseReal

parseNatural :: OWLParser Expr
parseNatural = do
	n <- natural
	return $ NatLit n

parseInteger :: OWLParser Expr
parseInteger = do
	n <- integer
	return $ IntLit n

parseReal :: OWLParser Expr
parseReal = do
	n <- real
	return $ RealLit n

---------------------------------------------------------------------------------------------------
-- Numeric Unary Operators
---------------------------------------------------------------------------------------------------

parseNumUnary :: OWLParser Expr
parseNumUnary = parseMinus

parseMinus :: OWLParser Expr
parseMinus = do
	op <- minusToken
	expr <- parseNumLeaf
	return $ NumMinus expr

---------------------------------------------------------------------------------------------------
-- Numeric Binary Operators
---------------------------------------------------------------------------------------------------

parseAddChain :: OWLParser Expr
parseAddChain = (try parseAddChainTail) <|> (try parseMulChain)

parseAddChainTail :: OWLParser Expr
parseAddChainTail = do
	e1 <- parseMulChain
	op <- plusToken <|> minusToken
	e2 <- parseAddChain
	if op == Tokens.Plus then
		return $ NumAdd e1 e2
	else
		return $ NumSub e1 e2

parseMulChain :: OWLParser Expr
parseMulChain = (try parseMulChainTail) <|> (try parseNumLeaf)

parseMulChainTail :: OWLParser Expr
parseMulChainTail = do
	e1 <- parseNumLeaf
	op <- timesToken <|> divideToken <|> modulusToken
	e2 <- parseMulChain
	if op == Tokens.Times then
		return $ NumMul e1 e2
	else if op == Tokens.Divide then
		return $ NumDiv e1 e2
	else
		return $ NumMod e1 e2
