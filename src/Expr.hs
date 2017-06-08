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
parseExpr = (try parseNumExpr) 
	<|> (try parseBoolExpr) 
	<|> parseStuffExpr

---------------------------------------------------------------------------------------------------
-- Numeric Expr
---------------------------------------------------------------------------------------------------

parseNumExpr :: OWLParser Expr
parseNumExpr = do
	expr <- parseNumNode
	return $ NumExpr expr

parseNumNode :: OWLParser NumNode
parseNumNode = parseAddChain

parseNumLeaf :: OWLParser NumNode
parseNumLeaf = (try parseNumUnary)
	<|> (try parseNumber)
	<|> (try (parens parseNumNode))
	<|> (try parseNumFuncCall)
	<|> (try parseNumEl)
	<|> parseNumID

---------------------------------------------------------------------------------------------------
-- Numeric Leaf Terms
---------------------------------------------------------------------------------------------------

parseNumID :: OWLParser NumNode
parseNumID = do
	name <- identifier
	return $ NumID name

parseNumEl :: OWLParser NumNode
parseNumEl = do
	(array, size) <- parseArrayEl
	return $ NumEl array size

parseNumFuncCall :: OWLParser NumNode
parseNumFuncCall = do
	(name, args) <- parseFuncCall
	return $ NumFuncCall name args

---------------------------------------------------------------------------------------------------
-- Numeric Literal Leaf Terms
---------------------------------------------------------------------------------------------------

parseNumber :: OWLParser NumNode
parseNumber = parseNatural <|> parseInteger <|> parseReal

parseNatural :: OWLParser NumNode
parseNatural = do
	n <- natural
	return $ NumNat n

parseInteger :: OWLParser NumNode
parseInteger = do
	n <- integer
	return $ NumInt n

parseReal :: OWLParser NumNode
parseReal = do
	n <- real
	return $ NumReal n

---------------------------------------------------------------------------------------------------
-- Numeric Unary Operators
---------------------------------------------------------------------------------------------------

parseNumUnary :: OWLParser NumNode
parseNumUnary = parseMinus

parseMinus :: OWLParser NumNode
parseMinus = do
	op <- minusToken
	expr <- parseNumLeaf
	return $ NumMinus expr

---------------------------------------------------------------------------------------------------
-- Numeric Binary Operators
---------------------------------------------------------------------------------------------------

parseAddChain :: OWLParser NumNode
parseAddChain = (try parseAddChainTail) <|> (try parseMulChain)

parseAddChainTail :: OWLParser NumNode
parseAddChainTail = do
	e1 <- parseMulChain
	op <- plusToken <|> minusToken
	e2 <- parseAddChain
	if op == Tokens.Plus then
		return $ NumAdd e1 e2
	else
		return $ NumSub e1 e2

parseMulChain :: OWLParser NumNode
parseMulChain = (try parseMulChainTail) <|> (try parseNumLeaf)

parseMulChainTail :: OWLParser NumNode
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

---------------------------------------------------------------------------------------------------
-- Boolean Expr
---------------------------------------------------------------------------------------------------

parseBoolExpr :: OWLParser Expr
parseBoolExpr = do
	expr <- parseBoolNode
	return $ BoolExpr expr

parseBoolNode :: OWLParser BoolNode
parseBoolNode = parseOrChain

parseBoolLeaf :: OWLParser BoolNode
parseBoolLeaf = (try parseBoolUnary)
	<|> (try parseBoolean)
	<|> (try (parens parseBoolNode))
	<|> (try parseBoolFuncCall)
	<|> (try parseBoolEl)
	<|> parseBoolID

---------------------------------------------------------------------------------------------------
-- Boolean Leaf Terms
---------------------------------------------------------------------------------------------------

parseBoolean :: OWLParser BoolNode
parseBoolean = do
	b <- boolean
	return $ BoolLit b

parseBoolID :: OWLParser BoolNode
parseBoolID = do
	name <- identifier
	return $ BoolID name

parseBoolEl :: OWLParser BoolNode
parseBoolEl = do
	(array, size) <- parseArrayEl
	return $ BoolEl array size

parseBoolFuncCall :: OWLParser BoolNode
parseBoolFuncCall = do
	(name, args) <- parseFuncCall
	return $ BoolFuncCall name args

---------------------------------------------------------------------------------------------------
-- Boolean Unary Operators
---------------------------------------------------------------------------------------------------

parseBoolUnary :: OWLParser BoolNode
parseBoolUnary = parseNeg

parseNeg :: OWLParser BoolNode
parseNeg = do
	notToken
	n <- parseBoolLeaf
	return $ BoolNot n

---------------------------------------------------------------------------------------------------
-- Boolean Binary Operators
---------------------------------------------------------------------------------------------------

parseOrChain :: OWLParser BoolNode
parseOrChain = (try parseOrChainTail) <|> (try parseAndChain)

parseOrChainTail :: OWLParser BoolNode
parseOrChainTail = do
	e1 <- parseAndChain
	op <- orToken <|> corToken
	e2 <- parseOrChain
	if op == Tokens.Or then
		return $ BoolOr e1 e1
	else
		return $ BoolOrC e1 e2

parseAndChain :: OWLParser BoolNode
parseAndChain = (try parseAndChainTail) <|> (try parseRelational)

parseAndChainTail :: OWLParser BoolNode
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

parseRelational :: OWLParser BoolNode
parseRelational = (try parseRelationalTail) <|> parseBoolLeaf

parseRelationalTail :: OWLParser BoolNode
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
-- Stuff Expr
---------------------------------------------------------------------------------------------------

parseStuffExpr :: OWLParser Expr
parseStuffExpr = do
	expr <- parseStuffNode
	return $ StuffExpr expr

parseStuffNode :: OWLParser StuffNode
parseStuffNode = (try parseStuffFuncCall) 
	<|> (try parseReadCall) 
	<|> (try parseStuffID) 
	<|> (try parseStuffChar)
	<|> (try parseStuffArray)
--	<|> (try parseStuffEl)
	<|> parseStuffString

parseStuffID :: OWLParser StuffNode
parseStuffID = do
	name <- identifier
	return $ StuffID name

parseStuffEl :: OWLParser StuffNode
parseStuffEl = do
	(array, size) <- parseArrayEl
	return $ StuffEl array size

parseStuffChar :: OWLParser StuffNode
parseStuffChar = do
	s <- cchar
	return $ StuffChar s

parseStuffArray :: OWLParser StuffNode
parseStuffArray = do
	exprs <- braces (sepBy parseExpr comma)
	return $ StuffArray exprs

parseStuffString :: OWLParser StuffNode
parseStuffString = do
	s <- sstring
	return $ StuffArray $ stringToArray s

stringToArray :: String -> [Expr]  
stringToArray [] = []  
stringToArray (x:xs) = (StuffExpr (StuffChar x)) : stringToArray xs

parseStuffFuncCall :: OWLParser StuffNode
parseStuffFuncCall = do
	(name, args) <- parseFuncCall
	return $ StuffFuncCall name args

parseReadCall :: OWLParser StuffNode
parseReadCall = do
	readToken
	arg <- parens parseExpr
	return $ StuffReadCall arg

---------------------------------------------------------------------------------------------------
-- General Operators
---------------------------------------------------------------------------------------------------

parseFuncCall :: OWLParser (String, [Expr])
parseFuncCall = do
	id <- identifier
	args <- parens $ sepBy parseExpr comma
	return (id, args)

parseArrayEl :: OWLParser (StuffNode, NumNode)
parseArrayEl = do
	array <- parseStuffNode
	size <- brackets parseNumNode
	return (array, size)