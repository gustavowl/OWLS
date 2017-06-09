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
	<|> (try parseNumFuncCall)
	<|> (try parseNumEl)
	<|> (try parseNumPtr)
	<|> (try parseNumField)
	<|> (try parseNumID)
	<|> (parens parseNumNode)

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

parseNumPtr :: OWLParser NumNode
parseNumPtr = do
	ptr <- parsePtrContent
	return $ NumPtr ptr

parseNumField :: OWLParser NumNode
parseNumField = do
	(node, field) <- parseField
	return $ NumField node field

parseNumFuncCall :: OWLParser NumNode
parseNumFuncCall = do
	(func, args) <- parseFuncCall
	return $ NumFuncCall func args

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
	<|> (try parseBoolFuncCall)
	<|> (try parseBoolEl)
	<|> (try parseBoolPtr)
	<|> (try parseBoolField)
	<|> (try parseBoolID)
	<|> (parens parseBoolNode)

---------------------------------------------------------------------------------------------------
-- Boolean Literal
---------------------------------------------------------------------------------------------------

parseBoolean :: OWLParser BoolNode
parseBoolean = do
	b <- boolean
	return $ BoolLit b

---------------------------------------------------------------------------------------------------
-- Boolean Leaf Terms
---------------------------------------------------------------------------------------------------

parseBoolID :: OWLParser BoolNode
parseBoolID = do
	name <- identifier
	return $ BoolID name

parseBoolEl :: OWLParser BoolNode
parseBoolEl = do
	(array, size) <- parseArrayEl
	return $ BoolEl array size

parseBoolPtr :: OWLParser BoolNode
parseBoolPtr = do
	ptr <- parsePtrContent
	return $ BoolPtr ptr

parseBoolField :: OWLParser BoolNode
parseBoolField = do
	(node, field) <- parseField
	return $ BoolField node field

parseBoolFuncCall :: OWLParser BoolNode
parseBoolFuncCall = do
	(func, args) <- parseFuncCall
	return $ BoolFuncCall func args

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
	expr <- try parseStuffNode
	return $ StuffExpr expr

parseStuffNode :: OWLParser StuffNode
parseStuffNode = (try parseReadCall)
	<|> (try parseStuffFuncCall) 
	<|> (try parseStuffEl)
	<|> (try parseStuffChar)
	<|> (try parseStuffArray)
	<|> (try parseStuffString)
	<|> (try parseStuffID) 
	<|> (parens parseStuffNode)

---------------------------------------------------------------------------------------------------
-- Stuff Literals
---------------------------------------------------------------------------------------------------

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

---------------------------------------------------------------------------------------------------
-- Stuff Leaf Terms
---------------------------------------------------------------------------------------------------

parseStuffID :: OWLParser StuffNode
parseStuffID = do
	name <- identifier
	return $ StuffID name

parseStuffEl :: OWLParser StuffNode
parseStuffEl = do
	(array, size) <- parseArrayEl
	return $ StuffEl array size

parseStuffPtr :: OWLParser StuffNode
parseStuffPtr = do
	ptr <- parsePtrContent
	return $ StuffPtr ptr

parseStuffField :: OWLParser StuffNode
parseStuffField = do
	(node, name) <- parseField
	return $ StuffField node name

parseStuffFuncCall :: OWLParser StuffNode
parseStuffFuncCall = do
	(func, args) <- parseFuncCall
	return $ StuffFuncCall func args

parseReadCall :: OWLParser StuffNode
parseReadCall = do
	readToken
	lparen
	rparen
	return $ StuffReadCall

---------------------------------------------------------------------------------------------------
-- General Operators
---------------------------------------------------------------------------------------------------

parseFuncCall :: OWLParser (String, [Expr])
parseFuncCall = do
	func <- identifier
	args <- parens $ sepBy parseExpr comma
	return (func, args)

parseArrayEl :: OWLParser (StuffNode, NumNode)
parseArrayEl = do
	array <- parseStuffID <|> (parens parseStuffNode)
	size <- brackets parseNumNode
	return (array, size)

parseField :: OWLParser (StuffNode, String)
parseField = do
	struct <- parseStuffID <|> (parens parseStuffNode)
	dotToken
	field <- identifier
	return (struct, field)

parsePtrContent :: OWLParser StuffNode
parsePtrContent = do
	atToken
	ptr <- parseStuffID <|> (parens parseStuffNode)
	return ptr