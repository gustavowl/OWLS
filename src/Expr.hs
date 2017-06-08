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
parseExpr = (try parseNumExpr) <|> (try parseBoolExpr) <|> (try parseArrayExpr) <|> parseStuffExpr








-- BUGADO -- Falta concertar o array
parseArrayExpr :: OWLParser Expr
parseArrayExpr = do
	lbrace
	expr <- (try parseNumber) <|> parseNumID
	comma
	rbrace
	return $ NumExpr expr


parseTaillArray :: OWLParser Expr
parseTaillArray = (try parseNumExpr) <|> (try parseBoolExpr) <|> (try parseStuffExpr)






---------------------------------------------------------------------------------------------------
-- Grammar - Numeric
---------------------------------------------------------------------------------------------------

parseNumExpr :: OWLParser Expr
parseNumExpr = do
	expr <- parseAddChain
	return $ NumExpr expr

parseNumLeaf :: OWLParser NumNode
parseNumLeaf = (try parseNumUnary)
	<|> (try parseNumber)
	<|> (try (parens parseAddChain))
	<|> (try parseNumFuncCall)
	<|> parseNumID

---------------------------------------------------------------------------------------------------
-- Grammar - Leaf Terms
---------------------------------------------------------------------------------------------------

parseNumID :: OWLParser NumNode
parseNumID = do
	name <- identifier
	return $ NumID name

parseNumFuncCall :: OWLParser NumNode
parseNumFuncCall = do
	(name, args) <- parseFuncCall
	return $ NumFuncCall name args

---------------------------------------------------------------------------------------------------
-- Grammar - Literal Leaf Terms
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
-- Gramar - Unary Operators
---------------------------------------------------------------------------------------------------

parseNumUnary :: OWLParser NumNode
parseNumUnary = parseMinus

parseMinus :: OWLParser NumNode
parseMinus = do
	op <- minusToken
	expr <- parseNumLeaf
	return $ NumMinus expr

---------------------------------------------------------------------------------------------------
-- Gramar - Binary Operators
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
-- Grammar
---------------------------------------------------------------------------------------------------

parseBoolExpr :: OWLParser Expr
parseBoolExpr = do
	expr <- parseOrChain
	return $ BoolExpr expr

parseBoolLeaf :: OWLParser BoolNode
parseBoolLeaf = (try parseBoolUnary)
	<|> (try parseBoolean)
	<|> (try (parens parseOrChain))
	<|> (try parseBoolFuncCall)
	<|> parseBoolID

---------------------------------------------------------------------------------------------------
-- Grammar - Leaf Terms
---------------------------------------------------------------------------------------------------

parseBoolean :: OWLParser BoolNode
parseBoolean = do
	b <- boolean
	return $ BoolLit b

parseBoolID :: OWLParser BoolNode
parseBoolID = do
	name <- identifier
	return $ BoolID name

parseBoolFuncCall :: OWLParser BoolNode
parseBoolFuncCall = do
	(name, args) <- parseFuncCall
	return $ BoolFuncCall name args

---------------------------------------------------------------------------------------------------
-- Gramar - Unary Operators
---------------------------------------------------------------------------------------------------

parseBoolUnary :: OWLParser BoolNode
parseBoolUnary = parseNeg

parseNeg :: OWLParser BoolNode
parseNeg = do
	notToken
	n <- parseBoolLeaf
	return $ BoolNot n

---------------------------------------------------------------------------------------------------
-- Gramar - Binary Operators
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
-- Gramar - Relational Binary Operators
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
-- Grammar - Stuff
---------------------------------------------------------------------------------------------------

parseStuffExpr :: OWLParser Expr
parseStuffExpr = do
	expr <- (try parseStuffFuncCall) <|> (try parseReadCall) <|> (try parseStuffID) <|> (try parseStuffChar) <|> parseStuffString
	return $ StuffExpr expr

parseStuffID :: OWLParser StuffNode
parseStuffID = do
	name <- identifier
	return $ StuffID name

parseStuffChar :: OWLParser StuffNode
parseStuffChar = do
	s <- cchar
	return $ StuffChar s

parseStuffArray :: OWLParser StuffNode
parseStuffArray = parseStuffGenericArray <|> parseStuffString
--- Falta Terminar ... Vitor vai terminar HJ A NOITE

parseStuffGenericArray :: OWLParser StuffNode
parseStuffGenericArray = do
	s <- sstring
	return $ StuffArrayString s

parseStuffString :: OWLParser StuffNode
parseStuffString = do
	s <- sstring
	return $ StuffArrayString $ convertToChar s

convertToChar :: String -> String  
convertToChar [] = []  
convertToChar (x:xs) =  "(StuffChar '" ++ [x] ++ "')" ++ convertToChar xs

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
-- Generic Function Call
---------------------------------------------------------------------------------------------------

parseFuncCall :: OWLParser (String, [Expr])
parseFuncCall = do
	id <- identifier
	args <- parens $ sepBy parseExpr comma
	return (id, args)