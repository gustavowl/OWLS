module BoolExpr where

import Text.ParserCombinators.Parsec
import Text.Parsec.Combinator
import State
import Lexer
import NumExpr
import qualified Tokens

type OWLBoolInterpreter = OWLParser Bool

---------------------------------------------------------------------------------------------------
-- Variable types and values
---------------------------------------------------------------------------------------------------

isBoolean :: VarType -> Bool
isBoolean (AtomicType "bool") = True
isBoolean _ = False

getBoolean :: VarValue -> Bool
getBoolean (BoolValue v) = v
getBoolean _ = False

---------------------------------------------------------------------------------------------------
-- Grammar
---------------------------------------------------------------------------------------------------

parseBoolExpr :: OWLBoolInterpreter
parseBoolExpr = parseOrChain

parseBoolLeaf :: OWLBoolInterpreter
parseBoolLeaf = (try parseBoolUnary)
	<|> (try boolean)
	<|> (try (parens parseBoolExpr))
	<|> (try parseBoolFuncCall)
	<|> parseBoolID

---------------------------------------------------------------------------------------------------
-- Grammar - Leaf Terms
---------------------------------------------------------------------------------------------------

parseBoolID :: OWLBoolInterpreter
parseBoolID = do
	state <- getState
	id <- identifier
	let s = getVarScope state id
	let (_, _, t, v) = getVar state (id, s)
	if t == (AtomicType "bool") then
		return $ getBoolean v
	else
		fail $ "Variable value " ++ id ++ " is not boolean."

parseBoolFuncCall :: OWLBoolInterpreter
parseBoolFuncCall = parseBoolID -- TODO

---------------------------------------------------------------------------------------------------
-- Gramar - Unary Operators
---------------------------------------------------------------------------------------------------

parseBoolUnary :: OWLBoolInterpreter
parseBoolUnary = parseNeg

parseNeg :: OWLBoolInterpreter
parseNeg = do
	op <- notToken
	n <- parseBoolLeaf
	return $ not n

---------------------------------------------------------------------------------------------------
-- Gramar - Binary Operators
---------------------------------------------------------------------------------------------------

parseOrChain :: OWLBoolInterpreter
parseOrChain = (try parseOrChainTail) <|> (try parseAndChain)

parseOrChainTail :: OWLBoolInterpreter
parseOrChainTail = do
	e1 <- parseAndChain
	op <- orToken -- TODO: corToken
	e2 <- parseOrChain
	return $ e1 || e2

parseAndChain :: OWLBoolInterpreter
parseAndChain = (try parseAndChainTail) <|> (try parseEqChain)

parseAndChainTail :: OWLBoolInterpreter
parseAndChainTail = do
	e1 <- parseEqChain
	op <- andToken -- TODO: candToken
	e2 <- parseAndChain
	return $ e1 && e2

---------------------------------------------------------------------------------------------------
-- Gramar - Relational Binary Operators
---------------------------------------------------------------------------------------------------

parseEqChain :: OWLBoolInterpreter
parseEqChain = (try parseEqChainTail) <|> (try parseRelational) <|> (try parseBoolLeaf)

parseEqChainTail :: OWLBoolInterpreter
parseEqChainTail = do
	e1 <- parseRelational
	op <- eqToken <|> difToken
	e2 <- parseEqChain
	if op == Tokens.Equals then
		return $ e1 == e2
	else
		return $ not (e1 == e2)

parseRelational :: OWLBoolInterpreter
parseRelational = do
	(t1, v1) <- parseNumExpr
	op <- greaterToken <|> greaterEqToken <|> lessToken <|> lessEqToken
	(t2, v2) <- parseNumExpr
	if op == Tokens.Greater then
		return $ v1 > v2
	else if op == Tokens.Less then
		return $ v1 < v2
	else if op == Tokens.GreaterEq then
		return $ v1 >= v2
	else
		return $ v1 <= v2