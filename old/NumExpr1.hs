module Expr where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Prim
import Lexer
import qualified Tokens

type OWLNumInterpreter = OWLParser (VarType, Double)

---------------------------------------------------------------------------------------------------
-- Primitive type convertions
---------------------------------------------------------------------------------------------------

checkType :: VarType -> VarType -> Boolean
checkType (AtomicType "nat") (AtomicType "int") = True
checkType (AtomicType "int") (AtomicType "real") = True 
checkType (AtomicType "nat") (AtomicType "real") = True
checkType a b = a == b

isNumber :: VarType -> Boolean
isNumber (AtomicType "nat") = True
isNumber (AtomicType "int") = True
isNumber (AtomicType "real") = True
isNumber _ = False

---------------------------------------------------------------------------------------------------
-- Grammar
---------------------------------------------------------------------------------------------------

parseNumExpr :: OWLInterpreter
parseNumExpr = do
	(t, v) <- parseAddChain
	return $ Just (t, NumberValue v)

parseExprNumLeaf :: OWLNumInterpreter
parseExprNumLeaf = (try parseNumUnary)
	<|> (try parseNumber)
	<|> (try (parens parseNumExpr))
	<|> (try parseNumFuncCall)
	<|> parseNumID

---------------------------------------------------------------------------------------------------
-- Grammar - Leaf Terms
---------------------------------------------------------------------------------------------------

parseNumID :: OWLNumInterpreter
parseNumID = do
	id <- identifier
	k <- getVarKey id
	((_, t), v) <- getVar k
	n <- getVarNum v
	if checkType t (AtomicType "nat") then
		return (AtomicType "nat", n)
	else if checkType t (AtomicType "int") then
		return (AtomicType "int", n)
	else if checkType t (AtomicType "real") then
		return (AtomicType "real", n)
	else
		fail "Variable value " ++ id ++ " is not a number."
	return v

getVarNum :: VarValue -> OWLNumInterpreter
getVarNum _ (NumberValue n) = do return n
getVarNum id _ = do fail "Variable value " ++ id ++ " is not a number."

---------------------------------------------------------------------------------------------------
-- Grammar - Literal Leaf Terms
---------------------------------------------------------------------------------------------------

parseNumber :: OWLNumInterpreter
parseNumber = parseNatural <|> parseInteger <|> parseReal

parseNatural :: OWLNumInterpreter
parseNatural = do
	n <- natural
	return (AtomicType "nat", n)

parseInteger :: OWLNumInterpreter
parseInteger = do
	n <- integer
	return (AtomicType "int", n)

parseReal :: OWLNumInterpreter
parseReal = do
	n <- real
	return (AtomicType "real", n)

---------------------------------------------------------------------------------------------------
-- Gramar - Unary Operators
---------------------------------------------------------------------------------------------------

parseNumUnary :: OWLNumInterpreter
parseNumUnary = parseMinus

parseMinus :: OWLNumInterpreter
parseMinus = do
	op <- minusToken
	n <- parseExprNumLeaf
	return $ numNegate n

numNegate :: (VarType, Double) -> (VarType, Double)
numNegate (AtomicType "nat", v) = (AtomicType "int", -v)
numNegate (t, v) = (t, -v)
numNegate _ = fail "WTF"

---------------------------------------------------------------------------------------------------
-- Gramar - Numeric Binary Operators
---------------------------------------------------------------------------------------------------

parseAddChain :: OWLNumInterpreter
parseAddChain = (try parseAddChainTail) <|> (try parseMulChain)

parseAddChainTail :: OWLNumInterpreter
parseAddChainTail = do
	e1 <- parseMulChain
	op <- plusToken <|> minusToken
	e2 <- parseAddChain
	if checkType 
	if op = Tokens.Plus then
		return $ numAdd e1 e2
	else
		return $ numSub e1 e2

numAdd :: (VarType, Double) -> (VarType, Double) -> (VarType, Double)
numAdd (AtomicType "real", v1) (_, v2) = (AtomicType "real", v1 + v2)
numAdd (_, v1) (AtomicType "real", v2) = (AtomicType "real", v1 + v2)
numAdd (AtomicType "int", v1) (_, v2) = (AtomicType "int", v1 + v2)
numAdd (_, v1) (AtomicType "int", v2) = (AtomicType "int", v1 + v2)

numSub :: (VarType, Double) -> (VarType, Double) -> (VarType, Double)
numSub (t1, v1) (t2 v2) = (t1, v1 - v2)

parseMulChain :: OWLParser Expr
parseMulChain = (try parseMulChainTail) <|> (try parseExprLeaf)

parseMulChainTail :: OWLParser Expr
parseMulChainTail = do
	(t1, v1) <- parseExprLeaf
	op <- timesToken <|> parseDivOp <|> parseModOp
	(t2, v2) <- parseMulChain
	return $ ExprBinOp op e1 e2

parseMulOp :: OWLParser BinOp
parseMulOp = do
	
	return $ Mul

parseDivOp :: OWLParser BinOp
parseDivOp = do
	divideToken
	return $ Div

parseModOp :: OWLParser BinOp
parseModOp = do
	modulusToken
	return $ Mod