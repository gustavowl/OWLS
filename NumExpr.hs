module NumExpr where

import Data.Fixed
import Text.ParserCombinators.Parsec
import Text.Parsec.Combinator
import State
import Lexer
import FuncCall
import qualified Tokens

type OWLNumInterpreter = OWLParser (VarType, Double)

---------------------------------------------------------------------------------------------------
-- Variable types and values
---------------------------------------------------------------------------------------------------

isNumber :: VarType -> Bool
isNumber (AtomicType "nat") = True
isNumber (AtomicType "int") = True
isNumber (AtomicType "real") = True
isNumber _ = False

getNumber :: VarValue -> Double
getNumber (NumberValue n) = n
getNumber _ = 0

---------------------------------------------------------------------------------------------------
-- Grammar
---------------------------------------------------------------------------------------------------

parseNumExpr :: OWLNumInterpreter
parseNumExpr = parseAddChain

parseNumLeaf :: OWLNumInterpreter
parseNumLeaf = (try parseNumUnary)
	<|> (try parseNumber)
	<|> (try (parens parseNumExpr))
	<|> (try parseNumFuncCall)
	<|> parseNumID

---------------------------------------------------------------------------------------------------
-- Grammar - Leaf Terms
---------------------------------------------------------------------------------------------------

parseNumID :: OWLNumInterpreter
parseNumID = do
	state <- getState
	name <- identifier
	let scope = getVarScope state name
	let (_, _, t, v) = getVar state (name, scope)
	if (checkType t (AtomicType "real")) && (isNumber t) then do
		let n = getNumber v
		if checkType t (AtomicType "nat") then
			return (AtomicType "nat", n)
		else if checkType t (AtomicType "int") then
			return (AtomicType "int", n)
		else
			return (AtomicType "real", n)
	else
		fail $ "Variable value " ++ name ++ " is not a number."

parseNumFuncCall :: OWLNumInterpreter
parseNumFuncCall = do
	(t, v) <- parseFuncCall $ AtomicType "real"
	return (t, getNumber v)

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
	n <- parseNumLeaf
	return $ numNeg n

numNeg :: (VarType, Double) -> (VarType, Double)
numNeg (AtomicType "nat", v) = (AtomicType "int", -v)
numNeg (t, v) = (t, -v)

---------------------------------------------------------------------------------------------------
-- Gramar - Binary Operators
---------------------------------------------------------------------------------------------------

parseAddChain :: OWLNumInterpreter
parseAddChain = (try parseAddChainTail) <|> (try parseMulChain)

parseAddChainTail :: OWLNumInterpreter
parseAddChainTail = do
	e1 <- parseMulChain
	op <- plusToken <|> minusToken
	e2 <- parseAddChain
	if op == Tokens.Plus then
		return $ numAdd e1 e2
	else
		return $ numSub e1 e2

numAdd :: (VarType, Double) -> (VarType, Double) -> (VarType, Double)
numAdd (AtomicType "real", v1) (_, v2) = (AtomicType "real", v1 + v2)
numAdd (_, v1) (AtomicType "real", v2) = (AtomicType "real", v1 + v2)
numAdd (AtomicType "int", v1) (_, v2) = (AtomicType "int", v1 + v2)
numAdd (_, v1) (AtomicType "int", v2) = (AtomicType "int", v1 + v2)
numAdd (_, v1) (_, v2) = (AtomicType "nat", v1 + v2)

numSub :: (VarType, Double) -> (VarType, Double) -> (VarType, Double)
numSub (AtomicType "real", v1) (_, v2) = (AtomicType "real", v1 - v2)
numSub (_, v1) (AtomicType "real", v2) = (AtomicType "real", v1 - v2)
numSub (_, v1) (_, v2) = (AtomicType "int", v1 - v2)

parseMulChain :: OWLNumInterpreter
parseMulChain = (try parseMulChainTail) <|> (try parseNumLeaf)

parseMulChainTail :: OWLNumInterpreter
parseMulChainTail = do
	e1 <- parseNumLeaf
	op <- timesToken <|> divideToken <|> modulusToken
	e2 <- parseMulChain
	if op == Tokens.Times then
		return $ numMul e1 e2
	else if op == Tokens.Divide then
		return $ numDiv e1 e2
	else
		return $ numMod e1 e2

numMul :: (VarType, Double) -> (VarType, Double) -> (VarType, Double)
numMul (AtomicType "real", v1) (_, v2) = (AtomicType "real", v1 * v2)
numMul (_, v1) (AtomicType "real", v2) = (AtomicType "real", v1 * v2)
numMul (AtomicType "int", v1) (_, v2) = (AtomicType "int", v1 * v2)
numMul (_, v1) (AtomicType "int", v2) = (AtomicType "int", v1 * v2)
numMul (_, v1) (_, v2) = (AtomicType "nat", v1 * v2)

numDiv :: (VarType, Double) -> (VarType, Double) -> (VarType, Double)
numDiv (_, v1) (_, v2) = (AtomicType "real", v1 / v2)

numMod :: (VarType, Double) -> (VarType, Double) -> (VarType, Double)
numMod (AtomicType "real", v1) (_, v2) = (AtomicType "real", mod' v1 v2)
numMod (_, v1) (AtomicType "real", v2) = (AtomicType "real", mod' v1 v2)
numMod (AtomicType "int", v1) (_, v2) = (AtomicType "int", mod' v1 v2)
numMod (_, v1) (AtomicType "int", v2) = (AtomicType "int", mod' v1 v2)
numMod (_, v1) (_, v2) = (AtomicType "nat", mod' v1 v2)