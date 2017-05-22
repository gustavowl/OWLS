module Expr where

import Debug.Trace
import State
import NumExpr
import BoolExpr

type OWLExprInterpreter = OWLParser VarValue

---------------------------------------------------------------------------------------------------
-- Default functions
---------------------------------------------------------------------------------------------------

-- parseExpr (expectedType)
parseExpr :: VarType -> OWLExprInterpreter
parseExpr (AtomicType "bool") = convertBoolExpr
parseExpr (AtomicType "nat") = convertNumExpr
parseExpr (AtomicType "int") = convertNumExpr
parseExpr (AtomicType "real") = convertNumExpr
parseExpr t = parseDefaultExpr t

convertNumExpr :: OWLExprInterpreter
convertNumExpr = do 
	(t, v) <- parseNumExpr
	return $ NumberValue v

convertBoolExpr :: OWLExprInterpreter
convertBoolExpr = do
	v <- parseBoolExpr
	return $ BoolValue v

parseDefaultExpr :: VarType -> OWLExprInterpreter
parseDefaultExpr t = do
	return $ NumberValue 0 -- TODO