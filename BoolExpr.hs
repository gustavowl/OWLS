module BoolExpr where

import Text.ParserCombinators.Parsec
import Text.Parsec.Combinator
import State
import Lexer
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
parseBoolExpr = do
	return True -- TODO