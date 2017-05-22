module State where

import Data.Functor.Identity
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Tokens

---------------------------------------------------------------------------------------------------
-- State (global scope, local scope stack, user types)
---------------------------------------------------------------------------------------------------
type OWLState = (OWLScope, [OWLScope], [UserType])

---------------------------------------------------------------------------------------------------
-- Name table (functions IDs, procedure IDs, variable IDs)
---------------------------------------------------------------------------------------------------
type OWLScope = ([Function], [Procedure], [Var])

---------------------------------------------------------------------------------------------------
-- Function (parameters, return type, body)
---------------------------------------------------------------------------------------------------
type FuncDec = (String, [VarDec], VarType) 
type Function = (FuncDec, [Token])

---------------------------------------------------------------------------------------------------
-- Procedure (parameters, body)
---------------------------------------------------------------------------------------------------
type ProcDec = (String, [VarDec]) 
type Procedure = (ProcDec, [Token])

---------------------------------------------------------------------------------------------------
-- Variables (name, type, value)
---------------------------------------------------------------------------------------------------
-- A variable in the state table
type VarDec = (String, VarType)
type Var = (String, VarType, VarValue)

---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
-- UserType (name, [fields])
type UserType = (String, [(String, VarType)])

-- Description of variable type (atomic type, pointer of type, or array of type and given size)
data VarType = AtomicType String 
	| PointerType VarType 
	| ArrayType Integer VarType 
	| ProcType ProcDec | FuncType FuncDec
	deriving (Eq,Show)

---------------------------------------------------------------------------------------------------
-- Values
---------------------------------------------------------------------------------------------------
-- Description of variable value (primitive, struct or array)
data VarValue = NumberValue Double 
	| UserValue [VarValue] 
	| ArrayValue [VarValue]
	| PointerValue Key
	| CharValue Char
	| BoolValue Bool
	| SubprogramValue [Token]
	deriving (Eq,Show)

-- Name + scope ID
type Key = (String, Integer)

---------------------------------------------------------------------------------------------------
-- Parser Types
---------------------------------------------------------------------------------------------------
type OWLParser = ParsecT [Token] OWLState Identity
type OWLInterpreter = OWLParser (Maybe (VarType, VarValue))
type OWLStatement = OWLParser (Maybe (Maybe (VarType, VarValue)))

---------------------------------------------------------------------------------------------------
-- Update State
---------------------------------------------------------------------------------------------------

updateVar :: Key -> VarValue -> OWLState -> OWLState
updateVar (id, scope) v state = state -- TODO

updateFunc :: Key -> [Token] -> OWLState -> OWLState
updateFunc k v state = state -- TODO

updateProc :: Key -> [Token] -> OWLState -> OWLState
updateProc k v state = state -- TODO

addVar :: VarDec -> OWLState -> OWLState
addVar dec state = state -- TODO

addFunc :: FuncDec -> OWLState -> OWLState
addFunc dec state = state -- TODO

addProc :: ProcDec -> OWLState -> OWLState
addProc dec state = state -- TODO

---------------------------------------------------------------------------------------------------
-- Table access
---------------------------------------------------------------------------------------------------

getVar :: OWLState -> Key -> Var
getVar state (id, scope) = (id, AtomicType "int", NumberValue 0) -- TODO: get from state

getVarType :: OWLState -> Key -> VarType
getVarType state key = let (s, t, v) = getVar state key in t

getVarScope :: OWLState -> String -> Integer
getVarScope (global, locals, _) id = 0

isInScope :: OWLScope -> String -> Bool
isInScope scope id = False

---------------------------------------------------------------------------------------------------
-- Initial Values
---------------------------------------------------------------------------------------------------

initState :: OWLState
initState = (initScope, [], [])

initScope :: OWLScope
initScope = ([], [], [])

getInitValue :: VarType -> VarValue
getInitValue (AtomicType "nat") = NumberValue 0
getInitValue (AtomicType "int") = NumberValue 0
getInitValue (AtomicType "real") = NumberValue 0
getInitValue (AtomicType "char") = NumberValue 0
getInitValue (AtomicType "bool") = BoolValue False
getInitValue (AtomicType _) = UserValue [] -- TODO: initialize each field
getInitValue (PointerType _) = PointerValue ("", 0)
getInitValue (ArrayType _ n) = ArrayValue [] -- TODO: initialize each element
getInitValue (ProcType _) = SubprogramValue []