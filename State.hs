module State where

import Data.Functor.Identity
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Tokens

---------------------------------------------------------------------------------------------------
-- State (var table, func table, proc table, type table)
---------------------------------------------------------------------------------------------------
type OWLState = ([Var], [Function], [Procedure], [UserType])

---------------------------------------------------------------------------------------------------
-- Function (parameters, return type, body)
---------------------------------------------------------------------------------------------------
type FuncDec = (String, Integer, [VarDec], VarType) 
type Function = (String, Integer, [VarDec], VarType, [Token])

---------------------------------------------------------------------------------------------------
-- Procedure (parameters, body)
---------------------------------------------------------------------------------------------------
type ProcDec = (String, Integer, [VarDec]) 
type Procedure = (String, Integer, [VarDec], [Token])

---------------------------------------------------------------------------------------------------
-- Variables (name, type, value)
---------------------------------------------------------------------------------------------------
type UpdatedVar = (Var, Bool)
-- A variable in the state table
type VarDec = (String, Integer, VarType)
-- String id, Integer scope, VarType type, VarValue
type Var = (String, Integer, VarType, VarValue)

---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
-- UserType (name, [fields])
type UserType = (String, [(String, VarType)])

-- Description of variable type (atomic type, pointer of type, or array of type and given size)
data VarType = AtomicType String 
	| PointerType VarType 
	| ArrayType Integer VarType 
	| FuncType FuncDec
	| ProcType ProcDec 
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
	| FuncValue Key
	| ProcValue Key
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

upVar :: Var -> String -> Integer -> VarValue -> UpdatedVar
upVar (vName, vScope, vType, vValue) name scope newValue = 
	if vName == name && vScope == scope then
		((vName, vScope, vType, newValue), True)
	else 
		((vName, vScope, vType, vValue), False)

recVar :: [Var] -> String -> Integer -> VarValue -> Bool -> [Var]
recVar [] name scope v up = if up then [] else fail "Error"
recVar (hVar:tailVar) name scope v up = 
	let (var, u) = upVar hVar name scope v in var : recVar tailVar name scope v (up || u)

updateVar :: Key -> VarValue -> OWLState -> OWLState
updateVar (name, scope) v (var, func, proc, types) = 
	let newVar = recVar var name scope v False in (newVar, func, proc, types)

updateFunc :: Key -> [Token] -> OWLState -> OWLState
updateFunc (name, scope) v (var, func, proc, types) = (var, func, proc, types) -- TODO

updateProc :: Key -> [Token] -> OWLState -> OWLState
updateProc (name, scope) v (var, func, proc, types) = (var, func, proc, types) -- TODO

addVar :: VarDec -> OWLState -> OWLState
addVar (name, scope, typ) (var, func, proc, types) = 
	let newVar = (name, scope, typ, getInitValue typ) in
	(newVar:var, func, proc, types)

addFunc :: FuncDec -> OWLState -> OWLState
addFunc (name, scope, params, ret) (var, func, proc, types) = 
	let newFunc = (name, scope, params, ret, []) in
	(var, newFunc:func, proc, types)

addProc :: ProcDec -> OWLState -> OWLState
addProc (name, scope, params) (var, func, proc, types) = 
	let newProc = (name, scope, params, []) in
	(var, func, newProc:proc, types)

---------------------------------------------------------------------------------------------------
-- Table access
---------------------------------------------------------------------------------------------------

getVar :: OWLState -> Key -> Var
getVar state (name, scope) = (name, scope, AtomicType "int", NumberValue 0) -- TODO: get from state

getVarType :: OWLState -> Key -> VarType
getVarType state key = let (_, _, t, _) = getVar state key in t

getVarScope :: OWLState -> String -> Integer
getVarScope (var, func, proc, types) id = 0 -- TODO

getFunc :: OWLState -> Key -> Function
getFunc state (name, scope) = (name, scope, [], AtomicType "int", []) -- TODO: get from state

getFuncRet :: OWLState -> Key -> VarType
getFuncRet state key = let (_, _, _, t, _) = getFunc state key in t

getFuncScope :: OWLState -> String -> Integer
getFuncScope (var, func, proc, types) id = 0 -- TODO

---------------------------------------------------------------------------------------------------
-- Initial Values
---------------------------------------------------------------------------------------------------

initState :: OWLState
initState = ([], [], [], [])

getInitValue :: VarType -> VarValue
getInitValue (AtomicType "nat") = NumberValue 0
getInitValue (AtomicType "int") = NumberValue 0
getInitValue (AtomicType "real") = NumberValue 0
getInitValue (AtomicType "char") = NumberValue 0
getInitValue (AtomicType "bool") = BoolValue False
getInitValue (AtomicType _) = UserValue [] -- TODO: initialize each field
getInitValue (PointerType _) = PointerValue ("", 0)
getInitValue (ArrayType _ n) = ArrayValue [] -- TODO: initialize each element
getInitValue (FuncType _) = FuncValue ("", -1)
getInitValue (ProcType _) = ProcValue ("", -1)