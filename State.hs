module State where

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
type ProcDec = (String, [VarDec], [Statement]) 
type Procedure = (ProcDec, [Token])

---------------------------------------------------------------------------------------------------
-- Variables (name, type, value)
---------------------------------------------------------------------------------------------------
-- A variable in the state table
type VarDec = (String, VarType)
type Var = (String, VarDec, Value)

---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
-- UserType (name, [fields])
data UserType = (String, [(String, VarType)])

-- Description of variable type (atomic type, pointer of type, or array of type and given size)
data VarType = AtomicType String 
	| PointerType VarType 
	| ArrayType Integer VarType 
	| ProcType ProcDec | FuncType FuncDec

---------------------------------------------------------------------------------------------------
-- Values
---------------------------------------------------------------------------------------------------
-- Description of variable value (primitive, struct or array)
data VarValue = NumberValue Double 
	| UserValue [VarValue] 
	| ArrayValue [VarValue]
	| PointerValue Key
	| CharValue Char
	| BoolValue Boolean
	| SubprogramValue [Token]

-- Name + scope ID
type Key = (String, Integer)

---------------------------------------------------------------------------------------------------
-- Update State
---------------------------------------------------------------------------------------------------

updateVar :: OWLState -> String -> VarValue -> OWLState
updateVar state k v = state -- TODO

updateFunc :: OWLState -> String -> [Token] -> OWLState
updateFunc state k v = state -- TODO

updateProc :: OWLState -> String -> [Token] -> OWLState
updateProc state k v = state -- TODO

addVar :: OWLState -> VarDec -> OWLState
addVar state k v = state -- TODO

addFunc :: OWLState -> FuncDec -> OWLState
addFunc state k v = state -- TODO

addProc :: OWLState -> ProcDec -> OWLState
addProc state k v = state -- TODO

---------------------------------------------------------------------------------------------------
-- Table access
---------------------------------------------------------------------------------------------------

getVar :: OWLState -> Key -> Var
getVar (id, scope) = ((id, AtomicType "int"), NumberValue 0) -- TODO: getFrom state

getVarType :: OWLState -> Key -> VarType
getVarType state (id, scope) = do 
	(_, t, v) <- getVar state (id, scoppe)
	return t

getVarScope :: OWLState -> String -> Integer
getVarScope id = 0 -- TODO

---------------------------------------------------------------------------------------------------
-- Initial Values
---------------------------------------------------------------------------------------------------

getInitValue :: VarType -> VarValue
getInitValue (AtomicType "nat") = NumberValue 0
getInitValue (AtomicType "int") = NumberValue 0
getInitValue (AtomicType "real") = NumberValue 0
getInitValue (AtomicType "char") = NumberValue 0
getInitValue (AtomicType "bool") = BoolValue False
getInitValue (AtomicType _) = UserValue [] -- TODO: initialize each field
getInitValue (PointerType _) = PointerValue ("", 0)
getInitValue (ArrayType _) = ArrayValue [] -- TODO: initialize each element
getInitValue (ProcType _) = SubprogramValue []