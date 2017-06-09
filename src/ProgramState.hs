module ProgramState where

import ProgramTree

---------------------------------------------------------------------------------------------------
-- State
---------------------------------------------------------------------------------------------------

-- Pilha de escopos, lista de tipos definidos pelo usuário
type OWLState = ([Scope], [UserType])  

-- ID do escopo atual, ID do escopo ancestral, tabela de símbolos
type Scope = (Integer, Integer, [TableEntry])

-- Nome, Tipo, Valor
type TableEntry = (String, VarType, VarValue)

type UserType = ()

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
	| FuncValue Integer [Declaration] [Statement] -- ancestral, params (tipo + nome), corpo
	| ProcValue Integer [Declaration] [Statement]
	deriving (Eq,Show)

-- Name + scope ID
type Key = (String, Integer)

---------------------------------------------------------------------------------------------------
-- Gambiarras
---------------------------------------------------------------------------------------------------

nullScope :: Scope 
nullScope = (-1, -1, [])

nullKey :: Key
nullKey = ("NULL", -1)

nullVarType :: VarType
nullVarType = AtomicType "NULL"

nullVarValue :: VarValue
nullVarValue = NumberValue 123456

---------------------------------------------------------------------------------------------------
-- Symbol Table Access
---------------------------------------------------------------------------------------------------

-- Cria um novo escopo para uma chamada de função cujo ancestral é o dado como argumento.
newScope :: Integer -> OWLState -> OWLState
newScope newParentID ((currentID, parentID, table):scopeList, types) = 
	((currentID + 1, newParentID, []):(currentID, parentID, table):scopeList, types)

---------------------------------------------------------------------------------------------------
-- Scope Resolution
---------------------------------------------------------------------------------------------------

getScope :: Integer -> OWLState -> Scope
getScope scopeID (stack, _) = f (popToScope scopeID stack) where
	f [] = nullScope
	f (h:t) = h

getScopeID :: String -> OWLState -> Integer
getScopeID name (stack, _) = searchVarScope name stack

searchVarScope :: String -> [Scope] -> Integer
searchVarScope name [] = -1
searchVarScope name ((currentID, parentID, table):scopes) =
	if isInScope name table then
		currentID
	else
		searchVarScope name (popToScope parentID scopes)

popToScope :: Integer -> [Scope] -> [Scope]
popToScope scopeID [] = []
popToScope scopeID ((currentID, parentID, table):scopes) =
	if currentID == scopeID then
		(currentID, parentID, table):scopes
	else
		popToScope scopeID scopes

isInScope :: String -> [TableEntry] -> Bool
isInScope name [] = False
isInScope name ((varName, _, _):t) = 
	if name == varName then 
		True 
	else 
		isInScope name t

---------------------------------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------------------------------

addVarDec :: String -> VarType -> OWLState -> OWLState
addVarDec name varType ([(a, b, table)], types) = let
	newElement = (name, varType, getInitValue varType) in
	([(a, b, newElement:table)], types)

updateVar :: VarValue -> Key -> OWLState -> OWLState
updateVar value key state = state

getVar :: Key -> OWLState -> (VarType, VarValue)
getVar (name, scopeID) state = let
	(_, _, table) = getScope scopeID state in getVarFromTable name table

getVarFromTable :: String -> [TableEntry] -> (VarType, VarValue)
getVarFromTable name [] = (nullVarType, nullVarValue)
getVarFromTable name ((name', t, v):table) = 
	if name == name' then 
		(t, v)
	else 
		getVarFromTable name table

addFuncDec :: String -> [Declaration] -> VarType -> [Statement] -> OWLState -> OWLState
addFuncDec name params ret body ([(a, b, table)], types) = let 
	newFuncDec = (name, FuncType (extractParamTypes params) ret, FuncValue a params body) in
	([(a, b, newFuncDec:table)], types)

addProcDec :: String -> [Declaration] -> [Statement] -> OWLState -> OWLState
addProcDec name params body ([(a, b, table)], types) = let 
	newProcDec = (name, ProcType (extractParamTypes params), ProcValue a params body) in
	([(a, b, newProcDec:table)], types)

extractParamType :: Declaration -> VarType
extractParamType (Var _ varType _) = varType
extractParamType (Function _ params ret _) = FuncType (extractParamTypes params) ret
extractParamType (Procedure _ params _) = ProcType (extractParamTypes params)

extractParamTypes :: [Declaration] -> [VarType]
extractParamTypes [] = []
extractParamTypes (h:params) = (extractParamType h) : (extractParamTypes params)

{-
setVarValue :: VarType -> (Maybe Expr) -> State -> VarValue
setVarValue varType (Just expr) state = getInitValue varType -- TODO: avaliar expressão
setVarValue varType Nothing _ = getInitValue varType 

-- scope é id do ancestral
addDec :: Declaration -> Integer -> [TableEntry] -> [TableEntry]
addDec (Var name varType expr) _ table = (name, varType, setVarValue varType expr state) : table
addDec (Function name params ret body) scope table = 
	(name, FuncType (extractParamTypes params) ret, FuncValue scope params body) : table
addDec (Procedure name params body) scope table = 
	(name, ProcType (extractParamTypes params), ProcValue scope params body) : table

-- lista de declaração, id do escopo atual
addGlobalDecsTable :: [Declaration] -> Integer -> [TableEntry] -> [TableEntry]
addGlobalDecsTable [] _ table = table
addGlobalDecsTable (h:decs) scope table = addGlobalDecsTable decs scope (addDec h scope table)

addGlobalDecs :: [Declaration] -> OWLState -> OWLState
addGlobalDecs decs ([(current, ances, table)], userType) = 
	let newState = addGlobalDecsTable decs current table in ([(current, ances, newTable)], userType)

addLocalDec :: Declaration -> OWLState -> OWLState
addLocalDec (Var name t Nothing) state = state -- TODO
addLocalDec (Var name t (Just v)) state = state -- TODO
addLocalDec _ state = state -- TODO

-}

-- TODO
-- ...

---------------------------------------------------------------------------------------------------
-- Initial Values
---------------------------------------------------------------------------------------------------

-- Apenas o escopo global, vazio.
initState :: OWLState
initState = ([(0, -1, [])], [])

getInitValue :: VarType -> VarValue
getInitValue (AtomicType "nat") = NumberValue 0
getInitValue (AtomicType "int") = NumberValue 0
getInitValue (AtomicType "real") = NumberValue 0
getInitValue (AtomicType "char") = CharValue 'a'
getInitValue (AtomicType "bool") = BoolValue False
getInitValue (AtomicType _) = UserValue [] -- TODO: initialize each field
getInitValue (PointerType _) = PointerValue ("", 0)
getInitValue (ArrayType _ n) = ArrayValue [] -- TODO: initialize each element
getInitValue (FuncType _ _) = FuncValue (-1) [] []
getInitValue (ProcType _) = ProcValue (-1) [] []