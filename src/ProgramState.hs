module ProgramState where

import ProgramTree
import Types

---------------------------------------------------------------------------------------------------
-- State
---------------------------------------------------------------------------------------------------

-- Contador de escopos, contador de variáveis anônimas, Pilha de escopos, lista de tipos definidos pelo usuário
type OWLState = (Integer, Integer, [Scope], [UserType])  

-- ID do escopo atual, ID do escopo ancestral, tabela de símbolos
type Scope = (Integer, Integer, [TableEntry])

-- Nome, Tipo, Valor
type TableEntry = (String, VarType, VarValue)

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
nullVarValue = NumberValue 12345666

---------------------------------------------------------------------------------------------------
-- Scope
---------------------------------------------------------------------------------------------------

-- Cria um novo escopo para uma chamada de função cujo ancestral é o dado como argumento.
newScope :: Integer -> OWLState -> OWLState
newScope newParentID (count, avars, scopeList, types) = 
	(count + 1, avars, (count, newParentID, []):scopeList, types)

getScope :: Integer -> OWLState -> Scope
getScope scopeID (_, _, stack, _) = f (popToScope scopeID stack) where
	f [] = nullScope
	f (h:t) = h

getScopeID :: String -> OWLState -> IO Integer
getScopeID name (_, _, stack, _) = do
	let id = searchVarScope name stack
	if id == -1 then do
		--print stack
		fail $ "Variable " ++ name ++ " not found."
	else
		return id

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

popScope :: OWLState -> OWLState
popScope (c1, c2, h:t, types) = (c1, c2, t, types)
popScope a = a -- Should not happen.

---------------------------------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------------------------------

addVarDec :: String -> VarType -> OWLState -> OWLState
addVarDec name varType (c1, c2, [], types) = (c1, c2, [], types)
addVarDec name varType (c1, c2, (a, b, table):scopes, types) = let
	newElement = (name, varType, (getInitValue varType types)) in
	(c1, c2, (a, b, newElement:table):scopes, types)

updateTableEntry :: VarValue -> String -> [TableEntry] -> [TableEntry] 
updateTableEntry v name [] = []
updateTableEntry newValue name ((name', t, v):table) = 
	if name == name' then
		(name, t, newValue) : updateTableEntry newValue name (table)
	else 
		(name', t, v) : (updateTableEntry newValue name table)

updateScope :: VarValue -> Key -> Scope -> Scope
updateScope v (name, scope) (current, b, table) = 
	if current == scope then 
		(current, b, updateTableEntry v name table)
	else 
		(current, b, table)

updateVarScopes :: VarValue -> Key -> [Scope] -> [Scope]
updateVarScopes _ _ [] = []
updateVarScopes v k (h:scopes) = (updateScope v k h) : (updateVarScopes v k scopes)

updateVar :: VarValue -> Key -> OWLState -> OWLState
updateVar value key (c1, c2, scopes, userTypes) = (c1, c2, (updateVarScopes value key scopes), userTypes)

getVar :: Key -> OWLState -> IO (VarType, VarValue)
getVar (name, scopeID) state = do
	let (_, _, table) = getScope scopeID state 
	var <- getVarFromTable name table
	return var

getVarFromTable :: String -> [TableEntry] -> IO (VarType, VarValue)
getVarFromTable name [] = fail "Null pointer."
getVarFromTable name ((name', t, v):table) = do
	if name == name' then 
		return (t, v)
	else do
		var <- getVarFromTable name table
		return var

addFuncDec :: String -> [Declaration] -> VarType -> [Statement] -> OWLState -> OWLState
addFuncDec name params ret body (c1, c2, [(a, b, table)], types) = let 
	newFuncDec = (name, FuncType (extractParamTypes params) ret, FuncValue a params body) in
	(c1, c2, [(a, b, newFuncDec:table)], types)

addProcDec :: String -> [Declaration] -> [Statement] -> OWLState -> OWLState
addProcDec name params body (c1, c2, [(a, b, table)], types) = let 
	newProcDec = (name, ProcType (extractParamTypes params), ProcValue a params body) in
	(c1, c2, [(a, b, newProcDec:table)], types)

---------------------------------------------------------------------------------------------------
-- Declaration info
---------------------------------------------------------------------------------------------------

getDecType :: Declaration -> VarType
getDecType (Var name t expr) = t
getDecType (Function name p ret body) = (FuncType (extractParamTypes p) ret)
getDecType (Procedure name p body) = (ProcType (extractParamTypes p))

getDecName :: Declaration -> String
getDecName (Var name t expr) = name
getDecName (Function name p ret body) = name
getDecName (Procedure name p body) = name

extractParamType :: Declaration -> VarType
extractParamType (Var _ varType _) = varType
extractParamType (Function _ params ret _) = FuncType (extractParamTypes params) ret
extractParamType (Procedure _ params _) = ProcType (extractParamTypes params)

extractParamTypes :: [Declaration] -> [VarType]
extractParamTypes [] = []
extractParamTypes (h:params) = (extractParamType h) : (extractParamTypes params)

---------------------------------------------------------------------------------------------------
-- Initial Values
---------------------------------------------------------------------------------------------------

-- Apenas o escopo global, vazio.
initState :: [UserType] -> OWLState
initState types = (1, 0, [(0, -1, [])], types)

getListUserTypes :: OWLState -> [UserType]
getListUserTypes (_, _, _, userTypes) = userTypes

getUserType :: String -> [UserType] -> UserType
getUserType name1 ((name2, decs):types) = 
	if name1 == name2 then (name2, decs) else getUserType name1 types

initEachField :: [Declaration] -> [UserType] -> [VarValue]
initEachField [] _ = []
initEachField (h:decs) types = 
	let t = getDecType h in (getInitValue t types) : initEachField decs types

getInitValue :: VarType -> [UserType] -> VarValue
getInitValue (PointerType _) _ = PointerValue ("", 0)
getInitValue (ArrayType _) _ = ArrayValue 0 []
getInitValue (FuncType _ _) _ = FuncValue (-1) [] []
getInitValue (ProcType _ ) _ = ProcValue (-1) [] []
getInitValue (AtomicType "nat") _ = NumberValue 0
getInitValue (AtomicType "int") _ = NumberValue 0
getInitValue (AtomicType "real") _ = NumberValue 0
getInitValue (AtomicType "char") _ = CharValue 'a'
getInitValue (AtomicType "bool") _ = BoolValue False
getInitValue (AtomicType name) types = do
	let (_, decs) = (getUserType name types)
	let vars = (initEachField decs types)
	(UserValue vars)