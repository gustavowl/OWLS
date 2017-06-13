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

nullOWLState :: OWLState
nullOWLState = (-1, -1, [], [])

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
	let id = searchVarScope name nullVarType stack --TODO change this
	if id == -1 then do
		--print stack
		fail $ "Variable " ++ name ++ " not found."
	else
		return id
		
searchVarScope :: String -> VarType -> [Scope] -> Integer
searchVarScope name _ [] = -1
searchVarScope name varType ((currentID, parentID, table):scopes) =
	if isInScope name varType table then
		currentID
	else
		searchVarScope name varType (popToScope parentID scopes)

popToScope :: Integer -> [Scope] -> [Scope]
popToScope scopeID [] = []
popToScope scopeID ((currentID, parentID, table):scopes) =
	if currentID == scopeID then
		(currentID, parentID, table):scopes
	else
		popToScope scopeID scopes

isInScope :: String -> VarType -> [TableEntry] -> Bool
isInScope name _ [] = False
isInScope name nullVarType tableEntry =
	--only verifies scope accordingly to name. It does not care about
	--the type. Used by getScopeID
	verifyScopeNameRec name nullVarType tableEntry
isInScope name (FuncType a b) ((varName, (FuncType c d), e):t) =
	verifyScopeNameRec name (FuncType a b) ((varName, (FuncType c d), e):t)

isInScope name (FuncType a b) ((varName, _, _):t) = 
	isInScope name (FuncType a b) t

isInScope name (ProcType a) ((varName, (ProcType b), c):t) = 
	verifyScopeNameRec name (ProcType a) ((varName, (ProcType b), c):t)

isInScope name (ProcType a) ((varName, _, _):t) = 
	isInScope name (ProcType a) t

isInScope name varType ((varName, (FuncType _ _), _):t) = 
	isInScope name varType t

isInScope name varType ((varName, (ProcType _), _):t) = 
	isInScope name varType t

isInScope name varType ((varName, a, b):t) = 
	--verifies arrays, pointers and variables (Atomic)
	verifyScopeNameRec name varType ((varName, a, b):t)

verifyScopeNameRec :: String -> VarType -> [TableEntry] -> Bool
verifyScopeNameRec name varType ((varName, _, _):t) = 
	if name == varName then
		True
	else
		isInScope name varType t

getCurrentScopeID :: OWLState -> Integer
getCurrentScopeID (c1, c2, (current, _, _):t, types) = current

popScope :: OWLState -> OWLState
popScope (c1, c2, h:t, types) = (c1, c2, t, types)
popScope a = a -- Should not happen.

---------------------------------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------------------------------

addVarDec :: String -> VarType -> OWLState -> OWLState
addVarDec name varType (c1, c2, [], types) = (c1, c2, [], types)
addVarDec name varType (c1, c2, (a, b, table):scopes, types) = do
	if not(isInScope name varType table) then do
		let	newElement = (name, varType, (getInitValue varType types))
		(c1, c2, (a, b, newElement:table):scopes, types)
	else
		nullOWLState --caller should throw an error

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
addFuncDec name params ret body (c1, c2, [(a, b, table)], types) =
	if not(isInScope name (FuncType [] nullVarType) table) then 
		let newFuncDec = (name, FuncType (extractParamTypes params) ret, FuncValue a params body) in
		(c1, c2, [(a, b, newFuncDec:table)], types)
	else
		nullOWLState

addProcDec :: String -> [Declaration] -> [Statement] -> OWLState -> OWLState
addProcDec name params body (c1, c2, [(a, b, table)], types) = 
	if not(isInScope name (ProcType []) table) then
		let newProcDec = (name, ProcType (extractParamTypes params), ProcValue a params body) in
		(c1, c2, [(a, b, newProcDec:table)], types)
	else
		nullOWLState

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
initState :: [UserType] -> IO OWLState
initState types = do 
	verifyTypes types
	return (1, 0, [(0, -1, [])], types)

verifyType :: UserType -> [UserType] -> IO Bool
verifyType _ [] = do return True
verifyType (name1, decs) ((name2,_):userTypes) = do
	if name1 == name2 then fail "UserType declaration invalid." else verifyType (name1, decs) userTypes

verifyDecType :: Declaration -> [Declaration] -> IO Bool
verifyDecType _ [] = do return True
verifyDecType dec (d:decs) = do
	let name1 = getDecName dec
	let name2 = getDecName d
	if name1 == name2 then fail "UserType declaration invalid." else verifyDecType dec decs

verifyTypeDecs :: UserType -> IO Bool
verifyTypeDecs (_,[]) = do return True
verifyTypeDecs (name, (d:decs)) = do 
	verifyDecType d decs 
	verifyTypeDecs (name, decs)

verifyTypes :: [UserType] -> IO Bool 
verifyTypes [] = do return True
verifyTypes (u:userTypes) = do
	verifyType u userTypes
	verifyTypeDecs u  
	verifyTypes userTypes

getListUserTypes :: OWLState -> [UserType]
getListUserTypes (_, _, _, userTypes) = userTypes

-- Recebe a lista de todos os tipos de usuário atuais e retorna o tipo do usário com o nome dado
getUserType :: String -> [UserType] -> IO UserType
getUserType name [] = fail $ name ++ " is not a user type." 
getUserType name1 ((name2, decs):types) = do
	if name1 == name2 then 
		return (name2, decs) 
	else 
		getUserType name1 types

getUserTypeDecs :: String -> [UserType] -> [Declaration]
getUserTypeDecs name [] = []
getUserTypeDecs name1 ((name2, decs):types) =
	if name1 == name2 then
		decs 
	else do
		getUserTypeDecs name1 types

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
getInitValue (AtomicType "NULL") _ = nullVarValue
getInitValue (AtomicType name) types = do
	let decs = (getUserTypeDecs name types)
	let vars = (initEachField decs types)
	(UserValue vars)