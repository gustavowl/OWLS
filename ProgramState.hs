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
	| FuncValue Integer [Statement]
	| ProcValue Integer [Statement]
	deriving (Eq,Show)

-- Name + scope ID
type Key = (String, Integer)

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

getScope :: Integer -> OWLState -> Maybe Scope
getScope scopeID (stack, _) = f (popToScope scopeID stack) where
	f [] = Nothing
	f (h:t) = Just h

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
-- Declarations
---------------------------------------------------------------------------------------------------

addVarDec :: Declaration -> OWLState -> OWLState
addVarDec (Var name t Nothing) state = state -- TODO
addVarDec (Var name t (Just v)) state = state -- TODO
addVarDec _ state = state -- WTF

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
getInitValue (FuncType _ _) = FuncValue (-1) []
getInitValue (ProcType _) = ProcValue (-1) []