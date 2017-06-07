module Interpreter where

import System.IO
import ProgramTree
import ProgramState

runProgram :: Program -> IO()
runProgram (decs, main) = do
	state <- addGlobalDecs decs initState
	nextState <- callFunction "main" getMainParams (AtomicType "int") state 
	print ""

addGlobalDecs :: [Declaration] -> OWLState -> IO OWLState
addGlobalDecs [] state = do return state
addGlobalDecs (h:decs) state = do
	state2 <- (addDec h state)
	state3 <- addGlobalDecs decs state2
	return state3

getMainParams :: [Expr]
getMainParams = [] 
-- TODO: pegar parâmetros reais passados no terminal (não é prioridade)

callFunction :: String -> [Expr] -> VarType -> OWLState -> IO (OWLState, VarValue)
callFunction name params expectedType state = do 
	return (state, NumberValue 0)
	-- TODO:
	-- 1) Pegar informações da função na tabela de símbolos a partir do nome dela
	-- 2) Pegar o parentID dessa função
	-- 3) Adicionar novo escopo na pilha de escopos, passando o parentID como parâmetro

-- Statement pra interpretar -> valor esperado para o return (se houver) -> estatdo atual -> novo estado
runStatement :: Statement -> Maybe VarType -> OWLState -> IO (OWLState, Maybe VarValue)
runStatement (VarDec dec) _ state = do 
	bla <- addDec dec state
	return (bla, Nothing)
runStatement (WriteCall expr) _ state = do 
	return (state, Nothing)
runStatement (Return expr) Nothing state = do 
	return (state, Nothing) -- TODO: é pra dar erro
runStatement (Return expr) (Just t) state = do 
	return (state, Nothing) -- TODO: é pra calcular a expr, comparar o tipo resultante dela com o t e retornar o valor dela no lugar no Nothing
runStatement stmt t state = do 
	return (state, Nothing) -- TODO: definir o que fazer para cada tipo de stmt definido no ProgramTree

---------------------------------------------------------------------------------------------------
-- Valuate Expression
---------------------------------------------------------------------------------------------------

-- Calcula o valor da expressão
evalExpr :: Expr -> OWLState -> IO (VarType, VarValue, OWLState)
evalExpr (BoolExpr node) state1 = do
	(b, state2) <- evalBoolExpr node state1
	return (AtomicType "bool", BoolValue b, state2)

evalExpr (NumExpr node) state1 = do
	(n, s, state2) <- evalNumExpr node state1
	return (AtomicType s, NumberValue n, state2)

evalExpr (StuffExpr node) state = evalStuffExpr node state

evalBoolExpr :: BoolNode -> OWLState -> IO (Bool, OWLState)
evalBoolExpr (BoolLit b) state = do return (b, state)
evalBoolExpr (BoolID name) state = do
	let scopeID = getScopeID name state
	let (t, BoolValue v) = getVar (name, scopeID) state -- ????
	if t == AtomicType "bool" then
		return (v, state)
	else
		fail $ "Variable " ++ name ++ " is not a boolean."

evalBoolExpr (BoolFuncCall name args) state = do return (False, state) -- TODO
evalBoolExpr (BoolNot node) state1 = do
	(v, state2) <- evalBoolExpr node state1
	return (not v, state2)

evalBoolExpr (BoolAnd node1 node2) state1 = do
	(v1, state2) <- evalBoolExpr node1 state1
	(v2, state3) <- evalBoolExpr node2 state2
	return (v1 && v2, state3)

evalBoolExpr (BoolOr node1 node2) state1 = do
	(v1, state2) <- evalBoolExpr node1 state1
	(v2, state3) <- evalBoolExpr node2 state2
	return (v1 || v2, state3)

evalBoolExpr (BoolAndC node1 node2) state1 = do
	(v1, state2) <- evalBoolExpr node1 state1
	if v1 then do
		(v2, state3) <- evalBoolExpr node2 state2
		return (True, state3)
	else do
		return (False, state2)

evalBoolExpr (BoolOrC node1 node2) state1 = do
	(v1, state2) <- evalBoolExpr node1 state1
	if not v1 then do
		(v2, state3) <- evalBoolExpr node2 state2
		return (v2, state3)
	else do
		return (True, state2)

evalBoolExpr (BoolEq e1 e2) state1 = do
	(t1, v1, state2) <- evalExpr e1 state1
	(t2, v2, state3) <- evalExpr e2 state2
	if t1 == t2 && v1 == v2 then
		return (True, state3)
	else
		return (False, state3)

evalBoolExpr (BoolDif e1 e2) state1 = do
	(b, state2) <- evalBoolExpr (BoolEq e1 e2) state1
	return (not b, state2)

evalBoolExpr (BoolGt e1 e2) state1 = do
	(t1, NumberValue v1, state2) <- evalExpr e1 state1 -- TODO 
	(t2, NumberValue v2, state3) <- evalExpr e2 state2
	if v1 > v2 then
		return (True, state3)
	else
		return (False, state3)

evalBoolExpr (BoolGtEq e1 e2) state1 = do
	(t1, NumberValue v1, state2) <- evalExpr e1 state1
	(t2, NumberValue v2, state3) <- evalExpr e2 state2
	if v1 >= v2 then
		return (True, state3)
	else
		return (False, state3)

evalBoolExpr (BoolLt e1 e2) state = evalBoolExpr (BoolGt e2 e1) state
evalBoolExpr (BoolLtEq e1 e2) state = evalBoolExpr (BoolGtEq e2 e1) state

evalNumExpr :: NumNode -> OWLState -> IO (Double, String, OWLState)
evalNumExpr node state = do
	return (0, "", state) -- TODO

evalStuffExpr :: StuffNode -> OWLState -> IO(VarType, VarValue, OWLState)
evalStuffExpr node state = do
	return (AtomicType "", NumberValue 0, state) -- TODO

---------------------------------------------------------------------------------------------------
-- Declarations
---------------------------------------------------------------------------------------------------

addDec :: Declaration -> OWLState -> IO OWLState
addDec (Var name varType Nothing) state = do return $ addVarDec name varType state
addDec (Var name varType (Just e)) state1 = do 
	(t, value, state2) <- evalExpr e state1
	-- TODO: verificar tipo t com o tipo da variavel
	let state3 = addVarDec name varType state2
	let scopeID = getScopeID name state3
	return $ updateVar value (name, scopeID) state3
addDec _ state = do return state -- TODO: Function e Procedure