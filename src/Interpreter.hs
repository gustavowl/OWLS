module Interpreter where

import System.IO
import ProgramTree
import ProgramState
import Expr --needed for getting leaf nodes values

runProgram :: Program -> IO()
runProgram (decs, main) = do
	state <- addGlobalDecs decs initState
	i <- callMain getMainArgs main state
	print i

addGlobalDecs :: [Declaration] -> OWLState -> IO OWLState
addGlobalDecs [] state = do return state
addGlobalDecs (h:decs) state = do
	state2 <- (addDec h state)
	state3 <- addGlobalDecs decs state2
	return state3

getMainArgs :: [Expr]
getMainArgs = []
-- TODO: pegar parâmetros reais passados no terminal (não é prioridade)

callMain :: [Expr] -> Declaration -> OWLState -> IO Integer
callMain args (Function name params ret body) state1 = do
	state2 <- addParameters args params state1
	(state3, v) <- runFuncBody name body ret state2
	let ret = f v where
		f (NumberValue n) = round n
		f _ = 0
	return ret
callMain _ _ _ = do return 0

callFunction :: String -> [Expr] -> VarType -> OWLState -> IO (OWLState, VarValue)
callFunction name args retType state1 = do
	let scopeID = getScopeID name state1
	let (t, v) = getVar (name, scopeID) state1
	(parentID, params, retType, body) <- getFuncInfo name t v
	-- TODO: checar tipo de retorno
	let state2 = newScope parentID state1
	state3 <- addParameters args params state2
	runFuncBody name body retType state3 >>= return

callProcedure :: String -> [Expr] -> OWLState -> IO OWLState
callProcedure name args state1 = do
	let scopeID = getScopeID name state1
	let (t, v) = getVar (name, scopeID) state1
	(parentID, params, body) <- getProcInfo name t v
	let state2 = newScope parentID state1
	state3 <- addParameters args params state2
	runProcBody name body state3 >>= return

addParameters :: [Expr] -> [Declaration] -> OWLState -> IO OWLState
addParameters args params state = do return state 
-- TODO: adicionar variáveis para cada parâmetro e checar os tipos de cada

getFuncInfo :: String -> VarType -> VarValue -> IO (Integer, [Declaration], VarType, [Statement])
getFuncInfo name (FuncType _ retType) (FuncValue parentID params body) = do
	return (parentID, params, retType, body)
getFuncInfo name _ _ = do fail $ name ++ " is not a function."

getProcInfo :: String -> VarType -> VarValue -> IO (Integer, [Declaration], [Statement])
getProcInfo name (ProcType _) (ProcValue parentID params body) = do
	return (parentID, params, body)
getProcInfo name _ _ = do fail $ name ++ " is not a procedure."

---------------------------------------------------------------------------------------------------
-- Statements
---------------------------------------------------------------------------------------------------

data StatementResult = Finish
	| Continue
	| Return Expr
	| Break

runFuncBody :: String -> [Statement] -> VarType -> OWLState -> IO (OWLState, VarValue)
runFuncBody name [] retType state = do fail $ "Function " ++ name ++ " reached end with no return."
runFuncBody name (s:stmts) expectedType state1 = do 
	runStatement s state1 >>= f where
		f (state2, Finish) = fail $ "Function " ++ name ++ " must return a value."
		f (state2, Continue) = runFuncBody name stmts expectedType state2 >>= return
		f (state2, Return expr) = do 
			(exprType, value, state3) <- evalExpr expr state2
			if checkType exprType expectedType then 
				return (state3, value)
			else
				fail "WTF"
		f _ = fail "WTF"

runProcBody :: String -> [Statement] -> OWLState -> IO OWLState
runProcBody name [] state = do return state
runProcBody name (s:stmts) state1 = do 
	runStatement s state1 >>= f where
		f (state2, Return _) = fail $ "Procedure " ++ name ++ " must not return a value."
		f (state2, Continue) = runProcBody name stmts state2 >>= return
		f (state2, Finish) = do return state2
		f _ = fail "WTF"

runIfElseBody :: [Statement] -> OWLState -> IO (OWLState, StatementResult)
runIfElseBody [] state = do return (state, Continue)
runIfElseBody (s:stmts) state = do 
	runStatement s state >>= f where
		f (state1, Return expr) = do return (state1, Return expr)
		f (state1, Continue) = (runIfElseBody stmts state1)
		f _ = fail "WTF"

-- Statement pra interpretar -> valor esperado para o return (se houver) -> estado atual -> novo estado
runStatement :: Statement -> OWLState -> IO (OWLState, StatementResult)
-- Declarations.
runStatement (VarDec dec) state1 = do 
	state2 <- addDec dec state1
	return (state2, Continue)
runStatement (FuncDec dec) state1 = do 
	state2 <- addDec dec state1
	return (state2, Continue)
runStatement (ProcDec dec) state1 = do 
	state2 <- addDec dec state1
	return (state2, Continue)

-- Return statements.
runStatement (ProcRet) state = do
	return (state, Finish)
runStatement (FuncRet expr) state = do
	return (state, Return expr) 

runStatement (If expr ifbody elsebody) state1 = do
	(varType, varValue, state2) <- evalExpr expr state1
	if checkType varType (AtomicType "bool") then
		if varValue == BoolValue True then
			runIfElseBody ifbody state2
		else
			runIfElseBody elsebody state2
	else
		fail "Type error: expr must be bool."

runStatement (While expr body) state = do
	return (state, Continue)
runStatement (For ini expr incr body) state = do
	return (state, Continue)

-- General statements. (TODO)
runStatement (ProcCall name params) state = do
	return (state, Continue)
runStatement (WriteCall expr) state1 = do 
	(t, v, state2) <- evalExpr expr state1
	printValue v -- Ver printValue (note que falta definir como imprimir alguns tipos)
	return (state2, Continue)
{-
runStatement (Assignment name assign) state1 = do -- minha versão está dando erro de tipo
	scope <- getScopeID name state1
	(varTypeAssign, _) <- getVar (name, scope) state1
	(varType, value, state2) <- evalExpr assign state1
	-- TODO verificar tipo varType x varTypeAssign 
	state3 <- updateVar value (name, scope) state2
	return (state3, Continue)
-}
{-runStatement (Assignment name assign) state = do
	print "BEGIN ASSIGNMENT" --TODO delete this line (used for debugging)
	print name
	print assign
	print state
	let ([(a,b,[(c,d,g)])], f) = state
	(_, e, state2) <- evalExpr assign state
	print e
	--let state2 = ([(a,b,[(c,d,e)])], f)
	--print state2
	print "END ASSIGNMENT"
	return (state2, Continue)
	-}
runStatement (Assignment name assign) state = do
	return (state, Continue)	
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
	let (t, v) = getVar (name, scopeID) state
	if t == AtomicType "bool" then do
		let b = f v where
			f (BoolValue b) = b
			f _ = False
		return (b, state) 
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
		return (v2, state3)
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
	(t1, NumberValue v1, state2) <- evalExpr e1 state1
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
--evaluates addition
evalNumExpr (NumAdd node1 node2) state = do
	(val1, typ1, state1) <- evalNumExpr node1 state
	(val2, typ2, state2) <- evalNumExpr node2 state
	--TODO evaluate type for coersion
	return (val1 + val2, "nat", state)

--evaluates subtraction
evalNumExpr (NumSub node1 node2) state = do
	(val1, typ1, state1) <- evalNumExpr node1 state
	(val2, typ2, state2) <- evalNumExpr node2 state
	--TODO evaluate type for coersion
	return (val1 - val2, "nat", state)

--evaluates multiplication
evalNumExpr (NumMul node1 node2) state = do
	(val1, typ1, state1) <- evalNumExpr node1 state
	(val2, typ2, state2) <- evalNumExpr node2 state
	--TODO evaluate type for coersion
	return (val1 * val2, "nat", state)
--evaluates division TODO ask Luisa about return type of two int.
-- Always real or just entire part?
evalNumExpr (NumDiv node1 node2) state = do
	(val1, typ1, state1) <- evalNumExpr node1 state
	(val2, typ2, state2) <- evalNumExpr node2 state
	--TODO evaluate type for coersion
	return (val1 / val2, "TODO", state)
--TODO evaluate mod. Search how to mod in python

evalNumExpr node state = do 
	--typ: type; val: value represented/contained in node
	--print node
	(val, typ, state2) <- evalNumLeaf node state
	-- TODO evaluate for other expressions (recursion)
	return (val, typ, state)
--returns the value. the type of the number and the state
evalNumLeaf :: NumNode -> OWLState -> IO (Double, String, OWLState)
--Evaluates for natural numbers
evalNumLeaf (NumNat n) state = do return (n, "nat", state)
--Evaluates for integer numbers
evalNumLeaf (NumInt n) state = do return (n, "int", state)
--Evaluates for real numbers
evalNumLeaf (NumReal n) state = do return (n, "real", state)
--Evaluates for number variables
evalNumLeaf (NumID n) state = do
	print "NUMID BEGIN"
	print state
	--gets next variable
	let ([(a,b,(name,typ,val):t)],c) = state
	--verifies if variable is the desired one
	if (n == name) then do
		--found variable. Return its value
		ret <- evalNumVarLeafValue typ val state
		return ret
	else if (t /= []) then do
		--variable not found yet. Do recursion
		--let (r1, r2, ([(a2,b2,t2)],c2)) = evalNumLeaf (NumID n) ([(a,b, t)],c)
		(r1, r2, ([(a2,b2,t2)],c2)) <- evalNumLeaf (NumID n) ([(a,b, t)],c)
		--let k = evalNumLeaf (NumID n) ([(a,b, t)],c)
		--return ([(a2,b2, (name,typ,val):t2)],c2)
		--return (r1, r2, ([(a2,b2, t2)],c2))
		return (r1, r2, ([(a2,b2,(name,typ,val):t2)],c2))
		--(name,typ,val):
	else do
		--variable was not declared. Return error
		print "WTF at evalNumLeaf (NumID n) state"--TODO return error
		print "NUMID END"
		print t
		print ((name, typ, val):t)
		print "NUMID END"
		return (73.0, "nat", state) --TODO return error

{--Else case TODO whenever this message is printed, there is an expression that was not properly
evaluated-}
evalNumLeaf node state = do return (0, "só pro Haskell não frescar", state)

evalNumVarLeafValue :: VarType -> VarValue -> OWLState -> IO (Double, String, OWLState)
evalNumVarLeafValue typ (NumberValue val) state = do
	if (checkType typ (AtomicType "nat")) then do
		state2 <- evalNumLeaf (NumNat val) state
		return state2
	else if (checkType typ (AtomicType "int")) then do
		state2 <- evalNumLeaf (NumInt val) state
		return state2
	else if (checkType typ (AtomicType "real")) then do
		state2 <- evalNumLeaf (NumReal val) state
		return state2
	else
		--print "ERROR. NOT VALID TYPE AT interpreter.hs evalNumVarLeafValue"
		return (73.0, "TODO WTF", state) --TODO return ERROR
{-
evalNumVarLeafValue a b state = do
	return (-1, "WTF, DID YOU USE A POINTER?", state)-}

evalStuffExpr :: StuffNode -> OWLState -> IO (VarType, VarValue, OWLState)
evalStuffExpr node state = do
	return (AtomicType "", NumberValue 0, state) -- TODO

---------------------------------------------------------------------------------------------------
-- Declarations
---------------------------------------------------------------------------------------------------

addDec :: Declaration -> OWLState -> IO OWLState
addDec (Var name varType Nothing) state = do return $ addVarDec name varType state
addDec (Var name varType (Just e)) state1 = do 
	(t, value, state2) <- evalExpr e state1
	if checkType t varType then do -- TODO: verificar tipo t com o tipo da variavel
		let state3 = addVarDec name varType state2
		let scopeID = getScopeID name state3
		return $ updateVar value (name, scopeID) state3
	else 
		fail "Variable not compatible."
addDec (Function name params ret body) state = do
	return $ addFuncDec name params ret body state
addDec (Procedure name params body) state = do
	return $ addProcDec name params body state

---------------------------------------------------------------------------------------------------
-- Declarations
---------------------------------------------------------------------------------------------------
-- ordem dos argumentos: tipo da variável; tipo esperado da variável
checkType :: VarType -> VarType -> Bool
checkType (AtomicType "nat") (AtomicType "nat") = True
checkType (AtomicType "int") (AtomicType "int") = True
checkType (AtomicType "real") (AtomicType "real") = True
checkType (AtomicType "nat") (AtomicType "int") = True
checkType (AtomicType "int") (AtomicType "real") = True 
checkType (AtomicType "nat") (AtomicType "real") = True
checkType (AtomicType "bool") (AtomicType "bool") = True
checkType (AtomicType "char") (AtomicType "char") = True
checkType (AtomicType "char") (AtomicType "nat") = True
checkType v1 v2 = False -- TODO: inserir demais tipos

printValue :: VarValue -> IO()
printValue (BoolValue b) = do
	print b
printValue (CharValue c) = do
	print c
printValue (NumberValue d) = do
	print d
printValue v = do
	print v