	module Interpreter where

import Data.Fixed
import System.IO
import ProgramTree
import ProgramState
import Types
import Data.List

runProgram :: Program -> IO()
runProgram (types, decs, main) = do
	initialState <- initState types
	state <- addGlobalDecs decs initialState
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
	let state2 = newScope 0 state1
	state3 <- addParameters args params state1 state2
	(state4, v) <- runFuncBody name body ret state3
	let ret = f v where
		f (NumberValue n) = round n
		f _ = 0
	return ret
callMain _ _ _ = do return 0

callFunction :: Key -> [Expr] -> OWLState -> IO (VarType, VarValue, OWLState)
callFunction (name, scopeID) args state1 = do
	(t, v) <- getVar (name, scopeID) state1
	(parentID, params, retType, body) <- getFuncInfo name t v
	let state2 = newScope parentID state1
	state3 <- addParameters args params state1 state2
	(state4, value) <- runFuncBody name body retType state3
	return (retType, value, popScope state4)

callProcedure :: Key -> [Expr] -> OWLState -> IO OWLState
callProcedure (name, scopeID) args state1 = do
	(t, v) <- getVar (name, scopeID) state1
	(parentID, params, body) <- getProcInfo name t v
	let state2 = newScope parentID state1
	state3 <- addParameters args params state1 state2
	state4 <- runProcBody name body state3
	return $ popScope state4

-- args, params, state of who called subprogram, state of the new subprogram
addParameters :: [Expr] -> [Declaration] -> OWLState -> OWLState -> IO OWLState
addParameters [] [] state1 state2 = do return state2
addParameters args [] s1 s2 = do 
	fail "Too many arguments."
addParameters [] params s1 s2 = do 
	fail "Too few arguments."
addParameters (a:args) (h:params) state1 newState1 = do 
	let t1 = getDecType h
	let name = getDecName h
	(t2, v, state2) <- evalExpr a state1
	convertType t1 t2
	let newState2 = addVarDec name t1 newState1
	scopeID <- getScopeID name newState2
	let newState3 = updateVar v (name, scopeID) newState2
	addParameters args params state2 newState3

getFuncInfo :: String -> VarType -> VarValue -> IO (Integer, [Declaration], VarType, [Statement])
getFuncInfo name (FuncType _ retType) (FuncValue parentID params body) = do
	return (parentID, params, retType, body)
getFuncInfo name _ _ = do fail $ name ++ " is not a function."

getProcInfo :: String -> VarType -> VarValue -> IO (Integer, [Declaration], [Statement])
getProcInfo name (ProcType _) (ProcValue parentID params body) = do
	return (parentID, params, body)
getProcInfo name _ _ = do fail $ name ++ " is not a procedure."

getFuncRetType :: Key -> OWLState -> IO VarType
getFuncRetType (name, scopeID) state = do
	(t, v) <- getVar (name, scopeID) state
	(parentID, params, retType, body) <- getFuncInfo name t v
	return retType

---------------------------------------------------------------------------------------------------
-- Statements
---------------------------------------------------------------------------------------------------

data StatementResult = Finish
	| Continue
	| Return Expr
	| BreakCall

runFuncBody :: String -> [Statement] -> VarType -> OWLState -> IO (OWLState, VarValue)
runFuncBody name [] retType state = do fail $ "Function " ++ name ++ " reached end with no return."
runFuncBody name (s:stmts) expectedType state1 = do 
	runStatement s state1 >>= f where
		f (state2, Finish) = fail $ "Function " ++ name ++ " must return a value."
		f (state2, Continue) = runFuncBody name stmts expectedType state2 >>= return
		f (state2, Return expr) = do 
			(exprType, value, state3) <- evalExpr expr state2
			convertType expectedType exprType
			return (state3, value)
		f (state2, BreakCall) = fail "Break cannot break a function."

runProcBody :: String -> [Statement] -> OWLState -> IO OWLState
runProcBody name [] state = do return state
runProcBody name (s:stmts) state1 = do 
	runStatement s state1 >>= f where
		f (state2, Return _) = fail $ "Procedure " ++ name ++ " must not return a value."
		f (state2, Continue) = runProcBody name stmts state2 >>= return
		f (state2, Finish) = do return state2
		f (state2, BreakCall) = fail "Break cannot breaking a procedure."

runIfElseBody :: [Statement] -> OWLState -> IO (OWLState, StatementResult)
runIfElseBody [] state = do return (state, Continue)
runIfElseBody (s:stmts) state = do 
	runStatement s state >>= f where
		f (state1, Return expr) = do return (state1, Return expr)
		f (state1, Continue) = (runIfElseBody stmts state1)
		f (state1, BreakCall) = do return (state1, BreakCall)
		f _ = fail "Error in block (if else)."

runLoopBody :: [Statement] -> OWLState -> IO (OWLState, StatementResult)
--when finishes running while loop; try running it again
runLoopBody [] state = do return (state, Continue) 
runLoopBody (s:stmts) state = do --executes next statement
	runStatement s state >>= f where
		f (state1, Return expr) = do return (state1, Return expr)
		f (state1, Continue) = (runLoopBody stmts state1)
		f (state1, BreakCall) = do return (state1, BreakCall)
	--TODO stop when break

runWhileEvalResult :: Expr -> [Statement] -> OWLState -> StatementResult -> IO (OWLState, StatementResult)
runWhileEvalResult expr body state Continue = (runStatement (While expr body) state) --iterate once
runWhileEvalResult _ _ state (Return expr) = do return (state, Return expr) --returns from function
runWhileEvalResult _ _ state (BreakCall) = do return (state, Continue) 
--forces loop stop, But keeps running parent block

runForEvalResult :: Expr -> Statement -> [Statement] -> OWLState -> StatementResult -> IO (OWLState, StatementResult)
runForEvalResult expr incr body state Continue = (runForIncrementation expr incr body state) 
--increments and iterates
runForEvalResult _ incr _ state (Return expr) = do return (state, Return expr) 
--returns from function
runForEvalResult _ incr _ state (BreakCall) = do return (state, Continue) 
--forces loop stop, But keeps running parent block

runForIteration :: Expr -> Statement -> [Statement] -> OWLState -> IO (OWLState, StatementResult)
runForIteration expr incr body state1 = do
	--evaluates condition
	(varType, varValue, state2) <- evalExpr expr state1
	--verifies if type is valid. convertType will throw an error otherwise
	convertType (AtomicType "bool") varType
	if varValue == BoolValue True then do
		(state3, result) <- runLoopBody body state2 --executes body
		--evaluates result to decide wheter should stop or continue iterating
		runForEvalResult expr incr body state3 result
	else do
		let state3 = popScope state2
		return (state3, Continue) --else just do nothing. will stop running

--executes incrementation of for loop
runForIncrementation :: Expr -> Statement -> [Statement] -> OWLState -> IO(OWLState, StatementResult)
runForIncrementation expr incr body state1 = do
	(state2, _) <- runStatement incr state1
	runForIteration expr incr body state2

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
	convertType (AtomicType "bool") varType
	if varValue == BoolValue True then
		runIfElseBody ifbody state2
	else
		runIfElseBody elsebody state2

runStatement (While expr body) state1 = do
	(varType, varValue, state2) <- evalExpr expr state1
	--verifies if type is valid. convertType will throw an error otherwise
	convertType (AtomicType "bool") varType
	if varValue == BoolValue True then do
		(state3, result) <- runLoopBody body state2 --executes body
		--evaluates result to decide wheter should stop or continue iterating
		runWhileEvalResult expr body state3 result
	else do
		return (state2, Continue) --else just do nothing. will stop running

--these will only be executed once
runStatement (For (Var id varType initVal) expr incr body) state1 = do
	let state2 = newScope (getCurrentScopeID state1) state1
	(state3, _) <- runStatement (VarDec (Var id varType initVal)) state2
	runForIteration expr incr body state3
runStatement (For (Function name params ret body1) expr incr body2) state1 = do
	--let state2 = newScope parentID state1
	(state2, _) <- runStatement (FuncDec (Function name params ret body1)) state1
	return (state2, Continue)
runStatement (For (Procedure name params body1) expr incr body2) state1 = do
	--let state2 = newScope parentID state1
	(state2, _) <- runStatement (ProcDec (Procedure name params body1)) state1
	return (state2, Continue)

runStatement Break state = do
	return (state, BreakCall)

-- General statements. (TODO)
runStatement (ProcCall name args) state1 = do
	scopeID <- getScopeID name state1
	state2 <- callProcedure (name, scopeID) args state1
	return (state2, Continue)

runStatement (WriteCall expr) state1 = do 
	(t, v, state2) <- evalExpr expr state1
	printValue t v 
	return (state2, Continue)

runStatement (Assignment (AssignContent ptr) expr) state1 = do 
	(t, v, state2) <- evalExpr ptr state1
	key <- getPointerValue v
	state3 <- runAssignment key (AssignVar "") expr state1
	return (state3, Continue)

runStatement (Assignment assignkey expr) state1 = do 
	-- Key da variável "raiz"
	key <- getKeyToAssign assignkey state1
	-- Tipo e valor atual da variável
	state2 <- runAssignment key assignkey expr state1
	return (state2, Continue)

printValue :: VarType -> VarValue -> IO()
printValue _ (BoolValue b) = do
	putStr (show b)
printValue _ (CharValue c) = do
	putChar c
printValue (AtomicType "real") (NumberValue d) = do
	putStr $ (show d)
printValue _ (NumberValue d) = do
	putStr $ (show (round d))
printValue t (ArrayValue l e) = do
	printValueArray t l e 
printValue _ (PointerValue (name, scope)) = do
	putStr $ "(" ++ name ++ " ,"
	putStr (show scope)
	putStr ")"
printVaue _ _ =
	print "TODO"-- TODO: imprimir outros tipos
	
printValueArray t l (e: e1) = do
	printValue t e  
	if l > 1 then
		printValueArray t (l - 1) e1
	else
		putStr "" 

---------------------------------------------------------------------------------------------------
-- Declarations
---------------------------------------------------------------------------------------------------

addDec :: Declaration -> OWLState -> IO OWLState
addDec (Var name varType Nothing) state1 = do 
	let state2 = addVarDec name varType state1
	if nullOWLState == state2 then
		fail $ "Duplicate var declaration: " ++ name
	else
		return state2
addDec (Var name varType (Just e)) state1 = do 
	(actualType, value, state2) <- evalExpr e state1
	convertType varType actualType
	let state3 = addVarDec name varType state2
	if nullOWLState == state3 then
		fail $ "Duplicate var declaration: " ++ name
	else do
		scopeID <- getScopeID name state3
		return $ updateVar value (name, scopeID) state3

addDec (Function name params ret body) state1 = do
	let state2 = addFuncDec name params ret body state1
	if nullOWLState == state2 then
		fail $ "Duplicate function declaration: " ++ name
	else
		return state2
addDec (Procedure name params body) state1 = do
	let state2 = addProcDec name params body state1
	if nullOWLState == state2 then
		fail $ "Duplicate procedure declaration: " ++ name
	else
		return state2

---------------------------------------------------------------------------------------------------
-- Evaluate Expression
---------------------------------------------------------------------------------------------------

evalBoolExpr :: Expr -> OWLState -> IO (Bool, OWLState)
evalBoolExpr expr state1 = do
	(t, v, state2) <- evalExpr expr state1
	convertType (AtomicType "bool") t
	b <- getBoolValue v
	return (b, state2) 

boolToExpr :: Bool -> OWLState -> (VarType, VarValue, OWLState)
boolToExpr b state = (AtomicType "bool", BoolValue b, state)

evalNumExpr :: Expr -> OWLState -> IO (String, Double, OWLState)
evalNumExpr expr state1 = do
	(t, v, state2) <- evalExpr expr state1
	numt <- getNumberType t
	numv <- getNumberValue v
	return (numt, numv, state2)

numToExpr :: String -> Double -> OWLState -> (VarType, VarValue, OWLState)
numToExpr typ val state = (AtomicType typ, NumberValue val, state)

stringToDouble :: String -> Double
stringToDouble x = read x :: Double

getVarValueList :: VarValue -> [VarValue]
getVarValueList (UserValue v) = v 
getVarValueList _ = []

-- Calcula o valor da expressão
evalExpr :: Expr -> OWLState -> IO (VarType, VarValue, OWLState)

---------------------------------------------------------------------------------------------------
-- Evaluate Functions
---------------------------------------------------------------------------------------------------

-- Generic funcion call.
evalExpr (FuncCall name args) state1 = do
	scopeID <- getScopeID name state1
	(t, v, state2) <- callFunction (name, scopeID) args state1
	return (t, v, state2)

-- Read call.
evalExpr (ReadCall) state = do
	line <- getLine
	let size = genericLength line
	let expr = ArrayValue size $ convertArrayCharToExpr line 
	return (AtomicType "char", expr, state) -- ADD ArrayValue

-- Read call.
evalExpr (ReadNatCall) state = do
	line <- getLine 
	let value = stringToDouble line 
	let expr = NumberValue value
	if value < 0 then
		fail  "Number not Natural"
	else
		return (AtomicType "nat", expr, state) -- ADD ArrayValue

-- Read call.
evalExpr (ReadIntCall) state = do
	line <- getLine 
	let expr = NumberValue $ stringToDouble line 
	return (AtomicType "int", expr, state) -- ADD ArrayValue

-- Read call.
evalExpr (ReadRealCall) state = do
	line <- getLine 
	let expr = NumberValue $ stringToDouble line 
	return (AtomicType "real", expr, state) -- ADD ArrayValue

-- Floor call.
evalExpr (FloorCall expr) state1 = do
	(typ, val, state2) <- evalExpr expr state1
	if canConvertType (AtomicType "int") typ then
		return (typ, val, state2)
	else do
		v <- getNumberValue val
		let newv = fromIntegral (floor v)
		return (AtomicType "int", NumberValue newv, state2)

-- Ceil call.
evalExpr (CeilCall expr) state1 = do
	(typ, val, state2) <- evalExpr expr state1
	if canConvertType (AtomicType "int") typ then
		return (typ, val, state2)
	else do
		v <- getNumberValue val
		let newv = fromIntegral (ceiling v)
		return (AtomicType "int", NumberValue newv, state2)

-- Array call.
evalExpr (ArrayCall exprs) state1 = do
	if (exprs == []) then
		fail "Array call need an initial value."
	else do
		let (h:d) = exprs
		(typ, initValue, state2) <- evalExpr h state1
		evalArray typ initValue d state2 >>= return

-- Sizeof call.
evalExpr (SizeofCall expr) state1 = do
	(t, array, state2) <- evalExpr expr state1
	let size = 0 -- TODO: pegar tamanho da array
	return (AtomicType "nat", NumberValue size, state2)

-- New call.
evalExpr (NewCall typ) (a, count, b, c) = do
	let name = show count
	let state = addVarDec name typ (a, count + 1, b, c)
	return (PointerType typ, PointerValue (name, 0), state)

---------------------------------------------------------------------------------------------------
-- Variables
---------------------------------------------------------------------------------------------------

-- Variable.
evalExpr (ID name) state = do
	scopeID <- getScopeID name state
	(t, v) <- getVar (name, scopeID) state
	return (t, v, state)

-- Pointer content.
evalExpr (Content expr) state1 = do
	(pt, pv, state2) <- evalExpr expr state1
	(name, scopeID) <- getPointerValue pv
	(t, v) <- getVar (name, scopeID) state2
	return (t, v, state2)

-- Struct field.
evalExpr (Field expr name) state1 = do 
	((AtomicType t), v, state2) <- evalExpr expr state1
	(_, decs) <- getUserType t (getListUserTypes state2) -- tipo do usuário (name, [Declaration])	
	(fieldType, fieldValue) <- getFieldValue name decs (getVarValueList v)
	return (fieldType, fieldValue, state2)

-- Array elements.
evalExpr (ArrayEl aexpr iexpr) state1 = do 
	(at, av, state2) <- evalExpr aexpr state1
	(size, array) <- getArrayValue av
	typ <- getArrayType at
	(it, iv, state3) <- evalExpr iexpr state2
	convertType (AtomicType "nat") it
	index <- getNumberValue iv
	let i = round index
	if i >= size then do
		fail $ "Index out of bounds: " ++ show i
	else do
		let el = array !! (fromInteger i)
		return (typ, el, state3)

-- Variable address.
evalExpr (Addr name) state = do
	scopeID <- getScopeID name state
	let key = (name, scopeID)
	(t, v) <- getVar key state
	return (PointerType t, PointerValue key, state)

---------------------------------------------------------------------------------------------------
-- Evaluate Literals
---------------------------------------------------------------------------------------------------

evalExpr (BoolLit b) state = do
	return (AtomicType "bool", BoolValue b, state)

evalExpr (NatLit n) state = do
	return (AtomicType "nat", NumberValue n, state)

evalExpr (IntLit n) state = do
	return (AtomicType "int", NumberValue n, state)

evalExpr (RealLit n) state = do
	return (AtomicType "real", NumberValue n, state)

evalExpr (CharLit c) state = do
	return (AtomicType "char", CharValue c, state)

evalExpr (ArrayLit els) state = do
	result <- createUntypedArrayValue els state
	return result

---------------------------------------------------------------------------------------------------
-- Evaluate Boolean Operators
---------------------------------------------------------------------------------------------------

-- Boolean NOT.
evalExpr (BoolNot expr) state1 = do
	(b, state2) <- evalBoolExpr expr state1
	return $ boolToExpr (not b) state2 

-- Boolean AND.
evalExpr (BoolOr expr1 expr2) state1 = do
	(b1, state2) <- evalBoolExpr expr1 state1
	(b2, state3) <- evalBoolExpr expr2 state2
	return $ boolToExpr (b1 || b2) state3

-- Boolean OR.
evalExpr (BoolAnd expr1 expr2) state1 = do
	(b1, state2) <- evalBoolExpr expr1 state1
	(b2, state3) <- evalBoolExpr expr2 state2
	return $ boolToExpr (b1 && b2) state3

-- Boolean short-circuit OR.
evalExpr (BoolOrC expr1 expr2) state1 = do
	(b1, state2) <- evalBoolExpr expr1 state1
	if not b1 then do
		(b2, state3) <- evalBoolExpr expr2 state2
		return $ boolToExpr b2 state3
	else do
		return $ boolToExpr True state2

-- Boolean short-circuit AND.
evalExpr (BoolAndC expr1 expr2) state1 = do
	(b1, state2) <- evalBoolExpr expr1 state1
	if b1 then do
		(b2, state3) <- evalBoolExpr expr2 state2
		return $ boolToExpr b2 state3
	else do
		return $ boolToExpr False state2

---------------------------------------------------------------------------------------------------
-- Evaluate Relationals
---------------------------------------------------------------------------------------------------

-- Equals.
evalExpr (BoolEq e1 e2) state1 = do
	(t1, v1, state2) <- evalExpr e1 state1
	(t2, v2, state3) <- evalExpr e2 state2
	if v1 == v2 then
		return $ boolToExpr True state3
	else
		return $ boolToExpr False state3

-- Differs.
evalExpr (BoolDif e1 e2) state1 = do
	(b, state2) <- evalBoolExpr (BoolEq e1 e2) state1
	return $ boolToExpr (not b) state2

-- Greater than.
evalExpr (BoolGt e1 e2) state1 = do
	(t1, v1, state2) <- evalNumExpr e1 state1
	(t2, v2, state3) <- evalNumExpr e2 state2
	if v1 > v2 then
		return $ boolToExpr True state3
	else
		return $ boolToExpr False state3

-- Greater than or equal to.
evalExpr (BoolGtEq e1 e2) state1 = do
	(t1, v1, state2) <- evalNumExpr e1 state1
	(t2, v2, state3) <- evalNumExpr e2 state2
	if v1 >= v2 then
		return $ boolToExpr True state3
	else
		return $ boolToExpr False state3

-- Less than.
evalExpr (BoolLt e1 e2) state = evalExpr (BoolGt e2 e1) state

-- Less than or equal to.
evalExpr (BoolLtEq e1 e2) state = evalExpr (BoolGtEq e2 e1) state

---------------------------------------------------------------------------------------------------
-- Evaluate Numeric Operators
---------------------------------------------------------------------------------------------------

-- Numeric negation.
evalExpr (NumMinus expr) state1 = do
	(typ, val, state2) <- evalNumExpr expr state1
	if typ == "real" then
		return $ numToExpr "real" (-val) state2 
	else
		return $ numToExpr "int" (-val) state2

-- Numeric addition.
evalExpr (NumAdd expr1 expr2) state1 = do
	(typ1, val1, state2) <- evalNumExpr expr1 state1
	(typ2, val2, state3) <- evalNumExpr expr2 state2
	if typ1 == "real" || typ2 == "real" then
		return $ numToExpr "real" (val1 + val2) state3 
	else if typ1 == "int" || typ2 == "int" then
		return $ numToExpr "int" (val1 + val2) state3
	else
		return $ numToExpr "nat" (val1 + val2) state3

-- Numeric subtraction.
evalExpr (NumSub expr1 expr2) state1 = do
	(typ1, val1, state2) <- evalNumExpr expr1 state1
	(typ2, val2, state3) <- evalNumExpr expr2 state2
	if typ1 == "real" || typ2 == "real" then
		return $ numToExpr "real" (val1 - val2) state3
	else 
		return $ numToExpr "int" (val1 - val2) state3

-- Numeric multiplication.
evalExpr (NumMul expr1 expr2) state1 = do
	(typ1, val1, state2) <- evalNumExpr expr1 state1
	(typ2, val2, state3) <- evalNumExpr expr2 state2
	if typ1 == "real" || typ2 == "real" then
		return $ numToExpr "real" (val1 * val2) state3
	else if typ1 == "int" || typ2 == "int" then
		return $ numToExpr "int" (val1 * val2) state3
	else
		return $ numToExpr "nat" (val1 * val2) state3

-- Numeric division.
evalExpr (NumDiv expr1 expr2) state1 = do
	(typ1, val1, state2) <- evalNumExpr expr1 state1
	(typ2, val2, state3) <- evalNumExpr expr2 state2
	return $ numToExpr "real" (val1 / val2) state3

-- Numeric modulus.
evalExpr (NumMod expr1 expr2) state1 = do
	(typ1, val1, state2) <- evalNumExpr expr1 state1
	(typ2, val2, state3) <- evalNumExpr expr2 state2
	if typ1 == "real" || typ2 == "real" then
		return $ numToExpr "real" (mod' val1 val2) state3
	else if typ1 == "int" || typ2 == "int" then
		return $ numToExpr "int" (mod' val1 val2) state3
	else
		return $ numToExpr "nat" (mod' val1 val2) state3

---------------------------------------------------------------------------------------------------
-- Auxiliary
---------------------------------------------------------------------------------------------------

convertArrayCharToExpr :: [Char] -> [VarValue]
convertArrayCharToExpr [] = []  
convertArrayCharToExpr (x:xs) = (CharValue x) : convertArrayCharToExpr xs 
--stringToArray (x:xs) = (CharLit x) : stringToArray xs

--printValue t e  
	--if l > 1 then
		--printValueArray t (l - 1) e1
	--else
		--putStr "" 

createUntypedArrayValue :: [Expr] -> OWLState -> IO (VarType, VarValue, OWLState)
createUntypedArrayValue [] state = do fail "Cannot create empty array."
createUntypedArrayValue (h:t) state1 = do
	(typ, expr, state2) <- evalExpr h state1
	(exprs, state3) <- createArrayValues t typ state2
	let size = 1 + (toInteger (length t))
	return (ArrayType typ, ArrayValue size (expr:exprs), state3)

createArrayValues :: [Expr] -> VarType -> OWLState -> IO ([VarValue], OWLState)
createArrayValues [] typ state = do return ([], state)
createArrayValues (h:t) typ1 state1 = do
	(typ2, expr, state2) <- evalExpr h state1
	convertType typ1 typ2
	(exprs, state3) <- createArrayValues t typ1 state2
	return (expr:exprs, state3)

evalArray :: VarType -> VarValue -> [Expr] -> OWLState -> IO (VarType, VarValue, OWLState)
evalArray initTyp initVal [] state = do return (initTyp, initVal, state)
evalArray initTyp initVal (d:dims) state1 = do
	(typ, val, state2) <- evalExpr d state1
	convertType (AtomicType "nat") typ
	size <- getNumberValue val
	let array = createArray (round size) initVal
	evalArray (ArrayType initTyp) (ArrayValue (round size) array) dims state2 >>= return

createArray :: Integer -> VarValue -> [VarValue]
createArray size value = if size == 0 then [] else value:(createArray (size - 1) value)

---------------------------------------------------------------------------------------------------
-- Assignments
---------------------------------------------------------------------------------------------------

runAssignment :: Key -> AssignKey -> Expr -> OWLState -> IO OWLState
runAssignment key assignkey expr state1 = do
		-- Valor do elemento/campo/variável.
	(actualType, value, state2) <- evalExpr expr state1
	-- Tipo e valor atual da variável
	(expectedType, currentValue) <- getVar key state2
	-- Calcular novo valor da variável.
	value <- getModifiedValue expectedType currentValue assignkey (actualType, value, state2)
	-- Modificar o valor da variável.
	return $ updateVar value key state2

getKeyToAssign :: AssignKey -> OWLState -> IO Key
getKeyToAssign (AssignVar name) state = do
	scopeID <- getScopeID name state
	return (name, scopeID)
getKeyToAssign (AssignEl arrayKey index) state = getKeyToAssign arrayKey state
getKeyToAssign (AssignField structKey field) state = getKeyToAssign structKey state

getModifiedValue :: VarType -> VarValue -> AssignKey -> (VarType, VarValue, OWLState) -> IO VarValue
getModifiedValue expectedType currentValue (AssignVar _) (actualType, value, state1) = do 
	convertType expectedType actualType
	return value

getModifiedValue exptectedType currentValue (AssignEl arrayKey indexExpr) (actualType, value, state1) = do 
	-- Calculate index.
	(indexType, indexValue, state2) <- evalExpr indexExpr state1
	convertType (AtomicType "nat") indexType
	floatIndex <- getNumberValue indexValue
	let i = round (floatIndex)
	-- Type of the element
	expectedElType <- getArrayType exptectedType
	-- List of values
	(size, currentArrayValue) <- getArrayValue currentValue

	-- Check bounds.
	if i >= size then
		fail $ "Index out of bounds: " ++ show i ++ "/" ++ show size
	else do
		-- Current element value.
		let currentElValue = currentArrayValue !! (fromInteger i)
		-- Get modified element value.
		newElValue <- getModifiedValue expectedElType currentElValue arrayKey (actualType, value, state2)
		
		-- Override in the list
		let newArrayValue = overrideArrayValue i newElValue currentArrayValue 
		return (ArrayValue size newArrayValue)

-- tipo da variável chave, valor da variável chave, assign, (tipo do valor novo, valor, state)
getModifiedValue exptectedType currentValue (AssignField structKey fieldName) (actualType, value, state1) = do 
	
	currentFields <- getFieldValues currentValue -- [VarValues]
 	userTypeName <- getUserTypeName exptectedType
	let fields = getUserTypeDecs userTypeName (getListUserTypes state1)

	let currentFieldID = getFieldID fieldName fields -- nome do campo e lista com as declarações dos campos
	if currentFieldID < 0 then
		fail $ "There is no field name " ++ fieldName
	else do
		currentFieldValue <- getField currentFieldID currentFields -- pega o campo na posição i na variável atual
		currentFieldType <-  getFieldType currentFieldID fields

		newFieldValue <- getModifiedValue currentFieldType currentFieldValue structKey (actualType, value, state1)

		let newStructValue = overrideArrayValue currentFieldID newFieldValue currentFields
		return (UserValue newStructValue)

isAssignVar :: AssignKey -> Bool
isAssignVar (AssignVar a) = True
isAssignVar _ = False

getFieldID :: String -> [Declaration] -> Integer
getFieldID name [] = -1
getFieldID name (d:decs) = do
	let current = getDecName d
	if current == name then
		0
	else
		((getFieldID name decs) + 1)

getFieldType :: Integer -> [Declaration] -> IO VarType
getFieldType pos [] = fail $ "There is no field in current pos."
getFieldType 0 (d:decs) = do return (getDecType d)
getFieldType pos (d:decs) = getFieldType (pos - 1) decs

getField :: Integer -> [VarValue] -> IO VarValue
getField i [] = fail "WTF"
getField 0 (v:vars) = do return v
getField i (v:vars) = getField (i-1) vars

overrideArrayValue :: Integer -> VarValue -> [VarValue] -> [VarValue]
overrideArrayValue i v [] = []
overrideArrayValue i v (h:t) = if i == 0 then (v:t) else h:(overrideArrayValue (i - 1) v t)

getFieldValue :: String -> [Declaration] -> [VarValue] -> IO (VarType, VarValue)
getFieldValue name [] [] = do fail $ "There is no name field " ++ name
getFieldValue name1 (d:decs) (v:varValues) = do 
	let name2 = getDecName d
	let t = getDecType d
	if name1 == name2 then do
		return (t, v)
	else do
		getFieldValue name1 decs varValues
