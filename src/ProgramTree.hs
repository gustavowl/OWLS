module ProgramTree where

import Data.Functor.Identity
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Show.Functions
import Tokens

-- (Lista de declarações de variáveis/funções/procedimentos, main)
type Program = ([Declaration], Declaration)

data Declaration = Var String VarType (Maybe Expr)
	| Function String [Declaration] VarType [Statement]
	| Procedure String [Declaration] [Statement]
	deriving (Eq,Show)

data Statement = VarDec Declaration
	| FuncDec Declaration
	| ProcDec Declaration
	| FuncRet Expr
	| ProcRet
	| ProcCall String [Expr]
	| WriteCall Expr
	| Assignment String Expr
	| If Expr [Statement] [Statement]
	| While Expr [Statement]
	| For Declaration Expr Statement [Statement]
	| Break
	deriving (Eq,Show)
	-- TODO: mais coisa

data Expr = BoolExpr BoolNode 
	| NumExpr NumNode 
	| StuffExpr StuffNode
	deriving (Eq,Show)

data BoolNode = BoolLit Bool
	| BoolID String 
	| BoolEl StuffNode NumNode
	| BoolFuncCall String [Expr] 
	| BoolNot BoolNode
	| BoolAnd BoolNode BoolNode
	| BoolOr BoolNode BoolNode
	| BoolAndC BoolNode BoolNode
	| BoolOrC BoolNode BoolNode
	| BoolEq Expr Expr
	| BoolDif Expr Expr
	| BoolGt Expr Expr
	| BoolGtEq Expr Expr
	| BoolLt Expr Expr
	| BoolLtEq Expr Expr
	deriving (Eq,Show)

data NumNode = NumNat Double
	| NumInt Double
	| NumReal Double
	| NumID String
	| NumEl StuffNode NumNode
	| NumFuncCall String [Expr]
	| NumMinus NumNode
	| NumAdd NumNode NumNode
	| NumSub NumNode NumNode
	| NumMul NumNode NumNode
	| NumDiv NumNode NumNode
	| NumMod NumNode NumNode
	deriving (Eq,Show)

data StuffNode = StuffID String
	| StuffEl StuffNode NumNode
	| StuffChar Char
	| StuffArray [Expr]
	| StuffFuncCall String [Expr]
	| StuffReadCall Expr
	deriving (Eq,Show)

data VarType = AtomicType String 
	| ArrayType VarType Expr
	| PointerType VarType
	| FuncType [VarType] VarType -- params, retorno
	| ProcType [VarType]
	deriving (Eq,Show)

---------------------------------------------------------------------------------------------------
-- Parser Types
---------------------------------------------------------------------------------------------------
type OWLParser = ParsecT [Token] () Identity
