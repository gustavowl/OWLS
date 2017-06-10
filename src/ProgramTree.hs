module ProgramTree where

import Data.Functor.Identity
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Show.Functions
import Tokens

-- (Lista de tipos/structs, Lista de declarações de variáveis/funções/procedimentos, main)
type Program = ([UserType], [Declaration], Declaration)

data Declaration = Var String VarType (Maybe Expr)
	| Function String [Declaration] VarType [Statement]
	| Procedure String [Declaration] [Statement]
	deriving (Eq,Show)

type UserType = (String, [Declaration])

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

data Expr = BoolLit Bool
	| NatLit Double
	| IntLit Double
	| RealLit Double
	| CharLit Char
	| ArrayLit [Expr]
	| ID String
	| ArrayEl Expr Expr
	| FuncCall String [Expr]
	| ReadCall
	| Field Expr String
	| Ptr Expr
	| BoolNot Expr
	| BoolAnd Expr Expr
	| BoolOr Expr Expr
	| BoolAndC Expr Expr
	| BoolOrC Expr Expr
	| BoolEq Expr Expr
	| BoolDif Expr Expr
	| BoolGt Expr Expr
	| BoolGtEq Expr Expr
	| BoolLt Expr Expr
	| BoolLtEq Expr Expr
	| NumMinus Expr
	| NumAdd Expr Expr
	| NumSub Expr Expr
	| NumMul Expr Expr
	| NumDiv Expr Expr
	| NumMod Expr Expr
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
