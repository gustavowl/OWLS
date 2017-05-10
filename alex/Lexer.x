{
module Lexer where
}

%wrapper "basic"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-

  $white+                              ;
  "#".*                                ;
  \{                                   {\s -> LBrace}
  \}                                   {\s -> RBrace}
  \(                                   {\s -> LParen}
  \)                                   {\s -> RParen}
  :                                    {\s -> Colon}
  ";"                                  {\s -> SemiColon}
  ","                                  {\s -> Comma}
  .                                    {\s -> Dot}
  main                                 {\s -> Main}
  "="                                  {\s -> Assign}
  func                                 {\s -> Func}
  proc                                 {\s -> Proc}
  if                                   {\s -> If}
  else                                 {\s -> Else}
  then                                 {\s -> Then}
  while                                {\s -> While}
  for                                  {\s -> For}
  switch                               {\s -> Switch}
  case                                 {\s -> Case}
  "<="                                 {\s -> LtEqOp}
  "<"                                  {\s -> LtOp}
  ">="                                 {\s -> GtEqOp}
  >                                    {\s -> GtOp}
  ==                                   {\s -> EqOp}
  "+"                                  {\s -> AddOp}
  "-"                                  {\s -> SubOp}
  "*"                                  {\s -> MultOp}
  "/"                                  {\s -> DivOp}
  "%"                                  {\s -> ModOp}
  "&&"                                 {\s -> AndOp}
  "||"                                 {\s -> OrOp}
  "&"                                  {\s -> AndOpSC}
  "|"                                  {\s -> OrOpSC}
  "!="                                 {\s -> DifOp}
  nat                                  {\s -> Type s}
  int                                  {\s -> Type s}
  real                                 {\s -> Type s}
  \-? $digit+ \. $digit*               {\s -> Real (read s)}
  $digit+                              {\s -> Nat (read s)}
  \-? $digit+                          {\s -> Int (read s)}
  
  $alpha [$alpha $digit \_ \']*        {\s -> Id s }
  \" $alpha [$alpha $digit ! \_ \']* \"  {\s -> String s}

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
  LBrace         |
  RBrace         |
  LParen         |
  RParen         | 
  Colon          |
  SemiColon      |
  Comma          |
  Dot            |
  Main           | 
  Func           |
  Proc           |
  If             |
  Else           |
  Assign         |
  Then           |
  For            |
  While          |
  Switch         |
  Case           |
  LtEqOp         |
  LtOp           |
  GtEqOp         |
  GtOp           |
  EqOp           |
  DifOp          |
  AddOp          |
  SubOp          |
  AndOp          |
  MultOp         |
  DivOp          |
  ModOp          |
  OrOp           |
  AndOpSC        |
  OrOpSC         |
  Type String    |
  Id String      |
  Int Int        |
  String String  
  deriving (Eq, Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}