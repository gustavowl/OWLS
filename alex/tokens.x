{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

$whitechar = [ \t\n\r\f\v]
$special   = [\(\)\,\;\[\]\`\{\}]

$ascdigit  = 0-9
$unidigit  = [] -- TODO
$digit     = [$ascdigit $unidigit]

$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol = [] -- TODO
$symbol    = [$ascsymbol $unisymbol] # [$special \_\:\"\']
$large     = [A-Z \xc0-\xd6 \xd8-\xde]
$small     = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha     = [$small $large]
$graphic   = [$small $large $symbol $digit $special \:\"\']
$octit     = 0-7
$hexit     = [0-9 A-F a-f]
$idchar    = [$alpha $digit \']
$symchar   = [$symbol \:]
$nl        = [\n\r]
@reservedid = 
  as|case|class|data|default|deriving|do|else|hiding|if|
  import|in|infix|infixl|infixr|instance|let|module|newtype|
  of|qualified|then|type|where
@reservedop =
  ".." | ":" | "::" | "=" | \\ | "|" | "<-" | "->" | "@" | "~" | "=>"
@varid  = $small $idchar*
@conid  = $large $idchar*
@varsym = $symbol $symchar*
@consym = \: $symchar*
@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+] @decimal
$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
   | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
   | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
   | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | @decimal | o @octal | x @hexadecimal)
@gap     = \\ $whitechar+ \\
@string  = $graphic # [\"\\] | " " | @escape | @gap

tokens :-

  $white+                              ;
  "#".*                                ; -- comentario de linha
  main                                 { \s -> Main }
  func                                 { \s -> Func }
  proc                                 { \s -> Proc }
  var                                  { \s -> Var }
  ":"                                  { \s -> Colon}
  ";"                                  { \s -> SemiColon}
  "("                                  { \s -> LParen}
  ")"                                  { \s -> RParen}
  "{"                                  { \s -> LBrace}
  "}"                                  { \s -> RBrace}
  "="                                  { \s -> Assign}
  "/"                                  { \s -> DivOp}
  "*"                                  { \s -> MultOp}
  "-"                                  { \s -> SubOp}
  "+"                                  { \s -> AddOp}
  "%"                                  { \s -> ModOp}
  "||"                                 { \s -> OrOp}
  "|"                                  { \s -> OrOpSC}
  "&&"                                 { \s -> AndOp}
  "&"                                  { \s -> AndOpSC}
  "=="                                 { \s -> EqOp}
  "!="                                 { \s -> DiffOp}
  "<"                                  { \s -> LtOp}
  ">"                                  { \s -> GtOp}
  "<="                                 { \s -> LtEqOp}
  ">="                                 { \s -> GtEqOp}
  if                                   { \s -> If}
  else                                 { \s -> Else}
  for                                  { \s -> For}
  while                                { \s -> While}
  switch                               {\s -> Switch}
  case                                 {\s -> Case}
  return                               { \s -> Return}
  break                                { \s -> Break}
  int                                  { \s -> Type s}
  nat                                  { \s -> Type s}
  real                                 { \s -> Type s}
  char                                 { \s -> Type s}
  print                                { \s -> Write}
  scan                                 { \s -> Get}
  $digit+                              { \s -> Nat (read s) }
  [\-]?@decimal+                       { \s -> Int (read s) }
  [\-]?@decimal+ \. @decimal+          { \s -> Real (read s)}
  $alpha [$alpha $digit \_ \']*        { \s -> Id s }
  \" @string* \"               { \s -> String s}
  \' @string?  \'                      { \s -> Char s}




{

-- Each action has type :: String -> Token

-- The token type:
data Token =
  Main |
  Func |
  Proc |
  Var     |
  Colon   |
  SemiColon |
  LParen |
  RParen |
  LBrace |
  RBrace |
  Assign    | 
  DivOp     |
  MultOp    |
  SubOp     |
  AddOp     |
  ModOp     |
  OrOp      |
  OrOpSC    |
  AndOp     |
  AndOpSC   |
  EqOp      |
  DiffOp    |
  LtOp      |
  GtOp      |
  LtEqOp    |
  GtEqOp    |
  If        |
  Else      |  
  For       |
  While     |
  Switch    |
  Case      | 
  Return    |
  Break     |
  Then      |
  Write     |
  Get       |
  Type String |
  Id String |
  Nat Int |
  Int Int |
  Real Double |
  Char String |
  String String
  deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
