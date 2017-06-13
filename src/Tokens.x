{
module Tokens where
}

%wrapper "posn"

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
  main                                 { \p s -> newToken p Main }
  func                                 { \p s -> newToken p Func }
  proc                                 { \p s -> newToken p Proc }
  struct                               { \p s -> newToken p Struct }
  ":"                                  { \p s -> newToken p Colon }
  ";"                                  { \p s -> newToken p SemiColon }
  ","                                  { \p s -> newToken p Comma }
  "("                                  { \p s -> newToken p LParen }
  ")"                                  { \p s -> newToken p RParen }
  "{"                                  { \p s -> newToken p LBrace }
  "}"                                  { \p s -> newToken p RBrace }
  "["                                  { \p s -> newToken p LBracket }
  "]"                                  { \p s -> newToken p RBracket }
  bool                                 { \p s -> newToken p (Id s) }
  nat                                  { \p s -> newToken p (Id s) }
  int                                  { \p s -> newToken p (Id s) }
  real                                 { \p s -> newToken p (Id s) }
  char                                 { \p s -> newToken p (Id s) }
  "="                                  { \p s -> newToken p Assign}
  "/"                                  { \p s -> newToken p Divide}
  "*"                                  { \p s -> newToken p Times}
  "-"                                  { \p s -> newToken p Minus}
  "+"                                  { \p s -> newToken p Plus}
  "%"                                  { \p s -> newToken p Modulus}
  "||"                                 { \p s -> newToken p Or_cir}
  "|"                                  { \p s -> newToken p Or}
  "&&"                                 { \p s -> newToken p And_cir}
  "&"                                  { \p s -> newToken p And}
  "=="                                 { \p s -> newToken p Equals}
  "!="                                 { \p s -> newToken p Not_Equals}
  "<"                                  { \p s -> newToken p Less}
  ">"                                  { \p s -> newToken p Greater}
  "<="                                 { \p s -> newToken p LessEq}
  ">="                                 { \p s -> newToken p GreaterEq}
  "!"                                  { \p s -> newToken p Exclamation}
  "@"                                  { \p s -> newToken p At}
  "$"                                  { \p s -> newToken p Dollar}
  true                                 { \p s -> newToken p (Bool True) }
  false                                { \p s -> newToken p (Bool False) }
  if                                   { \p s -> newToken p If}
  else                                 { \p s -> newToken p Else}
  for                                  { \p s -> newToken p For}
  while                                { \p s -> newToken p While}
  switch                               { \p s -> newToken p Switch}
  case                                 { \p s -> newToken p Case}
  return                               { \p s -> newToken p Return}
  break                                { \p s -> newToken p Break}
  read                                 { \p s -> newToken p Read}
  readNat                              { \p s -> newToken p ReadNat}
  readInt                              { \p s -> newToken p ReadInt}
  readReal                             { \p s -> newToken p ReadReal}
  write                                { \p s -> newToken p Write}
  array                                { \p s -> newToken p Array}
  floor                                { \p s -> newToken p Floor}
  ceil                                 { \p s -> newToken p Ceil}
  sizeof                               { \p s -> newToken p Sizeof}
  const                                { \p s -> newToken p Const}
  new                                  { \p s -> newToken p New}
  delete                               { \p s -> newToken p Delete}
  $digit+                              { \p s -> newToken p (Nat (read s))}
  [\-]?@decimal+                       { \p s -> newToken p (Int (read s))}
  [\-]?@decimal+ \. @decimal+          { \p s -> newToken p (Real (read s))}
  "."                                  { \p s -> newToken p Dot}
  $alpha [$alpha $digit \_ \']*        { \p s -> newToken p (Id s)}
  \" @string* \"                       { \p s -> newToken p (SString (read s))}
  \' @string? \'                       { \p s -> newToken p (CChar (read s))}

{

-- Each action has type :: String -> Token

-- The token type:
data Token = Token TokenSymbol Int Int deriving (Eq,Show)

data TokenSymbol =
  Main |
  Func |
  Proc |
  Read |
  ReadNat |
  ReadInt |
  ReadReal |
  Write |
  Array |
  Floor |
  Ceil |
  Sizeof |
  Colon   |
  SemiColon |
  Comma |
  LParen |
  RParen |
  LBrace |
  RBrace |
  LBracket |
  RBracket |
  Assign    | 
  Divide    |
  Times     |
  Minus     |
  Plus      |
  Modulus    |
  Or_cir    |
  Or        |
  And_cir   |
  And       |
  Exclamation |
  Equals    |
  Not_Equals |
  Less      |
  Greater    |
  LessEq |
  GreaterEq |
  Dot |
  At |
  Dollar |
  If  |
  Else |  
  For |
  While |
  Switch |
  Case   |
  Return |
  Break  |
  Struct |
  Const |
  New |
  Delete |
  Bool Bool |
  Id String |
  Nat Double |
  Int Double |
  Real Double |
  CChar Char |
  SString String 
  deriving (Eq,Show)

newToken :: AlexPosn -> TokenSymbol -> Token
newToken (AlexPn a l c) tok = Token tok l c

getTokens :: String -> [Token]
getTokens = alexScanTokens

}