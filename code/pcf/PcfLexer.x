{
module PcfLexer 
       ( Token (..) ,
         PcfPosn ,
         pcfLexing ,
	 getTokenPosn ,
	 pcfPosnToString ,
       ) where
}

%wrapper "posn" 

$digit = 0-9
$letter = [a-zA-Z]

@ident = [a-z_][a-zA-Z0-9'_]*
@identcap = [A-Z][a-zA-Z0-9'_]*
@number = [0-9][0-9]*

tokens :-

$white+		;
"--".*		;
"->"		{ \p s -> ARROW (convertPosition p s) }
"=="		{ \p s -> EQUAL (convertPosition p s) }
\(		{ \p s -> LPAREN (convertPosition p s) }
\)		{ \p s -> RPAREN (convertPosition p s) }
\<		{ \p s -> LPAIR (convertPosition p s) }
\> 		{ \p s -> RPAIR (convertPosition p s) }
\[		{ \p s -> LBOX (convertPosition p s) }
\]		{ \p s -> RBOX (convertPosition p s) }
\:		{ \p s -> COLON (convertPosition p s) }
\;		{ \p s -> SEMICOLON (convertPosition p s) }
\,		{ \p s -> COMMA (convertPosition p s) }
\*		{ \p s -> STAR (convertPosition p s) }
\+		{ \p s -> PLUS (convertPosition p s) }
"Nat"		{ \p s -> NAT (convertPosition p s) }
"Bool"		{ \p s -> BOOL (convertPosition p s) }
"True" 		{ \p s -> TRUE (convertPosition p s) }
"False" 	{ \p s -> FALSE (convertPosition p s) }
"if"		{ \p s -> IF (convertPosition p s) }
"then"		{ \p s -> THEN (convertPosition p s) }
"else"		{ \p s -> ELSE (convertPosition p s) }
"func"		{ \p s -> FUNC (convertPosition p s) }
"projfst"	{ \p s -> PROJFST (convertPosition p s) }
"projsnd" 	{ \p s -> PROJSND (convertPosition p s) }
"injfst"	{ \p s -> INJFST (convertPosition p s) }
"injsnd"	{ \p s -> INJSND (convertPosition p s) }
"case" 		{ \p s -> CASE (convertPosition p s) }
"of"		{ \p s -> OF (convertPosition p s) }
"in" 		{ \p s -> IN (convertPosition p s) }
"fix" 		{ \p s -> FIX (convertPosition p s) }
@ident 		{ \p s -> IDENT (convertPosition p s) s }
@identcap 	{ \p s -> IDENTCAP (convertPosition p s) s }
@number		{ \p s -> NUMBER (convertPosition p s) (read s :: Int) }

{
type PcfPosn = ((Int, Int), (Int, Int)) 

pcfPosnToString :: PcfPosn -> String 
pcfPosnToString ((line1, col1), (line2, col2)) =
  if line1 == line2 
  then "[ " ++ Prelude.show line1 ++ ": " ++ Prelude.show col1 ++ "--" ++ Prelude.show col2 ++ " ]"
  else "[ " ++ Prelude.show line1 ++ ":" ++ Prelude.show col1 ++ " -- " ++ Prelude.show line2 ++ ":" ++ Prelude.show col2 ++ " ]"

data Token = ARROW PcfPosn
     	   | LPAREN PcfPosn
	   | RPAREN PcfPosn
	   | LPAIR PcfPosn
	   | RPAIR PcfPosn
	   | LBOX PcfPosn
	   | RBOX PcfPosn
	   | IDENT  PcfPosn String
	   | IDENTCAP PcfPosn String
	   | NUMBER PcfPosn Int
	   | NAT PcfPosn
	   | BOOL PcfPosn
	   | TRUE PcfPosn
	   | FALSE PcfPosn
	   | IF PcfPosn
	   | THEN PcfPosn
	   | ELSE PcfPosn
	   | FUNC PcfPosn
	   | PROJFST PcfPosn
	   | PROJSND PcfPosn
	   | INJFST PcfPosn
	   | INJSND PcfPosn
	   | CASE PcfPosn
	   | OF PcfPosn	   
	   | IN PcfPosn
	   | FIX PcfPosn  
	   | COLON PcfPosn
	   | SEMICOLON PcfPosn
	   | COMMA PcfPosn
	   | STAR PcfPosn
	   | PLUS PcfPosn
	   | EQUAL PcfPosn
	   | LEXERROR PcfPosn
	   | FCSTART
	   | FCEND
	   deriving  (Eq)

convertPosition :: AlexPosn -> String -> PcfPosn
convertPosition (AlexPn _ line col) s = ((line, col), (line, col + length s))

getTokenPosn :: Token -> PcfPosn
getTokenPosn (ARROW p) = p
getTokenPosn (LPAREN p) = p
getTokenPosn (RPAREN p) = p
getTokenPosn (LPAIR p) = p
getTokenPosn (RPAIR p) = p
getTokenPosn (LBOX p) = p
getTokenPosn (RBOX p) = p
getTokenPosn (IDENT p _) = p 
getTokenPosn (IDENTCAP p _) = p 
getTokenPosn (NUMBER p _) = p 
getTokenPosn (NAT p) = p
getTokenPosn (BOOL p) = p
getTokenPosn (TRUE p) = p
getTokenPosn (FALSE p) = p
getTokenPosn (IF p) = p
getTokenPosn (THEN p) = p
getTokenPosn (ELSE p) = p
getTokenPosn (FUNC p) = p
getTokenPosn (PROJFST p) = p
getTokenPosn (PROJSND p) = p
getTokenPosn (INJFST p) = p
getTokenPosn (INJSND p) = p
getTokenPosn (CASE p) = p
getTokenPosn (OF p) = p
getTokenPosn (IN p) = p
getTokenPosn (FIX p) = p
getTokenPosn (COLON p) = p
getTokenPosn (SEMICOLON p) = p
getTokenPosn (COMMA p) = p
getTokenPosn (STAR p) = p
getTokenPosn (PLUS p) = p
getTokenPosn (EQUAL p) = p
getTokenPosn (LEXERROR p) = p
getTokenPosn FCSTART = ((-1, -1), (0, 0))
getTokenPosn FCEND = ((-1, -1), (-1, -1))

instance Show (Token) where 	
  show (ARROW _) = "ARROW"
  show (LPAREN _) = "LPAREN"
  show (RPAREN _) = "RPAREN"
  show (LPAIR _) = "LPAIR"
  show (RPAIR _) = "RPAIR"
  show (LBOX _) = "LBOX"
  show (RBOX _) = "RBOX"
  show (IDENT _ s) = "ID:" ++ s
  show (IDENTCAP _ s) = "ID:" ++ s 
  show (NUMBER _ n) = "NUM:" ++ show n 
  show (NAT _) = "NAT"
  show (BOOL _) = "BOOL"
  show (TRUE _) = "TRUE"
  show (FALSE _) = "FALSE"
  show (IF _) = "IF"
  show (THEN _) = "THEN"
  show (ELSE _) = "ELSE"
  show (FUNC _) = "FUNC"
  show (PROJFST _) = "PROJFST"
  show (PROJSND _) = "PROJSND"
  show (INJFST _) = "INJFST"
  show (INJSND _) = "INJSND"
  show (CASE _) = "CASE"
  show (OF _) = "OF"
  show (IN _) = "IN"
  show (FIX _) = "FIX"
  show (COLON _) = "COLON"
  show (SEMICOLON _) = "SEMICOLON"
  show (COMMA _) = "COMMA"
  show (STAR _) = "STAR"
  show (PLUS _) = "PLUS"
  show (EQUAL _) = "EQUAL"
  show (LEXERROR _) = "LEXERROR"
  show FCSTART = "FCSTART"
  show FCEND = "FCEND"


pcfLexing :: String -> [Token]
pcfLexing s = alexScanTokens s
}