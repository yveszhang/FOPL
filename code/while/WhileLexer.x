{
module WhileLexer 
       ( Token (..) 
       , ZPosn
       , whileLexing
       , getTokenPosn 
       , zPosnToString 
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
"!="		{ \p s -> NOTEQ (convertPosition p s) }
"<="		{ \p s -> LEQ (convertPosition p s) }
">="		{ \p s -> GEQ (convertPosition p s) }
"<"		{ \p s -> LTHAN (convertPosition p s) }
">"		{ \p s -> GTHAN (convertPosition p s) }
":="		{ \p s -> ASSIGN (convertPosition p s) }
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
"while" 	{ \p s -> WHILE (convertPosition p s) }
"do" 		{ \p s -> DO (convertPosition p s) }
"end" 		{ \p s -> END (convertPosition p s) }
"return" 	{ \p s -> RETURN (convertPosition p s) }
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
type ZPosn = ((Int, Int), (Int, Int)) 

zPosnToString :: ZPosn -> String 
zPosnToString ((line1, col1), (line2, col2)) =
  if line1 == line2 
  then "[ " ++ Prelude.show line1 ++ ": " ++ Prelude.show col1 ++ "--" ++ Prelude.show col2 ++ " ]"
  else "[ " ++ Prelude.show line1 ++ ":" ++ Prelude.show col1 ++ " -- " ++ Prelude.show line2 ++ ":" ++ Prelude.show col2 ++ " ]"

data Token = ARROW ZPosn
     	   | LPAREN ZPosn
	   | RPAREN ZPosn
	   | LPAIR ZPosn
	   | RPAIR ZPosn
	   | LBOX ZPosn
	   | RBOX ZPosn
	   | IDENT  ZPosn String
	   | IDENTCAP ZPosn String
	   | NUMBER ZPosn Int
	   | NAT ZPosn
	   | BOOL ZPosn
	   | TRUE ZPosn
	   | FALSE ZPosn
	   | IF ZPosn
	   | THEN ZPosn
	   | ELSE ZPosn
	   | ASSIGN ZPosn
	   | WHILE ZPosn
	   | DO ZPosn 
	   | END ZPosn
	   | RETURN ZPosn	   
	   | FUNC ZPosn
	   | PROJFST ZPosn
	   | PROJSND ZPosn
	   | INJFST ZPosn
	   | INJSND ZPosn
	   | CASE ZPosn
	   | OF ZPosn	   
	   | IN ZPosn
	   | FIX ZPosn  
	   | COLON ZPosn
	   | SEMICOLON ZPosn
	   | COMMA ZPosn
	   | STAR ZPosn
	   | PLUS ZPosn
	   | EQUAL ZPosn
	   | NOTEQ ZPosn	   
	   | LEQ ZPosn	   
	   | GEQ ZPosn	   
	   | LTHAN ZPosn	   
	   | GTHAN ZPosn	   
	   | LEXERROR ZPosn
	   | FCSTART
	   | FCEND
	   deriving  (Eq)

convertPosition :: AlexPosn -> String -> ZPosn
convertPosition (AlexPn _ line col) s = ((line, col), (line, col + length s))

getTokenPosn :: Token -> ZPosn
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
getTokenPosn (ASSIGN p) = p
getTokenPosn (WHILE p) = p
getTokenPosn (DO p) = p
getTokenPosn (END p) = p
getTokenPosn (RETURN p) = p
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
getTokenPosn (NOTEQ p) = p
getTokenPosn (LEQ p) = p
getTokenPosn (GEQ p) = p
getTokenPosn (LTHAN p) = p
getTokenPosn (GTHAN p) = p
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
  show (WHILE _) = "WHILE"
  show (DO _) = "DO"
  show (END _) = "END"
  show (RETURN _) = "RETURN"
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
  show (NOTEQ _) = "NOTEQ"
  show (LEQ _) = "LEQ"
  show (GEQ _) = "GEQ"
  show (LTHAN _) = "LTHAN"
  show (GTHAN _) = "GTHAN"
  show (LEXERROR _) = "LEXERROR"
  show FCSTART = "FCSTART"
  show FCEND = "FCEND"


whileLexing :: String -> [Token]
whileLexing s = alexScanTokens s
}