{
module WhileParser 
       ( E (..) 
       , whileProgramParsing 
       ) where 

import System.IO
import Control.Monad

import WhileLexer
import WhileLang

data E a = Ok a | Failed String 
     deriving (Show)

thenE :: E a -> (a -> E b) -> E b
thenE m k = 
  case m of Ok x -> k x 
            Failed s -> Failed s 

returnE :: a -> E a
returnE x = Ok x

failE :: String -> E a
failE s = Failed s 

catchE :: E a -> (String -> E a) -> E a
catchE m f = 
  case m of Ok a -> Ok a
       	    Failed s -> f s

}

%name whileProgramParsing whileProgram 
%tokentype { Token }
%monad { E } { thenE } { returnE }
%error { parseError }

%token
  ':'		 { COLON $$ }
  ';'		 { SEMICOLON $$ }
  ','		 { COMMA $$ }
  "*"		 { STAR $$ }
  "+"		 { PLUS $$ }
  "=="		 { EQUAL $$ }
  "!="		 { NOTEQ $$ }
  "<="		 { LEQ $$ }
  ">="		 { GEQ $$ }
  "<"		 { LTHAN $$ }
  ">"		 { GTHAN $$ }
  ":="		 { ASSIGN $$ }
  "->"		 { ARROW $$ }
  '('		 { LPAREN $$ }
  ')'		 { RPAREN $$ }
  '<'		 { LPAIR $$ }
  '>'		 { RPAIR $$ }
  '['		 { LBOX $$ }
  ']'		 { RBOX $$ }
  "id"		 { IDENT p s } 
  "idcap"	 { IDENTCAP p s } 
  "num"		 { NUMBER p n } 
  "Nat"		 { NAT $$ }
  "Bool"	 { BOOL $$ }
  "True"	 { TRUE $$ }
  "False"	 { FALSE $$ }
  "if"		 { IF $$ }
  "then"	 { THEN $$ }
  "else"	 { ELSE $$ }
  "while"        { WHILE $$ }
  "do"           { DO $$ }
  "end"          { END $$ }
  "return"       { RETURN $$ }
  "func" 	 { FUNC $$ }
  "projfst"	 { PROJFST $$ }
  "projsnd"	 { PROJSND $$ }
  "injfst"	 { INJFST $$ }
  "injsnd"	 { INJSND $$ }
  "case"	 { CASE $$ }
  "of"		 { OF $$ }
  "in"  	 { IN $$ }
  "fix"  	 { FIX $$ }
  "fcend"	 { FCEND }
  "fcstart" 	 { FCSTART }

%right "->"
%left '=' ';'
%nonassoc "==" "!=" "<=" ">=" "<" ">"
%left "+"
%left "*"

%%

whileExpression :: { WExp } 
: "id" { let IDENT p s = $1 in Wvar s }
| "num" { let NUMBER p n = $1 in Wconst (Wnat n) }
| "True" { Wconst (Wbool True) }
| "False" { Wconst (Wbool False) }
| '(' whileExpression ')' { $2 }
| whileExpression "+" whileExpression { Wadd $1 $3 }
| whileExpression "*" whileExpression { Wmult $1 $3 }
| whileExpression "==" whileExpression {Weq $1 $3 }
| whileExpression "!=" whileExpression {Wnot (Weq $1 $3) }
| whileExpression "<=" whileExpression {Wnot (Wgt $1 $3) }
| whileExpression ">=" whileExpression {Wnot (Wlt $1 $3) }
| whileExpression "<" whileExpression {Wlt $1 $3 }
| whileExpression ">" whileExpression {Wgt $1 $3 }

whileStatement :: { WProg }
: "id" ":=" whileExpression { let IDENT p s = $1 in Wassign s $3 }
| "if" whileExpression "then" whileStatement "else" whileStatement "end" { Wif $2 $4 $6 } 
| "while" whileExpression "do" whileStatement "end" { Wwhile $2 $4 }
| whileStatement ';' whileStatement { Wseq $1 $3 }

returnList :: { [String] }
: "id" { let IDENT p s = $1 in [s] }
| returnList ',' "id" { let IDENT p s = $3 in $1 ++ [s] }

whileProgram :: { (WProg, [String]) } 
: whileStatement ';' "return" '(' returnList ')' { ($1, $5) }

{
parseError [] = failE "Parse error!"
parseError (tok : ts) = failE $ "Error: " ++ show tok

}
