{
module PcfParser 
       ( E (..) 
       , Prog (..)
       , pcfProgramParsing 
       , pcfContextParsing
       , pcfProgs
       , pcfArg 
       ) where 

import System.IO
import Control.Monad

import PcfLexer
import PcfTyping

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

data Prog = Pvar String
       	  | Pnum Int
	  | Pop Prog Prog
	  | Ptrue 
	  | Pfalse
	  | Peqtest Prog Prog
	  | Pif Prog Prog Prog
	  | Ppair Prog Prog 
	  | Pprojfst Prog
	  | Pprojsnd Prog
	  | Pinjfst Prog
	  | Pinjsnd Prog 
	  | Pcase Prog (String, Prog) (String, Prog) 
	  | Pabs String Prog 
	  | Papp Prog Prog
	  | Pfix Prog 
	  deriving (Eq, Show)

makeApps :: [Prog] -> Prog
makeApps [p] = p
makeApps (p1 : p2 : ps) = makeApps $ (Papp p1 p2) : ps

progToPcfTerm :: Prog -> PcfTerm 
progToPcfTerm (Pvar s) = TM_var s
progToPcfTerm (Pnum n) = TM_num n
progToPcfTerm (Pop e1 e2) = TM_op (progToPcfTerm e1) $ progToPcfTerm e2
progToPcfTerm (Ptrue) = TM_true 
progToPcfTerm (Pfalse) = TM_false 
progToPcfTerm (Peqtest e1 e2) = TM_eqtest (progToPcfTerm e1) $ progToPcfTerm e2
progToPcfTerm (Pif e0 e1 e2) = TM_if (progToPcfTerm e0)  (progToPcfTerm e1) $ progToPcfTerm e2
progToPcfTerm (Ppair e1 e2) = TM_pair (progToPcfTerm e1) $ progToPcfTerm e2
progToPcfTerm (Pprojfst e) = TM_projfst $ progToPcfTerm e
progToPcfTerm (Pprojsnd e) = TM_projsnd $ progToPcfTerm e
progToPcfTerm (Pinjfst e) = TM_injfst $ progToPcfTerm e
progToPcfTerm (Pinjsnd e) = TM_injsnd $ progToPcfTerm e
progToPcfTerm (Pcase e0 (s1, e1) (s2, e2)) = TM_case (progToPcfTerm e0) (s1, progToPcfTerm e1) (s2, progToPcfTerm e2)
progToPcfTerm (Pabs str e) = TM_abs str $ progToPcfTerm e
progToPcfTerm (Papp e1 e2) = TM_app (progToPcfTerm e1) $ progToPcfTerm e2
progToPcfTerm (Pfix e) = TM_fix $ progToPcfTerm e

}

%name pcfProgramParsing pcfProgram 
%name pcfContextParsing pcfContext
%name pcfProgs pcfProgseq
%name pcfArg pcfArgument
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
%left '='
%left "+"
%left "*"

%%

pcfType :: { PcfType }
	: "Nat" { T_nat }
	| "Bool" { T_bool }
	| pcfType "*" pcfType { T_prod $1 $3 }
	| pcfType "+" pcfType { T_sum $1 $3 }
	| pcfType "->" pcfType { T_arrow $1 $3 }
	| '(' pcfType ')' { $2 }

pcfContext :: { TypeContext }
	   : {- empty -} { [] } 
	   | "id" ':' pcfType { let IDENT _ s = $1 in [(s, $3)] }
	   | pcfContext ';' "id" ':' pcfType { let IDENT _ s = $3 in $1 ++ [(s, $5)] }

pcfProgram :: { PcfTerm }
	   : pcfProgseq				{ progToPcfTerm $ makeApps $1 }

pcfProgseq :: { [Prog] }
	   : pcfArgument			{ [$1] }
	   | pcfProgseq pcfArgument   		{ $1 ++ [$2] }   

pcfArgument :: { Prog }
            : "id" 	    		   	{ let IDENT p s = $1 in Pvar s }
            | "num" 				{ let NUMBER p n = $1 in Pnum n }
	    | pcfProgseq "+" pcfProgseq      	{ Pop (makeApps $1) (makeApps $3) }
	    | pcfProgseq "*" pcfProgseq      	{ Pop (makeApps $1) (makeApps $3) }
	    | pcfProgseq "==" pcfProgseq     	{ Peqtest (makeApps $1) (makeApps $3) }
	    | "True"	    			{ Ptrue }
	    | "False"				{ Pfalse }
	    | "if" pcfProgseq "then" pcfProgseq "else" pcfProgseq 
	      { Pif (makeApps $2) (makeApps $4) (makeApps $6) }
  	    | '(' pcfProgseq ')'    	 	{ makeApps $2 }
	    | '<' pcfProgseq ',' pcfProgseq '>'	{ Ppair (makeApps $2) (makeApps $4) }
	    | "projfst" pcfArgument    	       	{ Pprojfst $2 }
	    | "projsnd" pcfArgument		{ Pprojsnd $2 }
	    | "injfst" pcfArgument 		{ Pinjfst $2 }
	    | "injsnd" pcfArgument 		{ Pinjsnd $2 }
	    | "case" pcfProgseq "of" "injfst" "id" "in" pcfProgseq ';' "injsnd" "id" "in" pcfProgseq
	      { let IDENT _ s1 = $5 in let IDENT _ s2 = $10 in 
	        Pcase (makeApps $2) (s1, makeApps $7) (s2, makeApps $12)
	      }
	    | "fix" pcfArgument			{ Pfix $2 }
	    | "func" "id" "->" pcfProgseq 	{ let IDENT _ s = $2 in Pabs s (makeApps $4) }

{
parseError [] = failE "Parse error!"
parseError (tok : ts) = failE $ "Error: " ++ show tok

}
