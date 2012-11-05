{-# OPTIONS_GHC -w #-}
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

-- parser produced by Happy Version 1.18.6

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (WExp)
	| HappyAbsSyn5 (WProg)
	| HappyAbsSyn6 ([String])
	| HappyAbsSyn7 ((WProg, [String]))

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51 :: () => Int -> ({-HappyReduction (E) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (E) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (E) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (E) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20 :: () => ({-HappyReduction (E) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (E) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (E) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (E) HappyAbsSyn)

action_0 (27) = happyShift action_5
action_0 (34) = happyShift action_6
action_0 (37) = happyShift action_7
action_0 (5) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 _ = happyFail

action_1 (27) = happyShift action_2
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (9) = happyShift action_15
action_3 _ = happyFail

action_4 (52) = happyAccept
action_4 _ = happyFail

action_5 (19) = happyShift action_14
action_5 _ = happyFail

action_6 (21) = happyShift action_9
action_6 (27) = happyShift action_2
action_6 (29) = happyShift action_10
action_6 (32) = happyShift action_11
action_6 (33) = happyShift action_12
action_6 (4) = happyGoto action_13
action_6 _ = happyFail

action_7 (21) = happyShift action_9
action_7 (27) = happyShift action_2
action_7 (29) = happyShift action_10
action_7 (32) = happyShift action_11
action_7 (33) = happyShift action_12
action_7 (4) = happyGoto action_8
action_7 _ = happyFail

action_8 (11) = happyShift action_19
action_8 (12) = happyShift action_20
action_8 (13) = happyShift action_21
action_8 (14) = happyShift action_22
action_8 (15) = happyShift action_23
action_8 (16) = happyShift action_24
action_8 (17) = happyShift action_25
action_8 (18) = happyShift action_26
action_8 (38) = happyShift action_29
action_8 _ = happyFail

action_9 (21) = happyShift action_9
action_9 (27) = happyShift action_2
action_9 (29) = happyShift action_10
action_9 (32) = happyShift action_11
action_9 (33) = happyShift action_12
action_9 (4) = happyGoto action_28
action_9 _ = happyFail

action_10 _ = happyReduce_2

action_11 _ = happyReduce_3

action_12 _ = happyReduce_4

action_13 (11) = happyShift action_19
action_13 (12) = happyShift action_20
action_13 (13) = happyShift action_21
action_13 (14) = happyShift action_22
action_13 (15) = happyShift action_23
action_13 (16) = happyShift action_24
action_13 (17) = happyShift action_25
action_13 (18) = happyShift action_26
action_13 (35) = happyShift action_27
action_13 _ = happyFail

action_14 (21) = happyShift action_9
action_14 (27) = happyShift action_2
action_14 (29) = happyShift action_10
action_14 (32) = happyShift action_11
action_14 (33) = happyShift action_12
action_14 (4) = happyGoto action_18
action_14 _ = happyFail

action_15 (27) = happyShift action_5
action_15 (34) = happyShift action_6
action_15 (37) = happyShift action_7
action_15 (40) = happyShift action_17
action_15 (5) = happyGoto action_16
action_15 _ = happyFail

action_16 _ = happyReduce_17

action_17 (21) = happyShift action_41
action_17 _ = happyFail

action_18 (11) = happyShift action_19
action_18 (12) = happyShift action_20
action_18 (13) = happyShift action_21
action_18 (14) = happyShift action_22
action_18 (15) = happyShift action_23
action_18 (16) = happyShift action_24
action_18 (17) = happyShift action_25
action_18 (18) = happyShift action_26
action_18 _ = happyReduce_14

action_19 (21) = happyShift action_9
action_19 (27) = happyShift action_2
action_19 (29) = happyShift action_10
action_19 (32) = happyShift action_11
action_19 (33) = happyShift action_12
action_19 (4) = happyGoto action_40
action_19 _ = happyFail

action_20 (21) = happyShift action_9
action_20 (27) = happyShift action_2
action_20 (29) = happyShift action_10
action_20 (32) = happyShift action_11
action_20 (33) = happyShift action_12
action_20 (4) = happyGoto action_39
action_20 _ = happyFail

action_21 (21) = happyShift action_9
action_21 (27) = happyShift action_2
action_21 (29) = happyShift action_10
action_21 (32) = happyShift action_11
action_21 (33) = happyShift action_12
action_21 (4) = happyGoto action_38
action_21 _ = happyFail

action_22 (21) = happyShift action_9
action_22 (27) = happyShift action_2
action_22 (29) = happyShift action_10
action_22 (32) = happyShift action_11
action_22 (33) = happyShift action_12
action_22 (4) = happyGoto action_37
action_22 _ = happyFail

action_23 (21) = happyShift action_9
action_23 (27) = happyShift action_2
action_23 (29) = happyShift action_10
action_23 (32) = happyShift action_11
action_23 (33) = happyShift action_12
action_23 (4) = happyGoto action_36
action_23 _ = happyFail

action_24 (21) = happyShift action_9
action_24 (27) = happyShift action_2
action_24 (29) = happyShift action_10
action_24 (32) = happyShift action_11
action_24 (33) = happyShift action_12
action_24 (4) = happyGoto action_35
action_24 _ = happyFail

action_25 (21) = happyShift action_9
action_25 (27) = happyShift action_2
action_25 (29) = happyShift action_10
action_25 (32) = happyShift action_11
action_25 (33) = happyShift action_12
action_25 (4) = happyGoto action_34
action_25 _ = happyFail

action_26 (21) = happyShift action_9
action_26 (27) = happyShift action_2
action_26 (29) = happyShift action_10
action_26 (32) = happyShift action_11
action_26 (33) = happyShift action_12
action_26 (4) = happyGoto action_33
action_26 _ = happyFail

action_27 (27) = happyShift action_5
action_27 (34) = happyShift action_6
action_27 (37) = happyShift action_7
action_27 (5) = happyGoto action_32
action_27 _ = happyFail

action_28 (11) = happyShift action_19
action_28 (12) = happyShift action_20
action_28 (13) = happyShift action_21
action_28 (14) = happyShift action_22
action_28 (15) = happyShift action_23
action_28 (16) = happyShift action_24
action_28 (17) = happyShift action_25
action_28 (18) = happyShift action_26
action_28 (22) = happyShift action_31
action_28 _ = happyFail

action_29 (27) = happyShift action_5
action_29 (34) = happyShift action_6
action_29 (37) = happyShift action_7
action_29 (5) = happyGoto action_30
action_29 _ = happyFail

action_30 (9) = happyShift action_42
action_30 (39) = happyShift action_46
action_30 _ = happyFail

action_31 _ = happyReduce_5

action_32 (9) = happyShift action_42
action_32 (36) = happyShift action_45
action_32 _ = happyFail

action_33 (11) = happyShift action_19
action_33 (12) = happyShift action_20
action_33 (13) = happyFail
action_33 (14) = happyFail
action_33 (15) = happyFail
action_33 (16) = happyFail
action_33 (17) = happyFail
action_33 (18) = happyFail
action_33 _ = happyReduce_13

action_34 (11) = happyShift action_19
action_34 (12) = happyShift action_20
action_34 (13) = happyFail
action_34 (14) = happyFail
action_34 (15) = happyFail
action_34 (16) = happyFail
action_34 (17) = happyFail
action_34 (18) = happyFail
action_34 _ = happyReduce_12

action_35 (11) = happyShift action_19
action_35 (12) = happyShift action_20
action_35 (13) = happyFail
action_35 (14) = happyFail
action_35 (15) = happyFail
action_35 (16) = happyFail
action_35 (17) = happyFail
action_35 (18) = happyFail
action_35 _ = happyReduce_11

action_36 (11) = happyShift action_19
action_36 (12) = happyShift action_20
action_36 (13) = happyFail
action_36 (14) = happyFail
action_36 (15) = happyFail
action_36 (16) = happyFail
action_36 (17) = happyFail
action_36 (18) = happyFail
action_36 _ = happyReduce_10

action_37 (11) = happyShift action_19
action_37 (12) = happyShift action_20
action_37 (13) = happyFail
action_37 (14) = happyFail
action_37 (15) = happyFail
action_37 (16) = happyFail
action_37 (17) = happyFail
action_37 (18) = happyFail
action_37 _ = happyReduce_9

action_38 (11) = happyShift action_19
action_38 (12) = happyShift action_20
action_38 (13) = happyFail
action_38 (14) = happyFail
action_38 (15) = happyFail
action_38 (16) = happyFail
action_38 (17) = happyFail
action_38 (18) = happyFail
action_38 _ = happyReduce_8

action_39 (11) = happyShift action_19
action_39 _ = happyReduce_6

action_40 _ = happyReduce_7

action_41 (27) = happyShift action_44
action_41 (6) = happyGoto action_43
action_41 _ = happyFail

action_42 (27) = happyShift action_5
action_42 (34) = happyShift action_6
action_42 (37) = happyShift action_7
action_42 (5) = happyGoto action_16
action_42 _ = happyFail

action_43 (10) = happyShift action_48
action_43 (22) = happyShift action_49
action_43 _ = happyFail

action_44 _ = happyReduce_18

action_45 (27) = happyShift action_5
action_45 (34) = happyShift action_6
action_45 (37) = happyShift action_7
action_45 (5) = happyGoto action_47
action_45 _ = happyFail

action_46 _ = happyReduce_16

action_47 (9) = happyShift action_42
action_47 (39) = happyShift action_51
action_47 _ = happyFail

action_48 (27) = happyShift action_50
action_48 _ = happyFail

action_49 _ = happyReduce_20

action_50 _ = happyReduce_19

action_51 _ = happyReduce_15

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (let IDENT p s = happy_var_1 in Wvar s
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (let NUMBER p n = happy_var_1 in Wconst (Wnat n)
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 _
	 =  HappyAbsSyn4
		 (Wconst (Wbool True)
	)

happyReduce_4 = happySpecReduce_1  4 happyReduction_4
happyReduction_4 _
	 =  HappyAbsSyn4
		 (Wconst (Wbool False)
	)

happyReduce_5 = happySpecReduce_3  4 happyReduction_5
happyReduction_5 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  4 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Wadd happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  4 happyReduction_7
happyReduction_7 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Wmult happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  4 happyReduction_8
happyReduction_8 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Weq happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  4 happyReduction_9
happyReduction_9 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Wnot (Weq happy_var_1 happy_var_3)
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  4 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Wnot (Wgt happy_var_1 happy_var_3)
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  4 happyReduction_11
happyReduction_11 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Wnot (Wlt happy_var_1 happy_var_3)
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  4 happyReduction_12
happyReduction_12 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Wlt happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  4 happyReduction_13
happyReduction_13 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Wgt happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  5 happyReduction_14
happyReduction_14 (HappyAbsSyn4  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (let IDENT p s = happy_var_1 in Wassign s happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happyReduce 7 5 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Wif happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 5 5 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Wwhile happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_3  5 happyReduction_17
happyReduction_17 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Wseq happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  6 happyReduction_18
happyReduction_18 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn6
		 (let IDENT p s = happy_var_1 in [s]
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  6 happyReduction_19
happyReduction_19 (HappyTerminal happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (let IDENT p s = happy_var_3 in happy_var_1 ++ [s]
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 6 7 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ((happy_var_1, happy_var_5)
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 52 52 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	COLON happy_dollar_dollar -> cont 8;
	SEMICOLON happy_dollar_dollar -> cont 9;
	COMMA happy_dollar_dollar -> cont 10;
	STAR happy_dollar_dollar -> cont 11;
	PLUS happy_dollar_dollar -> cont 12;
	EQUAL happy_dollar_dollar -> cont 13;
	NOTEQ happy_dollar_dollar -> cont 14;
	LEQ happy_dollar_dollar -> cont 15;
	GEQ happy_dollar_dollar -> cont 16;
	LTHAN happy_dollar_dollar -> cont 17;
	GTHAN happy_dollar_dollar -> cont 18;
	ASSIGN happy_dollar_dollar -> cont 19;
	ARROW happy_dollar_dollar -> cont 20;
	LPAREN happy_dollar_dollar -> cont 21;
	RPAREN happy_dollar_dollar -> cont 22;
	LPAIR happy_dollar_dollar -> cont 23;
	RPAIR happy_dollar_dollar -> cont 24;
	LBOX happy_dollar_dollar -> cont 25;
	RBOX happy_dollar_dollar -> cont 26;
	IDENT p s -> cont 27;
	IDENTCAP p s -> cont 28;
	NUMBER p n -> cont 29;
	NAT happy_dollar_dollar -> cont 30;
	BOOL happy_dollar_dollar -> cont 31;
	TRUE happy_dollar_dollar -> cont 32;
	FALSE happy_dollar_dollar -> cont 33;
	IF happy_dollar_dollar -> cont 34;
	THEN happy_dollar_dollar -> cont 35;
	ELSE happy_dollar_dollar -> cont 36;
	WHILE happy_dollar_dollar -> cont 37;
	DO happy_dollar_dollar -> cont 38;
	END happy_dollar_dollar -> cont 39;
	RETURN happy_dollar_dollar -> cont 40;
	FUNC happy_dollar_dollar -> cont 41;
	PROJFST happy_dollar_dollar -> cont 42;
	PROJSND happy_dollar_dollar -> cont 43;
	INJFST happy_dollar_dollar -> cont 44;
	INJSND happy_dollar_dollar -> cont 45;
	CASE happy_dollar_dollar -> cont 46;
	OF happy_dollar_dollar -> cont 47;
	IN happy_dollar_dollar -> cont 48;
	FIX happy_dollar_dollar -> cont 49;
	FCEND -> cont 50;
	FCSTART -> cont 51;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

happyThen :: () => E a -> (a -> E b) -> E b
happyThen = (thenE)
happyReturn :: () => a -> E a
happyReturn = (returnE)
happyThen1 m k tks = (thenE) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> E a
happyReturn1 = \a tks -> (returnE) a
happyError' :: () => [(Token)] -> E a
happyError' = parseError

whileProgramParsing tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn7 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError [] = failE "Parse error!"
parseError (tok : ts) = failE $ "Error: " ++ show tok
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 311 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
