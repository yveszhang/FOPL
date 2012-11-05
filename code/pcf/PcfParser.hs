{-# OPTIONS_GHC -w #-}
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

-- parser produced by Happy Version 1.18.6

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn7 (PcfType)
	| HappyAbsSyn8 (TypeContext)
	| HappyAbsSyn9 (PcfTerm)
	| HappyAbsSyn10 ([Prog])
	| HappyAbsSyn11 (Prog)

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
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80 :: () => Int -> ({-HappyReduction (E) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (E) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (E) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (E) HappyAbsSyn)

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
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32 :: () => ({-HappyReduction (E) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (E) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (E) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (E) HappyAbsSyn)

action_0 (19) = happyShift action_8
action_0 (21) = happyShift action_9
action_0 (25) = happyShift action_10
action_0 (27) = happyShift action_11
action_0 (30) = happyShift action_12
action_0 (31) = happyShift action_13
action_0 (32) = happyShift action_14
action_0 (35) = happyShift action_15
action_0 (36) = happyShift action_16
action_0 (37) = happyShift action_17
action_0 (38) = happyShift action_18
action_0 (39) = happyShift action_19
action_0 (40) = happyShift action_20
action_0 (43) = happyShift action_21
action_0 (9) = happyGoto action_26
action_0 (10) = happyGoto action_27
action_0 (11) = happyGoto action_23
action_0 _ = happyFail

action_1 (25) = happyShift action_25
action_1 (8) = happyGoto action_24
action_1 _ = happyReduce_10

action_2 (19) = happyShift action_8
action_2 (21) = happyShift action_9
action_2 (25) = happyShift action_10
action_2 (27) = happyShift action_11
action_2 (30) = happyShift action_12
action_2 (31) = happyShift action_13
action_2 (32) = happyShift action_14
action_2 (35) = happyShift action_15
action_2 (36) = happyShift action_16
action_2 (37) = happyShift action_17
action_2 (38) = happyShift action_18
action_2 (39) = happyShift action_19
action_2 (40) = happyShift action_20
action_2 (43) = happyShift action_21
action_2 (10) = happyGoto action_22
action_2 (11) = happyGoto action_23
action_2 _ = happyFail

action_3 (19) = happyShift action_8
action_3 (21) = happyShift action_9
action_3 (25) = happyShift action_10
action_3 (27) = happyShift action_11
action_3 (30) = happyShift action_12
action_3 (31) = happyShift action_13
action_3 (32) = happyShift action_14
action_3 (35) = happyShift action_15
action_3 (36) = happyShift action_16
action_3 (37) = happyShift action_17
action_3 (38) = happyShift action_18
action_3 (39) = happyShift action_19
action_3 (40) = happyShift action_20
action_3 (43) = happyShift action_21
action_3 (10) = happyGoto action_6
action_3 (11) = happyGoto action_7
action_3 _ = happyFail

action_4 (28) = happyShift action_5
action_4 _ = happyFail

action_5 _ = happyReduce_4

action_6 (15) = happyShift action_29
action_6 (16) = happyShift action_30
action_6 (17) = happyShift action_31
action_6 (19) = happyShift action_8
action_6 (21) = happyShift action_9
action_6 (25) = happyShift action_10
action_6 (27) = happyShift action_11
action_6 (30) = happyShift action_12
action_6 (31) = happyShift action_13
action_6 (32) = happyShift action_14
action_6 (35) = happyShift action_15
action_6 (36) = happyShift action_16
action_6 (37) = happyShift action_17
action_6 (38) = happyShift action_18
action_6 (39) = happyShift action_19
action_6 (40) = happyShift action_20
action_6 (43) = happyShift action_21
action_6 (10) = happyGoto action_6
action_6 (11) = happyGoto action_28
action_6 _ = happyFail

action_7 (46) = happyAccept
action_7 _ = happyReduce_14

action_8 (19) = happyShift action_8
action_8 (21) = happyShift action_9
action_8 (25) = happyShift action_10
action_8 (27) = happyShift action_11
action_8 (30) = happyShift action_12
action_8 (31) = happyShift action_13
action_8 (32) = happyShift action_14
action_8 (35) = happyShift action_15
action_8 (36) = happyShift action_16
action_8 (37) = happyShift action_17
action_8 (38) = happyShift action_18
action_8 (39) = happyShift action_19
action_8 (40) = happyShift action_20
action_8 (43) = happyShift action_21
action_8 (10) = happyGoto action_43
action_8 (11) = happyGoto action_23
action_8 _ = happyFail

action_9 (19) = happyShift action_8
action_9 (21) = happyShift action_9
action_9 (25) = happyShift action_10
action_9 (27) = happyShift action_11
action_9 (30) = happyShift action_12
action_9 (31) = happyShift action_13
action_9 (32) = happyShift action_14
action_9 (35) = happyShift action_15
action_9 (36) = happyShift action_16
action_9 (37) = happyShift action_17
action_9 (38) = happyShift action_18
action_9 (39) = happyShift action_19
action_9 (40) = happyShift action_20
action_9 (43) = happyShift action_21
action_9 (10) = happyGoto action_42
action_9 (11) = happyGoto action_23
action_9 _ = happyFail

action_10 _ = happyReduce_16

action_11 _ = happyReduce_17

action_12 _ = happyReduce_21

action_13 _ = happyReduce_22

action_14 (19) = happyShift action_8
action_14 (21) = happyShift action_9
action_14 (25) = happyShift action_10
action_14 (27) = happyShift action_11
action_14 (30) = happyShift action_12
action_14 (31) = happyShift action_13
action_14 (32) = happyShift action_14
action_14 (35) = happyShift action_15
action_14 (36) = happyShift action_16
action_14 (37) = happyShift action_17
action_14 (38) = happyShift action_18
action_14 (39) = happyShift action_19
action_14 (40) = happyShift action_20
action_14 (43) = happyShift action_21
action_14 (10) = happyGoto action_41
action_14 (11) = happyGoto action_23
action_14 _ = happyFail

action_15 (25) = happyShift action_40
action_15 _ = happyFail

action_16 (19) = happyShift action_8
action_16 (21) = happyShift action_9
action_16 (25) = happyShift action_10
action_16 (27) = happyShift action_11
action_16 (30) = happyShift action_12
action_16 (31) = happyShift action_13
action_16 (32) = happyShift action_14
action_16 (35) = happyShift action_15
action_16 (36) = happyShift action_16
action_16 (37) = happyShift action_17
action_16 (38) = happyShift action_18
action_16 (39) = happyShift action_19
action_16 (40) = happyShift action_20
action_16 (43) = happyShift action_21
action_16 (10) = happyGoto action_6
action_16 (11) = happyGoto action_39
action_16 _ = happyFail

action_17 (19) = happyShift action_8
action_17 (21) = happyShift action_9
action_17 (25) = happyShift action_10
action_17 (27) = happyShift action_11
action_17 (30) = happyShift action_12
action_17 (31) = happyShift action_13
action_17 (32) = happyShift action_14
action_17 (35) = happyShift action_15
action_17 (36) = happyShift action_16
action_17 (37) = happyShift action_17
action_17 (38) = happyShift action_18
action_17 (39) = happyShift action_19
action_17 (40) = happyShift action_20
action_17 (43) = happyShift action_21
action_17 (10) = happyGoto action_6
action_17 (11) = happyGoto action_38
action_17 _ = happyFail

action_18 (19) = happyShift action_8
action_18 (21) = happyShift action_9
action_18 (25) = happyShift action_10
action_18 (27) = happyShift action_11
action_18 (30) = happyShift action_12
action_18 (31) = happyShift action_13
action_18 (32) = happyShift action_14
action_18 (35) = happyShift action_15
action_18 (36) = happyShift action_16
action_18 (37) = happyShift action_17
action_18 (38) = happyShift action_18
action_18 (39) = happyShift action_19
action_18 (40) = happyShift action_20
action_18 (43) = happyShift action_21
action_18 (10) = happyGoto action_6
action_18 (11) = happyGoto action_37
action_18 _ = happyFail

action_19 (19) = happyShift action_8
action_19 (21) = happyShift action_9
action_19 (25) = happyShift action_10
action_19 (27) = happyShift action_11
action_19 (30) = happyShift action_12
action_19 (31) = happyShift action_13
action_19 (32) = happyShift action_14
action_19 (35) = happyShift action_15
action_19 (36) = happyShift action_16
action_19 (37) = happyShift action_17
action_19 (38) = happyShift action_18
action_19 (39) = happyShift action_19
action_19 (40) = happyShift action_20
action_19 (43) = happyShift action_21
action_19 (10) = happyGoto action_6
action_19 (11) = happyGoto action_36
action_19 _ = happyFail

action_20 (19) = happyShift action_8
action_20 (21) = happyShift action_9
action_20 (25) = happyShift action_10
action_20 (27) = happyShift action_11
action_20 (30) = happyShift action_12
action_20 (31) = happyShift action_13
action_20 (32) = happyShift action_14
action_20 (35) = happyShift action_15
action_20 (36) = happyShift action_16
action_20 (37) = happyShift action_17
action_20 (38) = happyShift action_18
action_20 (39) = happyShift action_19
action_20 (40) = happyShift action_20
action_20 (43) = happyShift action_21
action_20 (10) = happyGoto action_35
action_20 (11) = happyGoto action_23
action_20 _ = happyFail

action_21 (19) = happyShift action_8
action_21 (21) = happyShift action_9
action_21 (25) = happyShift action_10
action_21 (27) = happyShift action_11
action_21 (30) = happyShift action_12
action_21 (31) = happyShift action_13
action_21 (32) = happyShift action_14
action_21 (35) = happyShift action_15
action_21 (36) = happyShift action_16
action_21 (37) = happyShift action_17
action_21 (38) = happyShift action_18
action_21 (39) = happyShift action_19
action_21 (40) = happyShift action_20
action_21 (43) = happyShift action_21
action_21 (10) = happyGoto action_6
action_21 (11) = happyGoto action_34
action_21 _ = happyFail

action_22 (15) = happyShift action_29
action_22 (16) = happyShift action_30
action_22 (17) = happyShift action_31
action_22 (19) = happyShift action_8
action_22 (21) = happyShift action_9
action_22 (25) = happyShift action_10
action_22 (27) = happyShift action_11
action_22 (30) = happyShift action_12
action_22 (31) = happyShift action_13
action_22 (32) = happyShift action_14
action_22 (35) = happyShift action_15
action_22 (36) = happyShift action_16
action_22 (37) = happyShift action_17
action_22 (38) = happyShift action_18
action_22 (39) = happyShift action_19
action_22 (40) = happyShift action_20
action_22 (43) = happyShift action_21
action_22 (46) = happyAccept
action_22 (10) = happyGoto action_6
action_22 (11) = happyGoto action_28
action_22 _ = happyFail

action_23 _ = happyReduce_14

action_24 (13) = happyShift action_33
action_24 (46) = happyAccept
action_24 _ = happyFail

action_25 (12) = happyShift action_32
action_25 _ = happyFail

action_26 (46) = happyAccept
action_26 _ = happyFail

action_27 (15) = happyShift action_29
action_27 (16) = happyShift action_30
action_27 (17) = happyShift action_31
action_27 (19) = happyShift action_8
action_27 (21) = happyShift action_9
action_27 (25) = happyShift action_10
action_27 (27) = happyShift action_11
action_27 (30) = happyShift action_12
action_27 (31) = happyShift action_13
action_27 (32) = happyShift action_14
action_27 (35) = happyShift action_15
action_27 (36) = happyShift action_16
action_27 (37) = happyShift action_17
action_27 (38) = happyShift action_18
action_27 (39) = happyShift action_19
action_27 (40) = happyShift action_20
action_27 (43) = happyShift action_21
action_27 (10) = happyGoto action_6
action_27 (11) = happyGoto action_28
action_27 _ = happyReduce_13

action_28 (15) = happyReduce_15
action_28 (16) = happyReduce_15
action_28 (17) = happyReduce_15
action_28 (19) = happyReduce_15
action_28 (21) = happyReduce_15
action_28 (25) = happyReduce_15
action_28 (27) = happyReduce_15
action_28 (30) = happyReduce_15
action_28 (31) = happyReduce_15
action_28 (32) = happyReduce_15
action_28 (35) = happyReduce_15
action_28 (36) = happyReduce_15
action_28 (37) = happyReduce_15
action_28 (38) = happyReduce_15
action_28 (39) = happyReduce_15
action_28 (40) = happyReduce_15
action_28 (43) = happyReduce_15
action_28 _ = happyReduce_15

action_29 (19) = happyShift action_8
action_29 (21) = happyShift action_9
action_29 (25) = happyShift action_10
action_29 (27) = happyShift action_11
action_29 (30) = happyShift action_12
action_29 (31) = happyShift action_13
action_29 (32) = happyShift action_14
action_29 (35) = happyShift action_15
action_29 (36) = happyShift action_16
action_29 (37) = happyShift action_17
action_29 (38) = happyShift action_18
action_29 (39) = happyShift action_19
action_29 (40) = happyShift action_20
action_29 (43) = happyShift action_21
action_29 (10) = happyGoto action_55
action_29 (11) = happyGoto action_23
action_29 _ = happyFail

action_30 (19) = happyShift action_8
action_30 (21) = happyShift action_9
action_30 (25) = happyShift action_10
action_30 (27) = happyShift action_11
action_30 (30) = happyShift action_12
action_30 (31) = happyShift action_13
action_30 (32) = happyShift action_14
action_30 (35) = happyShift action_15
action_30 (36) = happyShift action_16
action_30 (37) = happyShift action_17
action_30 (38) = happyShift action_18
action_30 (39) = happyShift action_19
action_30 (40) = happyShift action_20
action_30 (43) = happyShift action_21
action_30 (10) = happyGoto action_54
action_30 (11) = happyGoto action_23
action_30 _ = happyFail

action_31 (19) = happyShift action_8
action_31 (21) = happyShift action_9
action_31 (25) = happyShift action_10
action_31 (27) = happyShift action_11
action_31 (30) = happyShift action_12
action_31 (31) = happyShift action_13
action_31 (32) = happyShift action_14
action_31 (35) = happyShift action_15
action_31 (36) = happyShift action_16
action_31 (37) = happyShift action_17
action_31 (38) = happyShift action_18
action_31 (39) = happyShift action_19
action_31 (40) = happyShift action_20
action_31 (43) = happyShift action_21
action_31 (10) = happyGoto action_53
action_31 (11) = happyGoto action_23
action_31 _ = happyFail

action_32 (19) = happyShift action_51
action_32 (28) = happyShift action_5
action_32 (29) = happyShift action_52
action_32 (7) = happyGoto action_50
action_32 _ = happyFail

action_33 (25) = happyShift action_49
action_33 _ = happyFail

action_34 (15) = happyReduce_31
action_34 (16) = happyReduce_31
action_34 (17) = happyReduce_31
action_34 (19) = happyReduce_31
action_34 (21) = happyReduce_31
action_34 (25) = happyReduce_31
action_34 (27) = happyReduce_31
action_34 (30) = happyReduce_31
action_34 (31) = happyReduce_31
action_34 (32) = happyReduce_31
action_34 (35) = happyReduce_31
action_34 (36) = happyReduce_31
action_34 (37) = happyReduce_31
action_34 (38) = happyReduce_31
action_34 (39) = happyReduce_31
action_34 (40) = happyReduce_31
action_34 (43) = happyReduce_31
action_34 _ = happyReduce_31

action_35 (15) = happyShift action_29
action_35 (16) = happyShift action_30
action_35 (17) = happyShift action_31
action_35 (19) = happyShift action_8
action_35 (21) = happyShift action_9
action_35 (25) = happyShift action_10
action_35 (27) = happyShift action_11
action_35 (30) = happyShift action_12
action_35 (31) = happyShift action_13
action_35 (32) = happyShift action_14
action_35 (35) = happyShift action_15
action_35 (36) = happyShift action_16
action_35 (37) = happyShift action_17
action_35 (38) = happyShift action_18
action_35 (39) = happyShift action_19
action_35 (40) = happyShift action_20
action_35 (41) = happyShift action_48
action_35 (43) = happyShift action_21
action_35 (10) = happyGoto action_6
action_35 (11) = happyGoto action_28
action_35 _ = happyFail

action_36 (15) = happyReduce_29
action_36 (16) = happyReduce_29
action_36 (17) = happyReduce_29
action_36 (19) = happyReduce_29
action_36 (21) = happyReduce_29
action_36 (25) = happyReduce_29
action_36 (27) = happyReduce_29
action_36 (30) = happyReduce_29
action_36 (31) = happyReduce_29
action_36 (32) = happyReduce_29
action_36 (35) = happyReduce_29
action_36 (36) = happyReduce_29
action_36 (37) = happyReduce_29
action_36 (38) = happyReduce_29
action_36 (39) = happyReduce_29
action_36 (40) = happyReduce_29
action_36 (43) = happyReduce_29
action_36 _ = happyReduce_29

action_37 (15) = happyReduce_28
action_37 (16) = happyReduce_28
action_37 (17) = happyReduce_28
action_37 (19) = happyReduce_28
action_37 (21) = happyReduce_28
action_37 (25) = happyReduce_28
action_37 (27) = happyReduce_28
action_37 (30) = happyReduce_28
action_37 (31) = happyReduce_28
action_37 (32) = happyReduce_28
action_37 (35) = happyReduce_28
action_37 (36) = happyReduce_28
action_37 (37) = happyReduce_28
action_37 (38) = happyReduce_28
action_37 (39) = happyReduce_28
action_37 (40) = happyReduce_28
action_37 (43) = happyReduce_28
action_37 _ = happyReduce_28

action_38 (15) = happyReduce_27
action_38 (16) = happyReduce_27
action_38 (17) = happyReduce_27
action_38 (19) = happyReduce_27
action_38 (21) = happyReduce_27
action_38 (25) = happyReduce_27
action_38 (27) = happyReduce_27
action_38 (30) = happyReduce_27
action_38 (31) = happyReduce_27
action_38 (32) = happyReduce_27
action_38 (35) = happyReduce_27
action_38 (36) = happyReduce_27
action_38 (37) = happyReduce_27
action_38 (38) = happyReduce_27
action_38 (39) = happyReduce_27
action_38 (40) = happyReduce_27
action_38 (43) = happyReduce_27
action_38 _ = happyReduce_27

action_39 (15) = happyReduce_26
action_39 (16) = happyReduce_26
action_39 (17) = happyReduce_26
action_39 (19) = happyReduce_26
action_39 (21) = happyReduce_26
action_39 (25) = happyReduce_26
action_39 (27) = happyReduce_26
action_39 (30) = happyReduce_26
action_39 (31) = happyReduce_26
action_39 (32) = happyReduce_26
action_39 (35) = happyReduce_26
action_39 (36) = happyReduce_26
action_39 (37) = happyReduce_26
action_39 (38) = happyReduce_26
action_39 (39) = happyReduce_26
action_39 (40) = happyReduce_26
action_39 (43) = happyReduce_26
action_39 _ = happyReduce_26

action_40 (18) = happyShift action_47
action_40 _ = happyFail

action_41 (15) = happyShift action_29
action_41 (16) = happyShift action_30
action_41 (17) = happyShift action_31
action_41 (19) = happyShift action_8
action_41 (21) = happyShift action_9
action_41 (25) = happyShift action_10
action_41 (27) = happyShift action_11
action_41 (30) = happyShift action_12
action_41 (31) = happyShift action_13
action_41 (32) = happyShift action_14
action_41 (33) = happyShift action_46
action_41 (35) = happyShift action_15
action_41 (36) = happyShift action_16
action_41 (37) = happyShift action_17
action_41 (38) = happyShift action_18
action_41 (39) = happyShift action_19
action_41 (40) = happyShift action_20
action_41 (43) = happyShift action_21
action_41 (10) = happyGoto action_6
action_41 (11) = happyGoto action_28
action_41 _ = happyFail

action_42 (14) = happyShift action_45
action_42 (15) = happyShift action_29
action_42 (16) = happyShift action_30
action_42 (17) = happyShift action_31
action_42 (19) = happyShift action_8
action_42 (21) = happyShift action_9
action_42 (25) = happyShift action_10
action_42 (27) = happyShift action_11
action_42 (30) = happyShift action_12
action_42 (31) = happyShift action_13
action_42 (32) = happyShift action_14
action_42 (35) = happyShift action_15
action_42 (36) = happyShift action_16
action_42 (37) = happyShift action_17
action_42 (38) = happyShift action_18
action_42 (39) = happyShift action_19
action_42 (40) = happyShift action_20
action_42 (43) = happyShift action_21
action_42 (10) = happyGoto action_6
action_42 (11) = happyGoto action_28
action_42 _ = happyFail

action_43 (15) = happyShift action_29
action_43 (16) = happyShift action_30
action_43 (17) = happyShift action_31
action_43 (19) = happyShift action_8
action_43 (20) = happyShift action_44
action_43 (21) = happyShift action_9
action_43 (25) = happyShift action_10
action_43 (27) = happyShift action_11
action_43 (30) = happyShift action_12
action_43 (31) = happyShift action_13
action_43 (32) = happyShift action_14
action_43 (35) = happyShift action_15
action_43 (36) = happyShift action_16
action_43 (37) = happyShift action_17
action_43 (38) = happyShift action_18
action_43 (39) = happyShift action_19
action_43 (40) = happyShift action_20
action_43 (43) = happyShift action_21
action_43 (10) = happyGoto action_6
action_43 (11) = happyGoto action_28
action_43 _ = happyFail

action_44 _ = happyReduce_24

action_45 (19) = happyShift action_8
action_45 (21) = happyShift action_9
action_45 (25) = happyShift action_10
action_45 (27) = happyShift action_11
action_45 (30) = happyShift action_12
action_45 (31) = happyShift action_13
action_45 (32) = happyShift action_14
action_45 (35) = happyShift action_15
action_45 (36) = happyShift action_16
action_45 (37) = happyShift action_17
action_45 (38) = happyShift action_18
action_45 (39) = happyShift action_19
action_45 (40) = happyShift action_20
action_45 (43) = happyShift action_21
action_45 (10) = happyGoto action_64
action_45 (11) = happyGoto action_23
action_45 _ = happyFail

action_46 (19) = happyShift action_8
action_46 (21) = happyShift action_9
action_46 (25) = happyShift action_10
action_46 (27) = happyShift action_11
action_46 (30) = happyShift action_12
action_46 (31) = happyShift action_13
action_46 (32) = happyShift action_14
action_46 (35) = happyShift action_15
action_46 (36) = happyShift action_16
action_46 (37) = happyShift action_17
action_46 (38) = happyShift action_18
action_46 (39) = happyShift action_19
action_46 (40) = happyShift action_20
action_46 (43) = happyShift action_21
action_46 (10) = happyGoto action_63
action_46 (11) = happyGoto action_23
action_46 _ = happyFail

action_47 (19) = happyShift action_8
action_47 (21) = happyShift action_9
action_47 (25) = happyShift action_10
action_47 (27) = happyShift action_11
action_47 (30) = happyShift action_12
action_47 (31) = happyShift action_13
action_47 (32) = happyShift action_14
action_47 (35) = happyShift action_15
action_47 (36) = happyShift action_16
action_47 (37) = happyShift action_17
action_47 (38) = happyShift action_18
action_47 (39) = happyShift action_19
action_47 (40) = happyShift action_20
action_47 (43) = happyShift action_21
action_47 (10) = happyGoto action_62
action_47 (11) = happyGoto action_23
action_47 _ = happyFail

action_48 (38) = happyShift action_61
action_48 _ = happyFail

action_49 (12) = happyShift action_60
action_49 _ = happyFail

action_50 (15) = happyShift action_57
action_50 (16) = happyShift action_58
action_50 (18) = happyShift action_59
action_50 _ = happyReduce_11

action_51 (19) = happyShift action_51
action_51 (28) = happyShift action_5
action_51 (29) = happyShift action_52
action_51 (7) = happyGoto action_56
action_51 _ = happyFail

action_52 _ = happyReduce_5

action_53 (15) = happyShift action_29
action_53 (16) = happyShift action_30
action_53 (17) = happyShift action_31
action_53 (19) = happyShift action_8
action_53 (21) = happyShift action_9
action_53 (25) = happyShift action_10
action_53 (27) = happyShift action_11
action_53 (30) = happyShift action_12
action_53 (31) = happyShift action_13
action_53 (32) = happyShift action_14
action_53 (35) = happyShift action_15
action_53 (36) = happyShift action_16
action_53 (37) = happyShift action_17
action_53 (38) = happyShift action_18
action_53 (39) = happyShift action_19
action_53 (40) = happyShift action_20
action_53 (43) = happyShift action_21
action_53 (10) = happyGoto action_6
action_53 (11) = happyGoto action_28
action_53 _ = happyReduce_20

action_54 (15) = happyShift action_29
action_54 (17) = happyShift action_31
action_54 (19) = happyShift action_8
action_54 (21) = happyShift action_9
action_54 (25) = happyShift action_10
action_54 (27) = happyShift action_11
action_54 (30) = happyShift action_12
action_54 (31) = happyShift action_13
action_54 (32) = happyShift action_14
action_54 (35) = happyShift action_15
action_54 (36) = happyShift action_16
action_54 (37) = happyShift action_17
action_54 (38) = happyShift action_18
action_54 (39) = happyShift action_19
action_54 (40) = happyShift action_20
action_54 (43) = happyShift action_21
action_54 (10) = happyGoto action_6
action_54 (11) = happyGoto action_28
action_54 _ = happyReduce_18

action_55 (17) = happyShift action_31
action_55 (19) = happyShift action_8
action_55 (21) = happyShift action_9
action_55 (25) = happyShift action_10
action_55 (27) = happyShift action_11
action_55 (30) = happyShift action_12
action_55 (31) = happyShift action_13
action_55 (32) = happyShift action_14
action_55 (35) = happyShift action_15
action_55 (36) = happyShift action_16
action_55 (37) = happyShift action_17
action_55 (38) = happyShift action_18
action_55 (39) = happyShift action_19
action_55 (40) = happyShift action_20
action_55 (43) = happyShift action_21
action_55 (10) = happyGoto action_6
action_55 (11) = happyGoto action_28
action_55 _ = happyReduce_19

action_56 (15) = happyShift action_57
action_56 (16) = happyShift action_58
action_56 (18) = happyShift action_59
action_56 (20) = happyShift action_72
action_56 _ = happyFail

action_57 (19) = happyShift action_51
action_57 (28) = happyShift action_5
action_57 (29) = happyShift action_52
action_57 (7) = happyGoto action_71
action_57 _ = happyFail

action_58 (19) = happyShift action_51
action_58 (28) = happyShift action_5
action_58 (29) = happyShift action_52
action_58 (7) = happyGoto action_70
action_58 _ = happyFail

action_59 (19) = happyShift action_51
action_59 (28) = happyShift action_5
action_59 (29) = happyShift action_52
action_59 (7) = happyGoto action_69
action_59 _ = happyFail

action_60 (19) = happyShift action_51
action_60 (28) = happyShift action_5
action_60 (29) = happyShift action_52
action_60 (7) = happyGoto action_68
action_60 _ = happyFail

action_61 (25) = happyShift action_67
action_61 _ = happyFail

action_62 (15) = happyShift action_29
action_62 (16) = happyShift action_30
action_62 (17) = happyShift action_31
action_62 (19) = happyShift action_8
action_62 (21) = happyShift action_9
action_62 (25) = happyShift action_10
action_62 (27) = happyShift action_11
action_62 (30) = happyShift action_12
action_62 (31) = happyShift action_13
action_62 (32) = happyShift action_14
action_62 (35) = happyShift action_15
action_62 (36) = happyShift action_16
action_62 (37) = happyShift action_17
action_62 (38) = happyShift action_18
action_62 (39) = happyShift action_19
action_62 (40) = happyShift action_20
action_62 (43) = happyShift action_21
action_62 (10) = happyGoto action_6
action_62 (11) = happyGoto action_28
action_62 _ = happyReduce_32

action_63 (15) = happyShift action_29
action_63 (16) = happyShift action_30
action_63 (17) = happyShift action_31
action_63 (19) = happyShift action_8
action_63 (21) = happyShift action_9
action_63 (25) = happyShift action_10
action_63 (27) = happyShift action_11
action_63 (30) = happyShift action_12
action_63 (31) = happyShift action_13
action_63 (32) = happyShift action_14
action_63 (34) = happyShift action_66
action_63 (35) = happyShift action_15
action_63 (36) = happyShift action_16
action_63 (37) = happyShift action_17
action_63 (38) = happyShift action_18
action_63 (39) = happyShift action_19
action_63 (40) = happyShift action_20
action_63 (43) = happyShift action_21
action_63 (10) = happyGoto action_6
action_63 (11) = happyGoto action_28
action_63 _ = happyFail

action_64 (15) = happyShift action_29
action_64 (16) = happyShift action_30
action_64 (17) = happyShift action_31
action_64 (19) = happyShift action_8
action_64 (21) = happyShift action_9
action_64 (22) = happyShift action_65
action_64 (25) = happyShift action_10
action_64 (27) = happyShift action_11
action_64 (30) = happyShift action_12
action_64 (31) = happyShift action_13
action_64 (32) = happyShift action_14
action_64 (35) = happyShift action_15
action_64 (36) = happyShift action_16
action_64 (37) = happyShift action_17
action_64 (38) = happyShift action_18
action_64 (39) = happyShift action_19
action_64 (40) = happyShift action_20
action_64 (43) = happyShift action_21
action_64 (10) = happyGoto action_6
action_64 (11) = happyGoto action_28
action_64 _ = happyFail

action_65 _ = happyReduce_25

action_66 (19) = happyShift action_8
action_66 (21) = happyShift action_9
action_66 (25) = happyShift action_10
action_66 (27) = happyShift action_11
action_66 (30) = happyShift action_12
action_66 (31) = happyShift action_13
action_66 (32) = happyShift action_14
action_66 (35) = happyShift action_15
action_66 (36) = happyShift action_16
action_66 (37) = happyShift action_17
action_66 (38) = happyShift action_18
action_66 (39) = happyShift action_19
action_66 (40) = happyShift action_20
action_66 (43) = happyShift action_21
action_66 (10) = happyGoto action_74
action_66 (11) = happyGoto action_23
action_66 _ = happyFail

action_67 (42) = happyShift action_73
action_67 _ = happyFail

action_68 (15) = happyShift action_57
action_68 (16) = happyShift action_58
action_68 (18) = happyShift action_59
action_68 _ = happyReduce_12

action_69 (15) = happyShift action_57
action_69 (16) = happyShift action_58
action_69 (18) = happyShift action_59
action_69 _ = happyReduce_8

action_70 (15) = happyShift action_57
action_70 _ = happyReduce_7

action_71 _ = happyReduce_6

action_72 _ = happyReduce_9

action_73 (19) = happyShift action_8
action_73 (21) = happyShift action_9
action_73 (25) = happyShift action_10
action_73 (27) = happyShift action_11
action_73 (30) = happyShift action_12
action_73 (31) = happyShift action_13
action_73 (32) = happyShift action_14
action_73 (35) = happyShift action_15
action_73 (36) = happyShift action_16
action_73 (37) = happyShift action_17
action_73 (38) = happyShift action_18
action_73 (39) = happyShift action_19
action_73 (40) = happyShift action_20
action_73 (43) = happyShift action_21
action_73 (10) = happyGoto action_75
action_73 (11) = happyGoto action_23
action_73 _ = happyFail

action_74 (15) = happyShift action_29
action_74 (16) = happyShift action_30
action_74 (17) = happyShift action_31
action_74 (19) = happyShift action_8
action_74 (21) = happyShift action_9
action_74 (25) = happyShift action_10
action_74 (27) = happyShift action_11
action_74 (30) = happyShift action_12
action_74 (31) = happyShift action_13
action_74 (32) = happyShift action_14
action_74 (35) = happyShift action_15
action_74 (36) = happyShift action_16
action_74 (37) = happyShift action_17
action_74 (38) = happyShift action_18
action_74 (39) = happyShift action_19
action_74 (40) = happyShift action_20
action_74 (43) = happyShift action_21
action_74 (10) = happyGoto action_6
action_74 (11) = happyGoto action_28
action_74 _ = happyReduce_23

action_75 (13) = happyShift action_76
action_75 (15) = happyShift action_29
action_75 (16) = happyShift action_30
action_75 (17) = happyShift action_31
action_75 (19) = happyShift action_8
action_75 (21) = happyShift action_9
action_75 (25) = happyShift action_10
action_75 (27) = happyShift action_11
action_75 (30) = happyShift action_12
action_75 (31) = happyShift action_13
action_75 (32) = happyShift action_14
action_75 (35) = happyShift action_15
action_75 (36) = happyShift action_16
action_75 (37) = happyShift action_17
action_75 (38) = happyShift action_18
action_75 (39) = happyShift action_19
action_75 (40) = happyShift action_20
action_75 (43) = happyShift action_21
action_75 (10) = happyGoto action_6
action_75 (11) = happyGoto action_28
action_75 _ = happyFail

action_76 (39) = happyShift action_77
action_76 _ = happyFail

action_77 (25) = happyShift action_78
action_77 _ = happyFail

action_78 (42) = happyShift action_79
action_78 _ = happyFail

action_79 (19) = happyShift action_8
action_79 (21) = happyShift action_9
action_79 (25) = happyShift action_10
action_79 (27) = happyShift action_11
action_79 (30) = happyShift action_12
action_79 (31) = happyShift action_13
action_79 (32) = happyShift action_14
action_79 (35) = happyShift action_15
action_79 (36) = happyShift action_16
action_79 (37) = happyShift action_17
action_79 (38) = happyShift action_18
action_79 (39) = happyShift action_19
action_79 (40) = happyShift action_20
action_79 (43) = happyShift action_21
action_79 (10) = happyGoto action_80
action_79 (11) = happyGoto action_23
action_79 _ = happyFail

action_80 (15) = happyShift action_29
action_80 (16) = happyShift action_30
action_80 (17) = happyShift action_31
action_80 (19) = happyShift action_8
action_80 (21) = happyShift action_9
action_80 (25) = happyShift action_10
action_80 (27) = happyShift action_11
action_80 (30) = happyShift action_12
action_80 (31) = happyShift action_13
action_80 (32) = happyShift action_14
action_80 (35) = happyShift action_15
action_80 (36) = happyShift action_16
action_80 (37) = happyShift action_17
action_80 (38) = happyShift action_18
action_80 (39) = happyShift action_19
action_80 (40) = happyShift action_20
action_80 (43) = happyShift action_21
action_80 (10) = happyGoto action_6
action_80 (11) = happyGoto action_28
action_80 _ = happyReduce_30

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 _
	 =  HappyAbsSyn7
		 (T_nat
	)

happyReduce_5 = happySpecReduce_1  7 happyReduction_5
happyReduction_5 _
	 =  HappyAbsSyn7
		 (T_bool
	)

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (T_prod happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  7 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (T_sum happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  7 happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (T_arrow happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  7 happyReduction_9
happyReduction_9 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_0  8 happyReduction_10
happyReduction_10  =  HappyAbsSyn8
		 ([]
	)

happyReduce_11 = happySpecReduce_3  8 happyReduction_11
happyReduction_11 (HappyAbsSyn7  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (let IDENT _ s = happy_var_1 in [(s, happy_var_3)]
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happyReduce 5 8 happyReduction_12
happyReduction_12 ((HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (let IDENT _ s = happy_var_3 in happy_var_1 ++ [(s, happy_var_5)]
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_1  9 happyReduction_13
happyReduction_13 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (progToPcfTerm $ makeApps happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  10 happyReduction_14
happyReduction_14 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2  10 happyReduction_15
happyReduction_15 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  11 happyReduction_16
happyReduction_16 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (let IDENT p s = happy_var_1 in Pvar s
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  11 happyReduction_17
happyReduction_17 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (let NUMBER p n = happy_var_1 in Pnum n
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  11 happyReduction_18
happyReduction_18 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (Pop (makeApps happy_var_1) (makeApps happy_var_3)
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  11 happyReduction_19
happyReduction_19 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (Pop (makeApps happy_var_1) (makeApps happy_var_3)
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  11 happyReduction_20
happyReduction_20 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (Peqtest (makeApps happy_var_1) (makeApps happy_var_3)
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  11 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn11
		 (Ptrue
	)

happyReduce_22 = happySpecReduce_1  11 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn11
		 (Pfalse
	)

happyReduce_23 = happyReduce 6 11 happyReduction_23
happyReduction_23 ((HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (Pif (makeApps happy_var_2) (makeApps happy_var_4) (makeApps happy_var_6)
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_3  11 happyReduction_24
happyReduction_24 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (makeApps happy_var_2
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happyReduce 5 11 happyReduction_25
happyReduction_25 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (Ppair (makeApps happy_var_2) (makeApps happy_var_4)
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_2  11 happyReduction_26
happyReduction_26 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (Pprojfst happy_var_2
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_2  11 happyReduction_27
happyReduction_27 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (Pprojsnd happy_var_2
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  11 happyReduction_28
happyReduction_28 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (Pinjfst happy_var_2
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2  11 happyReduction_29
happyReduction_29 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (Pinjsnd happy_var_2
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happyReduce 12 11 happyReduction_30
happyReduction_30 ((HappyAbsSyn10  happy_var_12) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (let IDENT _ s1 = happy_var_5 in let IDENT _ s2 = happy_var_10 in 
	        Pcase (makeApps happy_var_2) (s1, makeApps happy_var_7) (s2, makeApps happy_var_12)
	) `HappyStk` happyRest

happyReduce_31 = happySpecReduce_2  11 happyReduction_31
happyReduction_31 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (Pfix happy_var_2
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happyReduce 4 11 happyReduction_32
happyReduction_32 ((HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (let IDENT _ s = happy_var_2 in Pabs s (makeApps happy_var_4)
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 46 46 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	COLON happy_dollar_dollar -> cont 12;
	SEMICOLON happy_dollar_dollar -> cont 13;
	COMMA happy_dollar_dollar -> cont 14;
	STAR happy_dollar_dollar -> cont 15;
	PLUS happy_dollar_dollar -> cont 16;
	EQUAL happy_dollar_dollar -> cont 17;
	ARROW happy_dollar_dollar -> cont 18;
	LPAREN happy_dollar_dollar -> cont 19;
	RPAREN happy_dollar_dollar -> cont 20;
	LPAIR happy_dollar_dollar -> cont 21;
	RPAIR happy_dollar_dollar -> cont 22;
	LBOX happy_dollar_dollar -> cont 23;
	RBOX happy_dollar_dollar -> cont 24;
	IDENT p s -> cont 25;
	IDENTCAP p s -> cont 26;
	NUMBER p n -> cont 27;
	NAT happy_dollar_dollar -> cont 28;
	BOOL happy_dollar_dollar -> cont 29;
	TRUE happy_dollar_dollar -> cont 30;
	FALSE happy_dollar_dollar -> cont 31;
	IF happy_dollar_dollar -> cont 32;
	THEN happy_dollar_dollar -> cont 33;
	ELSE happy_dollar_dollar -> cont 34;
	FUNC happy_dollar_dollar -> cont 35;
	PROJFST happy_dollar_dollar -> cont 36;
	PROJSND happy_dollar_dollar -> cont 37;
	INJFST happy_dollar_dollar -> cont 38;
	INJSND happy_dollar_dollar -> cont 39;
	CASE happy_dollar_dollar -> cont 40;
	OF happy_dollar_dollar -> cont 41;
	IN happy_dollar_dollar -> cont 42;
	FIX happy_dollar_dollar -> cont 43;
	FCEND -> cont 44;
	FCSTART -> cont 45;
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

pcfProgramParsing tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn9 z -> happyReturn z; _other -> notHappyAtAll })

pcfContextParsing tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn8 z -> happyReturn z; _other -> notHappyAtAll })

pcfProgs tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn10 z -> happyReturn z; _other -> notHappyAtAll })

pcfArg tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn11 z -> happyReturn z; _other -> notHappyAtAll })

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
