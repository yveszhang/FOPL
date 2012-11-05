module WhileLang 
       ( WExp (..) -- WHILE expression
       , WProg (..) -- WHILE program
       , WState (..)
       , WVal (..)
       , execute 
       , getValue
       ) where 

import Control.Monad

data WVal = Wnat Int
          | Wbool Bool
          | Nav 
          deriving (Show, Eq)

data WExp = Wvar String -- variable 
          | Wconst WVal
          | Wadd WExp WExp -- addition 
          | Wmult WExp WExp -- multiplication
          | Weq WExp WExp -- equality test
          | Wnot WExp -- 
          | Wlt WExp WExp -- 
          | Wgt WExp WExp -- 
          deriving (Show) 

data WProg = Wassign String WExp -- assignment 
           | Wif WExp WProg WProg -- if .. then .. else ..
           | Wseq WProg WProg -- sequential 
           | Wwhile WExp WProg -- while-loop
           deriving (Show)

type WState = [(String, WVal)]

getValue :: String -> WState -> Maybe WVal
getValue x [] = Nothing 
getValue x ((x', v) : ss) = if x == x' then Just v else getValue x ss

update :: String -> WVal -> WState -> WState
update x v [] = [(x, v)]
update x v ((x', v') : ss) = 
  if x == x' then (x, v) : ss else (x', v') : update x v ss

evaluate :: WExp -> WState -> Maybe WVal
evaluate (Wvar x) s = getValue x s 
evaluate (Wconst v) s = Just v
-- evaluate (Wadd e1 e2) s = case (evaluate e1 s) 
--                           of Nothing -> Nothing 
--                              Just v1 -> case (evaluate e2 s) 
--                                         of Nothing -> Nothing 
--                                            Just v2 -> case (v1, v2) 
--                                                       of (Wnat n1, Wnat n2) -> Just (Wnat (n1+n2))
--                                                          _ -> Nothing 
-- evaluate (Wmult e1 e2) s = case (evaluate e1 s) 
--                            of Nothing -> Nothing 
--                               Just v1 -> case (evaluate e2 s) 
--                                          of Nothing -> Nothing 
--                                             Just v2 -> case (v1, v2) 
--                                                        of (Wnat n1, Wnat n2) -> Just(Wnat (n1*n2))
--                                                           _ -> Nothing 
-- evaluate (Weq e1 e2) s = case (evaluate e1 s) 
--                          of Nothing -> Nothing 
--                             Just v1 -> case (evaluate e2 s) 
--                                        of Nothing -> Nothing 
--                                           Just v2 -> case (v1, v2) 
--                                                      of (Wnat n1, Wnat n2) -> Just(Wbool (n1 == n2) )
--                                                         _ -> Nothing 
-- evaluate (Wlt e1 e2) s = case (evaluate e1 s) 
--                          of Nothing -> Nothing 
--                             Just v1 -> case (evaluate e2 s) 
--                                        of Nothing -> Nothing 
--                                           Just v2 -> case (v1, v2) 
--                                                      of (Wnat n1, Wnat n2) -> Just(Wbool (n1 < n2) )
--                                                         _ -> Nothing 
-- evaluate (Wgt e1 e2) s = case (evaluate e1 s) 
--                          of Nothing -> Nothing 
--                             Just v1 -> case (evaluate e2 s) 
--                                        of Nothing -> Nothing 
--                                           Just v2 -> case (v1, v2) 
--                                                      of (Wnat n1, Wnat n2) -> Just(Wbool (n1 > n2) )
--                                                         _ -> Nothing 
-- evaluate (Wnot e) s = case (evaluate e s) 
--                      of Just (Wbool b) -> Just (Wbool (not b))
--                         _ -> Nothing 
evaluate (Wadd e1 e2) s = do v1 <- evaluate e1 s
                             v2 <- evaluate e2 s
                             case (v1, v2) of (Wnat n1, Wnat n2) -> Just (Wnat (n1+n2))
                                              _ -> Nothing 
evaluate (Wmult e1 e2) s = do v1 <- evaluate e1 s
                              v2 <- evaluate e2 s
                              case (v1, v2) of (Wnat n1, Wnat n2) -> Just(Wnat (n1*n2))
                                               _ -> Nothing 
evaluate (Weq e1 e2) s = do v1 <- evaluate e1 s
                            v2 <- evaluate e2 s
                            case (v1, v2) of (Wnat n1, Wnat n2) -> Just(Wbool (n1 == n2) )
                                             _ -> Nothing 
evaluate (Wlt e1 e2) s = do v1 <- evaluate e1 s
                            v2 <- evaluate e2 s
                            case (v1, v2) of (Wnat n1, Wnat n2) -> Just(Wbool (n1 < n2) )
                                             _ -> Nothing 
evaluate (Wgt e1 e2) s = do v1 <- evaluate e1 s
                            v2 <- evaluate e2 s
                            case (v1, v2) of (Wnat n1, Wnat n2) -> Just(Wbool (n1 > n2) )
                                             _ -> Nothing 
evaluate (Wnot e) s = do v <- evaluate e s
                         case v of Wbool b -> Just (Wbool (not b))
                                   _ -> Nothing 

execute :: WProg -> WState -> WState 
execute (Wassign x e) s = case (evaluate e s) 
                          of Nothing -> s
                             Just v -> update x v s 
execute (Wif e p1 p2) s = case (evaluate e s) 
                          of Just (Wbool b) -> if b then execute p1 s else execute p2 s
                             _ -> s
execute (Wseq p1 p2) s = let s1 = execute p1 s in execute p2 s1
execute (Wwhile e p) s = case (evaluate e s) 
                         of Just (Wbool b) -> if b then execute (Wseq p (Wwhile e p)) s else s
                            _ -> s
                         
