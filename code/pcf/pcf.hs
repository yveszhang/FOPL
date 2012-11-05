module PCF
       ( PcfType 
       , getContext
       , getExpr
       , typeInferTest
       ) where 

import PcfTyping 
import PcfLexer
import PcfParser

getContext :: String -> E TypeContext
getContext = pcfContextParsing . pcfLexing

getExpr :: String -> E PcfTerm 
getExpr = pcfProgramParsing . pcfLexing

typeInferTest :: String -> String -> E TypeTemp
typeInferTest s_ctx s_exp = 
  case (getContext s_ctx, getExpr s_exp)  
  of (Ok ctx, Ok exp) -> case typeInfer ctx exp 
                         of Nothing -> Failed "Typing error!"
                            Just t -> Ok t
     (Failed s1, Failed s2) -> Failed (s1 ++ s2) 
     (Failed s, _) -> Failed s
     (_, Failed s) -> Failed s
     
testContext = "x : Nat; y : Bool; z : Nat + Bool; h : Nat -> Nat; g : Nat -> Bool; f : (Nat -> Bool) -> Nat -> Bool"

testTerms = [
   "1 + 2" 
  , "x * 2" 
  , "x * w" 
  , "1 == 2" 
  , "f (func f -> True)" 
  , "func xx -> y"
  , "x == h 2" 
  , "if y then x * 2 else h x" 
  , "if True then y else False" 
  , "if g (h x) then x == 2 else 3" 
  , "<x, y>"  
  , "<<h, h x>, g x>"
  , "projfst <<x, z>, y>" 
  , "projsnd <<y, y>, f> g x"
  , "projfst z"
  , "injfst <z, x>"
  , "injsnd (f g)" 
  , "case z of injfst x1 in x == x1; injsnd x2 in y"
  , "case injsnd <<x, z>, y> of injfst x1 in True; injsnd _ in False"
  , "func x -> injfst x"
  , "func g -> func x -> g x x"
  , "func x -> func x -> x"
  , "f (func w -> x == w) (x + 2)"
  , "f (func x -> y) x "
  , "fix f"
  , "fix f x" ]
            
test = 
  let printTyping ctx exp = case typeInferTest ctx exp 
                            of Failed s -> putStrLn $ exp ++ " : " ++ s
                               Ok t -> putStrLn $ exp ++ " : " ++ show t
  in do putStrLn $ "{ " ++ testContext ++  " }" 
        putStrLn "--------------------------------------------------------------------------------"
        mapM (printTyping testContext) testTerms