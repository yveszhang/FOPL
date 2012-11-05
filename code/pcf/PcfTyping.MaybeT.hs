module PcfTyping  -- Define type substitutions as functions 
       ( PcfType (..)
       , PcfTerm (..)
       , TypeTemp (..)
       , TypeContext
       , typeUnif
       , typeInfer
       ) where 

import Data.List
import Control.Monad
import Control.Monad.Trans

-- type for PCF types
data PcfType = T_nat
             | T_bool
             | T_prod PcfType PcfType  -- product type
             | T_sum PcfType PcfType   -- sum type 
             | T_arrow PcfType PcfType -- function type
             deriving (Eq)
               
-- type for PCF terms
data PcfTerm = TM_var String                   -- program variable, with a string denoting the variable name
             | TM_num Int                      -- Int constants 
             | TM_op PcfTerm PcfTerm           -- Int operations
             | TM_true                  
             | TM_false 
             | TM_eqtest PcfTerm PcfTerm       -- equality tests 
             | TM_if PcfTerm PcfTerm PcfTerm   -- conditionals
             | TM_pair PcfTerm PcfTerm         -- pairs
             | TM_projfst PcfTerm              -- projections
             | TM_projsnd PcfTerm 
             | TM_injfst PcfTerm               -- injections
             | TM_injsnd PcfTerm
             | TM_case PcfTerm (String, PcfTerm) (String, PcfTerm) -- case distinctions 
             | TM_abs String PcfTerm           -- lambda abstractions (functions), with string for the name of arguments
             | TM_app PcfTerm PcfTerm          -- lambda applications
             | TM_fix PcfTerm                  -- fix point
             deriving (Eq, Show)
                      
-- type for PCF templates
data TypeTemp = TT_nat
              | TT_bool
              | TT_prod TypeTemp TypeTemp   -- products
              | TT_sum TypeTemp TypeTemp    -- sums
              | TT_arrow TypeTemp TypeTemp  -- functions
              | TT_var Int                  -- type variables
              deriving (Eq)

type TypeContext = [(String, PcfType)]
type TempContext = [(String, TypeTemp)] 
type TypeAssign = Int -> TypeTemp -- type substitutions (unifier) as functions from type variables (indices) to templates

constTypes = [T_nat, T_bool]
constTemps = [TT_nat, TT_bool]

instance Show (PcfType) where 
  show T_nat = "Nat" 
  show T_bool = "Bool" 
  show (T_prod t1 t2) =
    case (t1, t2) of _ | t1 `elem` constTypes && t2 `elem` constTypes -> show t1 ++ " * " ++ show t2
                       | t1 `elem` constTypes -> show t1 ++ " * (" ++ show t2 ++ ")"
                       | t2 `elem` constTypes -> "(" ++ show t1 ++ ") * " ++ show t2
                       | otherwise -> "(" ++ show t1 ++ ") * (" ++ show t2 ++ ")"
  show (T_sum t1 t2) =
    case (t1, t2) of _ | t1 `elem` constTypes && t2 `elem` constTypes -> show t1 ++ " + " ++ show t2
                       | t1 `elem` constTypes -> show t1 ++ " + (" ++ show t2 ++ ")"
                       | t2 `elem` constTypes -> "(" ++ show t1 ++ ") + " ++ show t2
                       | otherwise -> "(" ++ show t1 ++ ") + (" ++ show t2 ++ ")"
  show (T_arrow t1 t2) = 
    case (t1, t2) of (T_arrow _ _, _) -> "(" ++ show t1 ++ ") -> " ++ show t2
                     _ -> show t1 ++ " -> " ++ show t2
  
instance Show (TypeTemp) where 
  show TT_nat = "Nat" 
  show TT_bool = "Bool" 
  show (TT_var n) = "t" ++ show n
  show (TT_prod t1 t2) =
    case (t1, t2) of (TT_var _, TT_var _) -> show t1 ++ " * " ++ show t2                     
                     (TT_var _, _) | t2 `elem` constTemps -> show t1 ++ " * " ++ show t2
                                   | otherwise -> show t1 ++ " * (" ++ show t2 ++ ")"
                     (_, TT_var _) | t1 `elem` constTemps -> show t1 ++ " * " ++ show t2 
                                   | otherwise -> "(" ++ show t1 ++ ") * " ++ show t2
                     _ | t1 `elem` constTemps && t2 `elem` constTemps -> show t1 ++ " * " ++ show t2
                       | t1 `elem` constTemps -> show t1 ++ " * (" ++ show t2 ++ ")"
                       | t2 `elem` constTemps -> "(" ++ show t1 ++ ") * " ++ show t2
                       | otherwise -> "(" ++ show t1 ++ ") * (" ++ show t2 ++ ")"
  show (TT_sum t1 t2) =
    case (t1, t2) of (TT_var _, TT_var _) -> show t1 ++ " + " ++ show t2                     
                     (TT_var _, _) | t2 `elem` constTemps -> show t1 ++ " + " ++ show t2
                                   | otherwise -> show t1 ++ " + (" ++ show t2 ++ ")"
                     (_, TT_var _) | t1 `elem` constTemps -> show t1 ++ " + " ++ show t2 
                                   | otherwise -> "(" ++ show t1 ++ ") + " ++ show t2
                     _ | t1 `elem` constTemps && t2 `elem` constTemps -> show t1 ++ " + " ++ show t2
                       | t1 `elem` constTemps -> show t1 ++ " + (" ++ show t2 ++ ")"
                       | t2 `elem` constTemps -> "(" ++ show t1 ++ ") + " ++ show t2
                       | otherwise -> "(" ++ show t1 ++ ") + (" ++ show t2 ++ ")"
  show (TT_arrow t1 t2) = 
    case (t1, t2) of (TT_arrow _ _, _) -> "(" ++ show t1 ++ ") -> " ++ show t2
                     _ -> show t1 ++ " -> " ++ show t2

-- Fresh variable monad
newtype FV t = FV {runFV :: Int -> (Int, t)}

instance Monad FV where 
  return x = FV (\i -> (i, x))
  c1 >>= c2 = FV $ \i -> let (i', x) = runFV c1 i in runFV (c2 x) i'
                            
getFreshVar :: FV TypeTemp
getFreshVar = FV $ \i -> (i+1, TT_var i)

execFV :: FV t -> t
execFV c = snd $ runFV c 0 

newtype MaybeT m t = MaybeT {runMaybeT :: m (Maybe t)}

instance Monad m => Monad (MaybeT m) where 
  return x = MaybeT $ return (Just x) 
  c >>= f = MaybeT $ do x <- runMaybeT c
                        case x of Nothing -> return Nothing 
                                  Just y -> runMaybeT (f y) 

instance MonadTrans MaybeT where 
  lift c = MaybeT $ c >>= (return . Just) 
--  lift c = MaybeT $ liftM Just c 

-- identity assignment (type substitution)
taID :: TypeAssign
taID n = TT_var n
  
-- transform function from PcfType to PcfTemp
pcfTypeToTemp :: PcfType -> TypeTemp  
pcfTypeToTemp T_nat = TT_nat
pcfTypeToTemp T_bool = TT_bool
pcfTypeToTemp (T_prod t1 t2) = TT_prod (pcfTypeToTemp t1) (pcfTypeToTemp t2)
pcfTypeToTemp (T_sum t1 t2) = TT_sum (pcfTypeToTemp t1) (pcfTypeToTemp t2)
pcfTypeToTemp (T_arrow t1 t2) = TT_arrow (pcfTypeToTemp t1) (pcfTypeToTemp t2)
  
-- all the type variables (indices) in a type template                                
typeVars :: TypeTemp -> [Int]
typeVars TT_nat = []
typeVars TT_bool = []
typeVars (TT_prod t1 t2) = typeVars t1 ++ typeVars t2
typeVars (TT_sum t1 t2) = typeVars t1 ++ typeVars t2
typeVars (TT_arrow t1 t2) = typeVars t1 ++ typeVars t2
typeVars (TT_var n) = [n]

-- apply a type substitution to a type template
assignTemp :: TypeTemp -> TypeAssign -> TypeTemp
assignTemp TT_nat _ = TT_nat 
assignTemp TT_bool _ = TT_bool 
assignTemp (TT_prod t1 t2) s = TT_prod (assignTemp t1 s) (assignTemp t2 s) 
assignTemp (TT_sum t1 t2) s = TT_sum (assignTemp t1 s) (assignTemp t2 s) 
assignTemp (TT_arrow t1 t2) s = TT_arrow (assignTemp t1 s) (assignTemp t2 s) 
assignTemp (TT_var n) s = s n 

-- apply a type substitution to a context of templates
assignContext :: TempContext -> TypeAssign -> TempContext
assignContext ctx as = map (\(str, t) -> (str, assignTemp t as)) ctx 

-- composition of type substitutions
assignCompose :: TypeAssign -> TypeAssign -> TypeAssign
assignCompose s1 s2 = \n -> assignTemp (s1 n) s2 
                          
-- build new substitution with new assignment of a template to a given type variable
updateAssign :: TypeAssign -> (Int, TypeTemp) -> TypeAssign
updateAssign s (n, t) = \m -> if n == m then t else s m 

-- unification algorithm (MGU)
typeUnif :: [(TypeTemp, TypeTemp)] -> Maybe TypeAssign
typeUnif [] = Just taID
typeUnif [(t1, t2)] = 
  case (t1, t2) of (TT_nat, TT_nat) -> Just taID
                   (TT_bool, TT_bool) -> Just taID
                   (TT_var n1, TT_var n2) -> 
                     if n1 == n2 
                     then Just taID 
                     else Just $ updateAssign taID (n1, TT_var n2)
                   (TT_var n, t) -> 
                     if n `elem` typeVars t 
                     then Nothing 
                     else Just $ updateAssign taID (n, t)
                   (t, TT_var n) -> 
                     if n `elem` typeVars t 
                     then Nothing 
                     else Just $ updateAssign taID (n, t) 
                   (TT_prod r1 r2, TT_prod s1 s2) -> typeUnif [(r1, s1), (r2, s2)]
                   (TT_sum r1 r2, TT_sum s1 s2) -> typeUnif [(r1, s1), (r2, s2)]
                   (TT_arrow r1 r2, TT_arrow s1 s2) -> typeUnif [(r1, s1), (r2, s2)]
                   _ -> Nothing 
typeUnif ((t1, t2) : ts) = 
  do ta <- typeUnif ts 
     ta'<- typeUnif [(assignTemp t1 ta, assignTemp t2 ta)] 
     return $ assignCompose ta ta'
  -- case typeUnif ts of Nothing -> Nothing 
  --                     Just ta -> 
  --                       case typeUnif [(assignTemp t1 ta, assignTemp t2 ta)] of 
  --                         Nothing -> Nothing 
  --                         Just ta' -> Just (assignCompose ta ta')
                          
-- function that, given a type variable (index) and a type substitution, 
-- returns the template assigned to the variable 
typeOfIndex :: Int -> TypeAssign -> TypeTemp
typeOfIndex n ta = ta n 

-- search type template of given free (program) variable in a context  
tempOfVar :: String -> TempContext -> Maybe TypeTemp
tempOfVar s [] = Nothing 
tempOfVar s ((s', t) : ts) = if s == s' then Just t else tempOfVar s ts

-- type inference algorithm in the MaybeT FV monad
typeInferTemp :: TempContext -> PcfTerm -> TypeTemp -> MaybeT FV TypeAssign
typeInferTemp ctx (TM_var str) t =
  MaybeT $ return $ do t' <- tempOfVar str ctx 
                       typeUnif [(t', t)] 
typeInferTemp ctx (TM_num _) t = 
  MaybeT $ return $ typeUnif [(TT_nat, t)] 
typeInferTemp ctx (TM_op e1 e2) t = 
  do as <- MaybeT $ return $ typeUnif [(t, TT_nat)] 
     let ctx1 = assignContext ctx as 
     as1 <- typeInferTemp ctx1 e1 TT_nat 
     let ctx2 = assignContext ctx1 as1 
     as2 <- typeInferTemp ctx2 e2 TT_nat 
     return $ assignCompose (assignCompose as as1) as2
typeInferTemp ctx TM_true t = 
  MaybeT $ return $ typeUnif [(TT_bool, t)] 
typeInferTemp ctx TM_false t = 
  MaybeT $ return $ typeUnif [(TT_bool, t)] 
typeInferTemp ctx (TM_eqtest e1 e2) t = 
  do as <- MaybeT $ return $ typeUnif [(TT_bool, t)] 
     let ctx1 = assignContext ctx as 
     as1 <- typeInferTemp ctx1 e1 TT_nat 
     let ctx2 = assignContext ctx1 as1 
     as2 <- typeInferTemp ctx2 e2 TT_nat 
     return $ assignCompose (assignCompose as as1) as2
typeInferTemp ctx (TM_if e0 e1 e2) t = 
  do as0 <- typeInferTemp ctx e0 TT_bool 
     let ctx1 = assignContext ctx as0
         t1 = assignTemp t as0
     as1 <- typeInferTemp ctx1 e1 t1 
     let ctx2 = assignContext ctx1 as1 
         t2 = assignTemp t1 as1 
     as2 <- typeInferTemp ctx2 e2 t2 
     return $ assignCompose (assignCompose as0 as1) as2
typeInferTemp ctx (TM_pair e1 e2) t = 
  do fvar1 <- lift getFreshVar 
     fvar2 <- lift getFreshVar
     as <- MaybeT $ return $ typeUnif [(t, TT_prod fvar1 fvar2)] 
     let ctx1 = assignContext ctx as 
         t1 = assignTemp fvar1 as 
     as1 <- typeInferTemp ctx1 e1 t1 
     let ctx2 = assignContext ctx1 as1 
         t2 = assignTemp fvar2 $ assignCompose as as1 
     as2 <- typeInferTemp ctx2 e2 t2 
     return $ assignCompose (assignCompose as as1) as2
typeInferTemp ctx (TM_projfst e) t = 
  do fvar <- lift getFreshVar
     typeInferTemp ctx e (TT_prod t fvar)
typeInferTemp ctx (TM_projsnd e) t = 
  do fvar <- lift getFreshVar
     typeInferTemp ctx e (TT_prod fvar t)
typeInferTemp ctx (TM_injfst e) t = 
  do fvar1 <- lift getFreshVar
     fvar2 <- lift getFreshVar 
     as <- MaybeT $ return $ typeUnif [(TT_sum fvar1 fvar2, t)] 
     let ctx' = assignContext ctx as 
         t' = assignTemp fvar1 as  
     as' <- typeInferTemp ctx' e t'
     return $ assignCompose as as'
typeInferTemp ctx (TM_injsnd e) t = 
  do fvar1 <- lift getFreshVar
     fvar2 <- lift getFreshVar
     as <- MaybeT $ return $ typeUnif [(TT_sum fvar1 fvar2, t)] 
     let ctx' = assignContext ctx as 
         t' = assignTemp fvar2 as  
     as' <- typeInferTemp ctx' e t' 
     return $ assignCompose as as'
typeInferTemp ctx (TM_case e0 (s1, e1) (s2, e2)) t =   
  do fvar1 <- lift getFreshVar
     fvar2 <- lift getFreshVar
     as0 <- typeInferTemp ctx e0 (TT_sum fvar1 fvar2) 
     let ctx1 = assignContext ((s1, fvar1) : ctx) as0 
         t1 = assignTemp t as0 
     as1 <- typeInferTemp ctx1 e1 t1 
     let ctx2 = assignContext ((s2, fvar2) : ctx) $ assignCompose as0 as1
         t2 = assignTemp t1 as1 
     as2 <- typeInferTemp ctx2 e2 t2 
     return $ assignCompose (assignCompose as0 as1) as2
typeInferTemp ctx (TM_abs str e) t =
  do fvar1 <- lift getFreshVar 
     fvar2 <- lift getFreshVar
     as <- MaybeT $ return $ typeUnif [(TT_arrow fvar1 fvar2, t)] 
     let ctx' = assignContext ((str, fvar1) : ctx) as 
         t' = assignTemp fvar2 as
     as' <- typeInferTemp ctx' e t'
     return $ assignCompose as as'
typeInferTemp ctx (TM_app e1 e2) t = 
  do fvar <- lift getFreshVar
     as1 <- typeInferTemp ctx e1 (TT_arrow fvar t) 
     let ctx2 = assignContext ctx as1
         t2 = assignTemp fvar as1 
     as2 <- typeInferTemp ctx2 e2 t2 
     return $ assignCompose as1 as2
typeInferTemp ctx (TM_fix e) t =           
  typeInferTemp ctx e (TT_arrow t t) 

typeInfer :: TypeContext -> PcfTerm -> Maybe TypeTemp
typeInfer ctx e = 
  execFV $ runMaybeT $  do fvar <- lift getFreshVar 
                           as <- typeInferTemp (map (\(s, t) -> (s, pcfTypeToTemp t)) ctx) e fvar 
                           return $ assignTemp fvar as 