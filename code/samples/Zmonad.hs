module Zmonad
       ( EM (..) -- Exception monad 
       , FM (..) -- Fresh monad
       ) where 

import Control.Monad

data EM t = OK t | Fail String 
          deriving (Show) 

instance Monad EM where 
  return x = OK x
  c >>= f = case c of OK x -> f x
                      Fail s -> Fail s
                      
abort :: String -> EM t 
abort s = Fail s

emAdd :: Int -> Int -> EM Int
emAdd x y = OK (x + y)

emSub :: Int -> Int -> EM Int
emSub x y = OK (x - y) 

emMult :: Int -> Int -> EM Int 
emMult x y = OK (x * y) 

emDiv :: Int -> Int -> EM Int
emDiv x y = if y == 0 then abort "Divide by zero!" else OK (x `div` y) 

arithCalc1 = do x <- emAdd 1 3
                y <- emSub 4 4 
                emMult x y 

arithCalc2 = do x <- emAdd 1 3
                y <- emSub 4 4 
                emDiv x y 


newtype FM t = FM {runFM :: Int -> (Int, t)}

instance Monad (FM) where 
  return x = FM $ \i -> (i, x)
  c >>= g = FM $ \i -> let (i', x) = runFM c i 
                       in runFM (g x) i' 
  
execFM :: FM t -> t
execFM c = snd $ runFM c 0
  
newFresh :: FM Int
newFresh = FM $ \i -> (i+1, i)

data Tm = Var String | New | X Int
        deriving (Eq, Show)

parseTm :: [Tm] -> FM [Tm] 
parseTm [] = return []
parseTm (x : xs) = if x == New 
                   then do r <- newFresh 
                           rs <- parseTm xs 
                           return (X r : rs) 
                   else do rs <- parseTm xs 
                           return (x : rs)    
                           
test = [ Var "x", New, Var "y", Var "z", New, New, Var "w" ]                        