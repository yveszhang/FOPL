-- Functions 

mAdd :: Int -> Int -> Int -- Type signature for functions. 
-- Without the signature, GHC will try to infer the type and gives a more general one. 

mAdd x y = x + y 
-- check the type of mAdd in GHCi, with and without the type signature  
-- check type of "(mAdd 2)"
-- Functions with two arguments can be used as infix operator when wrapped in `...`. 
-- Run "2 `mAdd` 3" in GHCi

mAddUncurry = uncurry mAdd
-- Uncurried version of mAdd. Check the type of mAddUncurry
-- Check the type of "curry mAddUncurry"

mAddLargeInt :: Integer -> Integer -> Integer
-- Int is the machine-based integer type. 
-- The type Integer dose not have size limit.

mAddLargeInt x y = x + y 
-- Test the function with very large integer and compare with mAdd

--------------------------------------------------------------------------------

-- Define recursive functions

fact1 :: Integer  -> Integer
fact1 n = if n == 0 then 1 else n * fact1 (n-1) 
-- An alternative definition with notion for anonymous functions:
-- fact1 = \n -> if n == 0 then 1 else n * fact1 (n-1)

fact2 :: Int -> Int
-- Definition with pattern matching.
fact2 0 = 1
fact2 n = n * fact2 (n-1)

fact3 :: Int -> Maybe Int
-- Maybe is a predefined type constructor: Maybe a = Nothing | Just a 
-- This definition deals with negative parameters. 
fact3 0 = Just 1
fact3 n = if n < 0 
          then Nothing 
          else case fact3 (n-1) of Nothing -> Nothing 
                                   Just m -> Just (n * m)

fact4 :: Int -> Maybe Int
-- Definition with guards
fact4 n  | n == 0 = Just 1
         | n < 0 = Nothing 
         | otherwise = case fact4 (n-1) of Nothing -> Nothing 
                                           Just m -> Just (n * m)


-- Fibonacci number
fibonacci :: Int -> Int
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)
-- This is a direct implementation, but has very low efficiency (exponential run time).

fiboPair :: Int -> (Int, Int) 
-- each fiboPair computes two consecutive fibonacci numbers
fiboPair 0 = (1, 1)  
fiboPair n = let (p1, p2) = fiboPair (n - 1) 
                 x = p1 + p2
             in (x, p2+x)

fiboLinear :: Int -> Int
-- fiboLinear is efficient: linear run time!
fiboLinear n = 
  let (n1, n2) = fiboPair (n `div` 2) 
  in if n `mod` 2 == 0 then n1 else n2

-- Ackermann function
ackermann :: Integer -> Integer -> Integer
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m-1) 1
ackermann m n = ackermann (m-1) $ ackermann m (n-1)


-- Mutual recursion : Haskell has native support of mutual recursion
isEven n = if n == 0 then True else isOdd (abs n - 1)
isOdd n = if n == 0 then False else isEven (abs n - 1)
        
--------------------------------------------------------------------------------

-- Forward recursion

factFW :: Int -> Int
factFW n = fwrec 1
  where fwrec m = if m >= n then m else m * fwrec (m + 1) 

--------------------------------------------------------------------------------

-- Tail recursion: recursive call only appears in the end --- the last step of computation

-- Factorization
factTRec :: Int -> Int -> Int
factTRec st 0 = st
factTRec st n = let st' = st * n 
                in factTRec st' (n-1)
-- In general, st stores information that you need to put into the stack bafore a recursive call.
-- In the example of factorization, that's "n * ", so in tail recursion, we put it into st (or more precisely st'). 

factTR = factTRec 1

-- Fibonacci in tail recursion
fiboTRec :: Int -> Int -> Int -> Int
fiboTRec st1 st2 0 = st2
fiboTRec st1 st2 1 = st1
fiboTRec st1 st2 n = let st2' = st1 
                         st1' = st1 + st2 
                     in fiboTRec st1' st2' (n-1) 
-- In the fibonacci example, we need to integers to store the stack information, namely F(n-1) and F(n-2)
                        
fiboTR :: Int -> Int
fiboTR n = fiboTRec 1 1 n

-- Fibonacci (linear version) in tail recursion                        
fiboPairTRec :: (Int, Int) -> Int -> (Int, Int)
fiboPairTRec p 0 = p
fiboPairTRec (m1, m2) n = let m1' = m1 + m2
                              m2' = m2 + m1' 
                          in fiboPairTRec (m1', m2') (n-1)
                         
fiboLinearTR n = let (m1, m2) = fiboPairTRec (1, 1) (n `div` 2) 
                 in if n `mod` 2 == 0 then m1 else m2

-- Fibonacci in forward and tail recursion
fiboFwTr :: Int -> Int 
fiboFwTr 0 = 1
fiboFwTr 1 = 1
fiboFwTr n = fiboFT 2 (1, 1) n
             where fiboFT m (s1, s2) bound = if m == bound then s1+s2 else fiboFT (m+1) (s1+s2, s1) bound