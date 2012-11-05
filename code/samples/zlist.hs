-- Prelude> :t (:)
-- Prelude> :t [] 
-- Prelude> :t (++)

-- Prelude> 1 : [2, 3]
-- Prelude> [1] ++ [2, 3]

-- Prelude> head [1, 2, 3]
-- Prelude> tail [1, 2, 3]
-- Prelude> last [1, 2, 3]
-- Prelude> init [1, 2, 3]
-- Prelude> null [1, 2, 3]
-- Prelude> null []

-- Prelude> [] == [[]]
-- Prelude> [[]] == [[], []]

--------------------------------------------------------------------------------
-- Strings as lists

-- Prelude> :t "hello"
-- Prelude> "hello " ++ "world!"
-- Prelude> ['I', 'S'] ++ ['C', 'A', 'S']
-- Prelude> 'H' : "ello"

-- Prelude> "hello" !! 0
-- Prelude> "hello" !! 3
-- Prelude> "hello" !! 6

len :: [a] -> Int
len [] = 0
len (x : xs) = len xs + 1
-- Prelude> len "hello"
-- Prelude> length "hello"

-- list of lists
-- Prelude>  [[1, 2], [3, 4]]
-- Prelude>  ["Hello ", "world", "!"]

flatten :: [[a]] -> [a] 
flatten [] = []
flatten (l : ls) = l ++ flatten ls 
-- Prelude> flatten  [[1, 2], [3, 4]]
-- Prelude> flatten  ["Hello ", "world", "!"]

--------------------------------------------------------------------------------
-- Ranges and infinite list

-- Prelude> [1 .. 10]
-- Prelude> [1, 3 .. 10]
-- Prelude> ['a' .. 'z']
-- Prelude> ['A' .. 'z']
-- Prelude> ['a', 'd' .. 'z']
-- Prelude> [10 .. 1]
-- Prelude> [10, 9 .. 1]

-- Prelude> [1, 3 .. 40]
-- Prelude> take 20 [1, 3 .. ]
-- Prelude> take 20 $ iterate (+2) 1

-- Prelude> let rep a = a : rep a
-- Prelude> take 20 (repeat 0)

--------------------------------------------------------------------------------
-- More list operations

-- Prelude> reverse [1 .. 10]
-- Prelude> take 3 [1 .. 10]
-- Prelude> take 3 [1 .. ]
-- Prelude> drop 3 [1 .. 10]
-- Prelude> maximum [1 .. 10]
-- Prelude> :t elem
-- Prelude> elem 1 [1 .. 10]
-- Prelude> 11 `elem` [1 .. 10]

-- Prelude> map (\x -> x * 2) [1 .. 10] 
-- Prelude> map (\x -> if x `mod` 2 == 0 then 'a' else 'b') [0 .. 20]
-- Prelude> filter (\x -> x `mod` 2 == 0) [0 .. 20]
-- Prelude> filter (\x -> not (x `elem` ['A' .. 'Z'])) "LaTeX"
-- Prelude> filter (\x -> x /= ' ') "Chinese Academy of Sciences"
-- Prelude> foldl (+) 0 [1 .. 10]
-- Prelude> zip [1, 2, 3] "abcd"
-- Prelude> zipWith (*) [1, 2, 3] [4, 5, 6]

--------------------------------------------------------------------------------
-- List comprehension

-- Prelude> [m * 2 + 1 | m <- [0 .. 9]]
-- Prelude> [x | x =  [0 .. 100], x `mod` 3 == 0]
-- Prelude> [(x, y) | x <- [-5 .. 5], y <- [-5 ..5], x^2 + y^2 <= 25]

-- Prelude> let len l = sum [ 1 | _ <- l]
-- Prelude> let listMinus l1 l2 = [ x | x <- l1, not (x `elem` l2) ]
-- Prelude> let removeCapitals s = [c | c <- s, not (c `elem` ['A' .. 'Z'])]
-- Prelude> let removeCapitals s = listMinus s ['A' .. 'Z']

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x : xs) = lts ++ eqs ++ gts
  where lts = quickSort ( filter (\y -> y < x) xs) 
        eqs = x : filter (== x) xs -- Don't forget the leading x 
        gts = quickSort ( filter (> x) xs)
        
generalSort :: (a -> a -> Ordering) -> [a] -> [a]
generalSort comp []  = []
generalSort comp (x : xs) = lts ++ eqs ++ gts
  where lts = generalSort comp (filter (\y -> comp y x  == LT) xs) 
        eqs = x : filter (\y -> comp y x == EQ) xs -- Don't forget the leading x 
        gts = generalSort comp (filter (\y -> comp y x == GT) xs)

fact :: Int -> Int
fact n = foldl (*) 1 [1 .. n]

fibo :: Int -> Int
fibo n = l !! n
  where l = 1 : 1 : (zipWith (+) l (tail l))
-- The following list comprehension version is WRONG !!!
-- fibo n = l !! n
--   where l = 1 : 1 : [a + b | a <- l, b <- tail l]
        
zipPair :: [a] -> [b] -> [(a, b)]
zipPair [] l = [] 
zipPair l [] = []
zipPair (x : xs) l = map (\y -> (x, y)) l ++ zipPair xs l
-- zipPair l1 l2 is equivalent to [(a, b) | a <- l1, b <- l2]

-- Triangle numbers 
--  1    3      6       10
--                       *
--              *       * *
--       *     * *     * * *
--  *   * *   * * *   * * * *

--------------------------------------------------------------------------------
