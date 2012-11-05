data Color = Red | Green | Blue
-- Red, Green, Blue are constructors 
-- Types defined this way are indeed sum types:
-- we can regard Color as "unit + unit + unit", 
-- with the first unit for Red, the second for Green and the third for Blue

toRGB :: Color -> Int
toRGB c = case c of Red -> 255 * 2^16
                    Green -> 255 * 2^8
                    Blue -> 255 

data Shape = Rectangle Float Float
           | Round Float
           | Square Float
--           deriving Show                    
-- Check types of constructors

area :: Shape -> Float
area (Rectangle x y)  = x * y
area (Square x)  = x^2
area (Round x) = pi * x^2

areaList :: [Shape] -> Float
-- areaList l = sum $ map area l
areaList = sum . map area 

--------------------------------------------------------------------------------

-- data IntTree = Leaf | Node (Int, IntTree, IntTree)
data IntTree = IntLeaf | IntNode Int IntTree IntTree
--             deriving (Show, Eq)
                      
leftIntTree :: IntTree -> Maybe IntTree
-- leftIntTree t = case t of Leaf -> Nothing 
--                           Node _ t1 _ -> Just t1
leftIntTree IntLeaf = Nothing                          
leftIntTree (IntNode _ t1 _) = Just t1

makeIntTreeFromList :: [Int] -> IntTree
makeIntTreeFromList [] = IntLeaf
makeIntTreeFromList (x : l) = 
  let (l1, l2) = splitAt (length l `div` 2) l in IntNode x (makeIntTreeFromList l1) (makeIntTreeFromList l2)
                                                 
-- A more general version of binary tree, defined as a polymorphic type
data BinTree a = Leaf | Node a (BinTree a) (BinTree a)

btHeight :: BinTree a -> Int
btHeight Leaf = 0
btHeight (Node _ t1 t2) = max (btHeight t1) (btHeight t2)

leftFold :: (a -> b -> b) -> b -> BinTree a -> b
leftFold f init Leaf = init
leftFold f init (Node x t1 t2) = 
  let res = leftFold f init t1 
  in leftFold f (f x res) t2
     
leftTraverse t = reverse (leftFold (:) [] t) 
-- leftTraverse = reverse . (leftFold (:) [])


-- type definition for PCF types
data PcfType = Tnat
             | Tbool
             | Tunit
             | Tfunc PcfType PcfType
             | Tprod PcfType PcfType
             | Tsum PcfType PcfType
               
typeOrder :: PcfType -> Int
typeOrder Tnat = 0
typeOrder Tbool = 0
typeOrder Tunit = 0
typeOrder (Tfunc t1 t2) = max (typeOrder t1 + 1) (typeOrder t2)
typeOrder (Tprod t1 t2) = max (typeOrder t1) (typeOrder t2)
typeOrder (Tsum t1 t2) = max (typeOrder t1) (typeOrder t2)

--------------------------------------------------------------------------------
-- Haskell type classes

instance Show (Shape) where 
  show (Rectangle x y) = "Rectangel (" ++ show x ++ ", " ++ show y ++ ")"
  show (Round x) = "Round (" ++ show x ++ ")"
  show (Square x) = "Square (" ++ show x ++ ")"

shapeEq :: Shape -> Shape -> Bool
shapeEq (Rectangle x1 y1) (Rectangle x2 y2) = x1 == x2 && y1 == y2
shapeEq (Round x1) (Round x2) = x1 == x2
shapeEq (Square x1) (Square x2) = x1 == x2
shapeEq s1 s2 = False

instance Eq (Shape) where 
  Rectangle x1 y1 == Rectangle x2 y2 = x1 == x2 && y1 == y2
  Round x1 == Round x2 = x1 == x2
  Square x1 == Square x2 = x1 == x2
  _ == _ = False

instance Eq (IntTree) where 
  IntLeaf == IntLeaf = True
  IntNode x1 l1 r1 == IntNode x2 l2 r2 = x1 == x2 && l1 == l2 && r1 == r2
  _ == _ = False
  
instance Eq a => Eq (BinTree a) where 
  -- Notice that "a" must be an instance of Eq, otherwise the definition is not valid
  Leaf == Leaf = True
  Node x1 l1 r1 == Node x2 l2 r2 = x1 == x2 && l1 == l2 && r1 == r2
  _ == _ = False
  
-- If we added "deriving Eq" in the end of the definition of BinTree type, 
-- then we don't have to write the above definition --- GHC automatically defines (==).

--------------------------------------------------------------------------------
  
data BST a = Nil | Snode a (BST a) (BST a) 

member :: Ord a => a -> BST a -> Bool
member key Nil = False
member key (Snode x t1 t2) | key == x = True 
                           | key < x = member key t1
                           | key > x = member key t2
                                       
insert :: Ord a => a -> BST a -> BST a
insert key Nil = Snode key Nil Nil
insert key t@(Snode x t1 t2) | key == x = t
                             | key < x = Snode x (insert key t1) t2 
                             | key > x = Snode x t1 (insert key t2)

--------------------------------------------------------------------------------
-- More general version of quick sort
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = lts ++ eqs ++ gts 
  where lts = qsort $ filter (< x) xs 
        eqs = x : filter (== x) xs 
        gts = qsort $ filter (> x) xs
        
-- Quick sort with user-defined comparison function
-- This is again more general than the above implementation "qsort" 
qsortBy :: (a -> a -> Ordering) -> [a] -> [a] 
qsortBy _ [] = []
qsortBy cmp (e : es) = lts ++ eqs ++ gts 
  where lts = qsortBy cmp $ filter (\x -> cmp x e == LT) es   
        eqs = e : filter (\x -> cmp x e == EQ) es
        gts = qsortBy cmp $ filter (\x -> cmp x e == GT) es 
        
reverseOrder :: (a -> a -> Ordering) -> a -> a -> Ordering
reverseOrder cmp x y = case cmp x y of LT -> GT
                                       GT -> LT 
                                       EQ -> EQ
        
-- Insersion sort
inSort :: Ord a => [a] -> [a] 
inSort [] = []
inSort (x : xs) = insert x (inSort xs) 
  where insert y [] = [y] 
        insert y l@(z : zs) = if y <= z then y : l else z : insert y zs
        
-- Merge sort
mgSort :: Ord a => [a] -> [a] 
mgSort [] = []
mgSort [x] = [x] 
mgSort l = let (l1, l2) = splitAt (length l `div` 2) l
           in merge (mgSort l1) (mgSort l2) 
  where splitAt 0 lis = ([], lis) 
        splitAt _ [] = ([], []) 
        splitAt n (x : xs) = let (l1, l2) = splitAt (n-1) xs in (x : l1, l2) 
        merge [] lis = lis 
        merge lis [] = lis 
        merge l1@(x : xs) l2@(y : ys) = if x <= y then x : merge xs l2 else y : merge l1 ys

