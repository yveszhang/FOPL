module LazyQueue 
       ( LazyQueue
       , qAdd
       , qHead 
       , qDelete
       , emptyQueue
       , isEmpty
       ) where 

-- import Data.List
import Random
import System.Random
import Data.Time.Clock

data LazyQueue a = LQ Int [a] Int [a]
             deriving (Eq)

instance Show a => Show (LazyQueue a) where
  show (LQ _ fl _ bl) = "{ " ++ show fl ++ " | " ++ show bl ++ " }"
  
emptyQueue = LQ 0 [] 0 []

isEmpty :: LazyQueue a -> Bool
isEmpty (LQ 0 [] 0 []) = True
isEmpty _ = False

rotate fs fl bs bl =
  if bs > fs then LQ (fs+bs) (fl ++ reverse bl) 0 []
  else LQ fs fl bs bl

qAdd :: LazyQueue a -> a -> LazyQueue a 
-- qAdd (LQ _ [] _ []) x = LQ 1 [x] 0 []
qAdd (LQ fs fl bs bl) x = rotate fs fl (bs+1) (x:bl)
              
qHead :: LazyQueue a -> Maybe a
qHead (LQ _ [] _ []) = Nothing 
qHead (LQ _ fl _ _) = Just (head fl) 

qDelete :: LazyQueue a -> LazyQueue a
qDelete (LQ 0 [] 0 []) = LQ 0 [] 0 []
qDelete (LQ fs fl bs bl) = rotate (fs-1) (tail fl) bs bl

randomTest n = 
  do t <- getCurrentTime 
     let seed = floor . toRational . utctDayTime $ t
         g1 = mkStdGen seed
         rsValue = randomRs (1, 100 :: Int) g1
         g2 = mkStdGen $ head rsValue
         rsChoice = randomRs (1, 3 :: Int) g2
     testRec emptyQueue rsValue rsChoice 0 n 
    where testRec qu vs cs m bound = 
            if m == bound then return ()
            else case head cs 
                 of 1 -> let qu' = qAdd qu $ head vs
                         in do print ("Add " ++ show (head vs) ++ ": " ++ show qu')
                               testRec qu' (tail vs) (tail cs) (m+1) bound
                    2 -> let x = qHead qu 
                         in do print ("Head: " ++ show x ++ ", " ++ show qu)
                               testRec qu vs (tail cs) (m+1) bound
                    _ -> let qu' = qDelete qu 
                         in do print ("Delete: " ++ show qu')
                               testRec qu' vs (tail cs) (m+1) bound
                        
     