module Queue 
       ( Queue
       , qAdd
       , qHead 
       , qDelete
       , emptyQueue
       , isEmpty
       -- , qMap
       -- , qFoldl
       -- , qFoldr
       -- , qTake
       -- , qDrop
       ) where 

-- import Data.List
-- import Random
import System.Random
import Data.Time.Clock

data Queue a = Qu [a] [a]
             deriving (Eq)

instance Show a => Show (Queue a) where
  show (Qu fl bl) = "{ " ++ show fl ++ " | " ++ show bl ++ " }"

emptyQueue = Qu [] []

isEmpty :: Queue a -> Bool
isEmpty (Qu [] []) = True
isEmpty _ = False

qAdd :: Queue a -> a -> Queue a 
qAdd (Qu [] []) x = Qu [x] []
qAdd (Qu fl bl) x = Qu fl (x : bl)
              
qHead :: Queue a -> Maybe a
qHead (Qu [] []) = Nothing 
qHead (Qu fl _) = Just (head fl) 

qDelete :: Queue a -> Queue a
qDelete (Qu [] []) = Qu [] []
qDelete (Qu [x] bl) = Qu (reverse bl) []
qDelete (Qu fl bl) = Qu (tail fl) bl

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
                        
     