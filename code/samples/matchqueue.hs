module MatchQueue 
       ( MatchQueue
       , mqPut
       , mqGet
       , mqMatchGet
       , emptyQueue
       , isEmpty
       , mqHead
       ) where 

import Data.List
import Queue

data MQ a = Bot | Elem a
data MatchQueue a = Queue a deriving Show

transfer :: Queue a -> Queue a 
transfer q = let l1 = reverse $ inList q
                 l2 = outList q
             in Queue { inList = [], outList = l2 ++ l1 }
                
emptyQueue = Queue { inList = [], outList = [] }

isEmpty :: Queue a -> Bool
isEmpty q  = case (inList q, outList q) of  ([], []) -> True 
                                            _ -> False

qPush :: Queue a -> a -> Queue a 
qPush q a = Queue { inList = a : inList q, outList = outList q }
              
qPop :: Queue a -> (Maybe a, Queue a) 
qPop q = if isEmpty q then (Nothing, q)  
         else let q' = transfer q 
              in case outList q' of [] -> (Nothing, q') 
                                    x : xs -> (Just x, Queue { inList = inList q', outList = xs })

qHead = fst . qPop