module MatchQueue 
       ( MatchQueue -- data types for match queue
       , mqAdd
       , mqHead
       , mqDelete
       , emptyQueue
       , isEmpty
       ) where 

data Queue a = Qu [a] [a]
             deriving (Eq)

instance Show a => Show (Queue a) where
  show (Qu fl bl) = show (fl ++ reverse bl) 

emptyQu = Qu [] []

isEmptyQu :: Queue a -> Bool
isEmptyQu (Qu [] []) = True
isEmptyQu _ = False

qAdd :: Queue a -> a -> Queue a 
qAdd (Qu [] []) x = Qu [x] []
qAdd (Qu fl bl) x = Qu fl (x : bl)
              
qHead :: Queue a -> Maybe a
qHead (Qu [] []) = Nothing 
qHead (Qu fl _) = Just (head fl) 

qDelete :: Queue a -> (Maybe a, Queue a)
qDelete (Qu [] []) = (Nothing, Qu [] [])
qDelete (Qu [x] bl) = (Just x, Qu (reverse bl) [])
qDelete (Qu fl bl) = (Just $ head fl, Qu (tail fl) bl)

data MatchQueue a = MQ { back :: Queue a, fronts :: [Queue a] }
                  deriving (Eq) 
                                  
instance Show a => Show (MatchQueue a) where 
  show mq = show (back mq) ++ " || " ++ show (fronts mq) 

qSearch :: (a -> Bool) -> Queue (a) -> (Maybe a, Queue a, Queue a)
qSearch pred qu = qSearchRec pred qu emptyQu
  where qSearchRec pred qu front = 
          case qDelete qu 
          of (Nothing, q) -> (Nothing, front, q)
             (Just x, q) -> if pred x then (Just x, front, q) else qSearchRec pred q (qAdd front x)

emptyQueue :: MatchQueue a
emptyQueue = MQ emptyQu []

isEmpty :: MatchQueue a -> Bool
isEmpty (MQ bqu []) = isEmptyQu bqu
isEmpty _ = False

mqAdd :: MatchQueue a -> a -> MatchQueue a 
mqAdd (MQ bqu fqus) x = MQ (qAdd bqu x) fqus
              
mqHead :: (a -> Bool) -> MatchQueue a -> Maybe a
mqHead pred (MQ bq fqus) = case searchFronts pred fqus 
                                of Nothing -> case qSearch pred bq of (Nothing, _, _) -> Nothing 
                                                                      (Just x, _, _) -> Just x
                                   Just x -> Just x
  where searchFronts pred [] = Nothing
        searchFronts pred (fq : fqs) = case qSearch pred fq 
                                       of (Nothing, _, _) -> searchFronts pred fqs 
                                          (Just x, _, _) -> Just x

mqDelete :: (a -> Bool) -> MatchQueue a -> MatchQueue a
mqDelete pred (MQ bqu fqus) = deleteRec pred fqus bqu emptyQu
  where deleteRec pred [] bq fs = case moveUntil pred (bq, fs) 
                                  of (Nothing, _, fs') -> MQ fs' []
                                     (Just _, bq', fs') -> if isEmptyQu fs' then MQ bq' [] else MQ bq' [fs']
        deleteRec pred (fq : fqs) bq fs = case moveUntil pred (fq, fs) 
                                          of (Nothing, fq', fs') -> deleteRec pred fqs bq fs'
                                             (Just _, Qu [] [], Qu [] []) -> MQ bq fqs 
                                             (Just _, Qu [] [], fs') -> MQ bq (fs' : fqs) 
                                             (Just _, fq', Qu [] []) -> MQ bq (fq' : fqs) 
                                             (Just _, fq', fs') -> MQ bq (fs' : fq' : fqs)
        moveUntil pred (fq, fs) = case qDelete fq 
                                  of (Nothing, fq') -> (Nothing, fq', fs) 
                                     (Just x, fq') -> if pred x then (Just x, fq', fs) 
                                                      else moveUntil pred (fq', qAdd fs x) 