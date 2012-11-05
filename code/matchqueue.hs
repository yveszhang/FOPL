-- In this module you will implement the match queue data structure.

-- A match queue is essentially a First-In-First-Match-Out, linear data set. 
-- It is like a FIFO queue, a new element will be always added to the end of the queue.
-- But retrieving as well as deleting is not always the head.
-- Retrieving and deleting operations are accompanied with a predicate, 
-- and the result will be the first element, starting from the front of the queue, 
-- that satisfies the predicate. 

-- For instace, given a match queue [1, 2, 3, 4, 1, 2, 5] 
-- and a predicate (written as a Haskell function): \x -> x > 3 
-- A retrieving will get the element 4, and a deleting will make a new queue [1, 2, 3, 1, 2, 5].
-- If we do delete again, we will get [1, 2, 3, 1, 2].

-- Below is the module structure. You need to implement the data type and all the operation functions
-- whose type are already specified. You can make the implementation based on the queue that we have 
-- defined in the class (in the queue.hs file of samplecode package), or you can do from scratch.

-- Notice that you are not allowed to change the types!

-- Finish the code. 
-- You'd better make a brief descrption of your implementation and a rough analysis of complexity.

module MatchQueue 
       ( MatchQueue -- data types for match queue
       , mqAdd
       , mqMatchHead
       , mqMatchDelete
       , emptyQueue
       , isEmpty
       ) where 

emptyQueue :: MatchQueue

isEmpty :: MatchQueue a -> Bool

mqAdd :: MatchQueue a -> a -> MatchQueue a 
              
mqMatchHead :: (a -> bool) -> MatchQueue a -> Maybe a

mqMatchDelete :: (a -> bool) -> MatchQueue a -> MatchQueue a