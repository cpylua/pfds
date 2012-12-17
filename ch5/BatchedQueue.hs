module BatchedQueue (BatchedQueue) where

import Prelude hiding (head,tail)
import Queue

data BatchedQueue a = Q [a] [a] deriving (Show)

instance Queue BatchedQueue where
  empty = Q [] []
  isEmpty (Q f _) = null f
  
  snoc (Q f r) x = check f (x:r)
  
  head (Q [] _) = error "empty queue"
  head (Q (x:f) r) = x
  
  tail (Q [] _) = error "empty queue"
  tail (Q (x:f) r) = check f r
  
check :: [a] -> [a] -> BatchedQueue a
check [] r = Q (reverse r) []
check f r = Q f r
  