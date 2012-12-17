{- exercise 5.1 -}

module BatchedDeque (BatchedDeque) where

import Prelude hiding (head,tail,init,last)
import Control.Arrow (second)
import Deque

data BatchedDeque a = Q [a] [a] deriving (Show)

instance Deque BatchedDeque where
  empty = Q [] []
  isEmpty (Q f r) = all null [f,r]
  
  head (Q [] []) = error "empty queue"
  head (Q [x] []) = x
  head (Q [] [x]) = x
  head (Q (x:_) _) = x
  
  last (Q [] []) = error "empty queue"
  last (Q [x] []) = x
  last (Q [] [x]) = x
  last (Q _ (x:_)) = x
  
  cons x (Q f r) = check (x:f) r
  
  snoc (Q f r) x = check f (x:r)
  
  tail (Q [] []) = error "empty queue"
  tail (Q [x] []) = Q [] []
  tail (Q [] [x]) = Q [] []
  tail (Q (x:f) r) = check f r
  
  init (Q [] []) = error "empty queue"
  init (Q [x] []) = Q [] []
  init (Q [] [x]) = Q [] []
  init (Q f (x:r)) = check f r

check :: [a] -> [a] -> BatchedDeque a
check [] r = split r (flip Q)
check f [] = split f Q
check f r = Q f r

split :: [a] -> ([a] -> [a] -> BatchedDeque a) -> BatchedDeque a
split xs ctor = uncurry ctor . second reverse $ splitAt m xs
  where m = length xs `div` 2

