module BankersQueue (BankersQueue) where

import Prelude hiding (head,tail)
import Queue

data BankersQueue a = Q Int [a] Int [a] deriving (Show)

instance Queue BankersQueue where
  empty = Q 0 [] 0 []
  isEmpty (Q lenf _ _ _) = lenf == 0
  
  snoc (Q lenf f lenr r) x = check lenf f (lenf+1) (x:r)
  
  head (Q _ [] _ _) = error "empty queue"
  head (Q _ (x:_) _ _) = x
  
  tail (Q _ [] _ _) = error "empty queue"
  tail (Q lenf (x:f) lenr r) = check (lenf-1) f lenr r
  
check :: Int -> [a] -> Int -> [a] -> BankersQueue a  
check lenf f lenr r
  | lenr <= lenf = Q lenf f lenr r
  | otherwise    = Q (lenf+lenr) (f ++ reverse r) 0 []
  