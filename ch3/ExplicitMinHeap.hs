-- exercise 3.7

module ExplicitMinHeap (ExplicitMinHeap) where

import Heap
import BinomialHeap2

data ExplicitMinHeap h a = E | H a (h a) deriving (Show)
                           
instance Heap h =>  Heap (ExplicitMinHeap h) where
  empty = E
  
  isEmpty E = True
  isEmpty _ = False
  
  merge E x = x
  merge x E = x
  merge (H a x) (H b y)
    | a < b     = H a (merge x y)
    | otherwise = H b (merge x y)
  
  insert v E = H v (insert v empty)
  insert v (H a x) = H (min v a) (insert v x)

  findMin E = Nothing
  findMin (H a _) = Just a

  deleteMin E = E
  deleteMin (H _ x) = H m y
    where y = deleteMin x
          m = case findMin y of
            Nothing -> error "min value of heap not found"
            Just v  -> v