module Heap (Heap(..)) where

class Heap h where
  empty :: Ord a => h a
  isEmpty :: Ord a => h a -> Bool
  insert :: Ord a => a -> h a -> h a
  merge :: Ord a => h a -> h a -> h a
  findMin :: Ord a => h a -> Maybe a
  deleteMin :: Ord a => h a -> h a
  
  fromList :: Ord a => [a] -> h a
  fromList = foldr insert empty