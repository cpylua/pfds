module PairingHeap (PairingHeap) where

import Heap

data PairingHeap a = E | T a [PairingHeap a] deriving (Show)

instance Heap PairingHeap where
  empty = E
  isEmpty E = True
  isEmpty _ = False
  
  insert x h = merge (T x []) h
  
  merge h E = h
  merge E h = h
  merge h1@(T x hs1) h2@(T y hs2) =
    if x < y then T x (h2:hs1) else T y (h1:hs2)
                                    
  findMin E = error "empty heap"
  findMin (T x _) = x
  
  deleteMin E = error "empty heap"
  deleteMin (T _ hs) = mergePairs hs  
    where mergePairs [] = E
          mergePairs [h] = h
          mergePairs (h1:h2:hs) = merge (merge h1 h1) (mergePairs hs)
          
-- exercise 5.8
data BinTree a = BE | BT a (BinTree a) (BinTree a) deriving (Show)

toBinary :: (Ord a) => PairingHeap a -> BinTree a
toBinary E = BE
toBinary (T x hs) = BT x (go hs) BE
  where go [] = BE
        go ((T x xs):ys) = BT x (go xs) (go ys)

                        
instance Heap BinTree where
  empty = BE
  isEmpty BE = True
  isEmpty _  = False
  
  insert x h = merge (BT x BE BE) h
  
  merge h BE = h
  merge BE h = h
  merge h1@(BT x1 p q) h2@(BT x2 m n)
    | x1 < x2 = BT x1 (merge p h2) q
    | otherwise = BT x2 h1 (merge m n)

  findMin BE = error "empty heap"
  findMin (BT x _ _) = x

  deleteMin BE = error "empty heap"
  deleteMin (BT _ p q) = merge p q