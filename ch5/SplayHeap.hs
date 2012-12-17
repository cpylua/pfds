module SplayHeap (SplayHeap) where

import Heap

data SplayHeap a = E | T (SplayHeap a) a (SplayHeap a)
                 deriving (Show)
                          
partition :: Ord a => a -> 
             SplayHeap a -> 
             (SplayHeap a, SplayHeap a)
partition pivot E = (E, E)             
partition pivot t@(T a x b)
  | x <= pivot = case b of
    E -> (t,E)
    T b1 y b2 ->
      if y <= pivot
      then let (small,big) = partition pivot b2
           in (T (T a x b1) y small, big)
      else let (small,big) = partition pivot b1
           in (T a x small, T big y b2)
  | otherwise = case a of
    E -> (E,t)
    T a1 y a2 ->
      if y <= pivot
      then let (small,big) = partition pivot a2
           in (T a1 y small, T big x b)
      else let (small,big) = partition pivot a1
           in (small, T big y (T a2 x b))
              
instance Heap SplayHeap where
  empty = E
  isEmpty E = True
  isEmpty _ = False
  
  insert x h = T a x b
    where (a,b) = partition x h
          
  merge E h = h
  merge h E = h
  merge (T a x b) h = T (merge a ha) x (merge b hb)
    where (ha,hb) = partition x h
  
  findMin E = error "empty heap"
  findMin (T E x _) = x
  findMin (T a _ _) = findMin a
  
  deleteMin E = error "empty heap"
  deleteMin (T E x b) = b
  deleteMin (T (T E _ a) y b) = T a y b
  deleteMin (T (T a x b) y c) = T (deleteMin a) x (T b y c)
  

-- exercise 5.7
hsort :: Ord a => [a] -> [a]
hsort = inorder . fromList
  where inorder :: Ord a => SplayHeap a -> [a]
        inorder E = []
        inorder (T a x b) = inorder a ++ [x] ++ inorder b