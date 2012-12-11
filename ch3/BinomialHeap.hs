module BinomialHeap (BinomialHeap) where

import Heap
import qualified Data.List as L

data BinomialTree a = T { 
  rank     :: Int, 
  root     :: a, 
  children :: [BinomialTree a]
} deriving (Show)

newtype BinomialHeap a = BH [BinomialTree a] deriving (Show)

-- merge two equal-sized trees
link :: Ord a =>
        BinomialTree a ->
        BinomialTree a ->
        BinomialTree a
link x@(T r vx cx) y@(T _ vy cy)
  | vx <= vy  = T (r+1) vx (y:cx)
  | otherwise = T (r+1) vy (x:cy)
                
insertTree :: (Ord a) => 
              BinomialTree a -> 
              [BinomialTree a] -> 
              [BinomialTree a]
insertTree x [] = [x]
insertTree x ts@(y:ys)
    | rx < ry = x:ts
    | otherwise = insertTree (link x y) ys
  where rx = rank x
        ry = rank y
        
mergeTree :: (Ord a) =>
             [BinomialTree a] ->
             [BinomialTree a] ->
             [BinomialTree a]
mergeTree xs [] = xs
mergeTree [] ys = ys
mergeTree xs@(x:xs') ys@(y:ys')
    | rx < ry = x:mergeTree xs' ys
    | rx > ry = y:mergeTree ys' xs
    | otherwise = insertTree (link x y) (mergeTree xs' ys')
  where rx = rank x
        ry = rank y
        
removeMinTree :: (Ord a) =>
                 [BinomialTree a] ->
                 Maybe (BinomialTree a, [BinomialTree a])
removeMinTree [] = Nothing
removeMinTree [t] = Just (t, [])
removeMinTree (t:ts) = do
  (t', ts') <- removeMinTree ts
  if root t < root t'
    then return (t,ts)
    else return (t', t:ts')
    
  
instance Heap BinomialHeap where
  empty = BH []
  
  isEmpty (BH xs) = null xs
  
  insert x (BH xs) = BH $ insertTree (T 0 x []) xs
  
  merge (BH xs) (BH ys) = BH $ mergeTree xs ys
  
  findMin (BH xs) = removeMinTree xs >>= \(t,_) ->
    return $ root t
  
  deleteMin (BH xs) = case removeMinTree xs of
    Nothing -> BH []
    Just ((T _ _ cs), ts) -> BH $ mergeTree (reverse cs) ts
    

-- exercise 3.5    
findMin' :: Ord a => BinomialHeap a -> Maybe a
findMin' (BH xs) = go xs
  where go [] = Nothing
        go xs = Just . root $ L.minimumBy (\x y -> root x `compare` root y) xs
        

-- exercise 3.6 is in BinomialHeap2.hs