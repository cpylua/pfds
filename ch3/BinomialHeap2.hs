module BinomialHeap2 (BinomialHeap) where

import Heap
import qualified Data.List as L

data BinomialTree a = T a [BinomialTree a] deriving (Show)
data BinomialHeap a = BH [(Int, BinomialTree a)] deriving (Show)


{- Subtrees are in decreasing order of their rank -}
link :: Ord a =>
        (Int,BinomialTree a) ->
        (Int,BinomialTree a) ->
        (Int,BinomialTree a)
link (r, x@(T vx cx)) (_, y@(T vy cy))
    | vx <= vy  = (nr, T vx (y:cx))
    | otherwise = (nr, T vy (x:cy))
  where nr = r+1                

rank = fst
childrenRanks = [0..]

root (_, (T v _)) = v
children (_, (T _ c)) = c

findMinTree :: Ord a => [(Int, BinomialTree a)] -> (Int, BinomialTree a)
findMinTree = L.minimumBy (\x y -> root x `compare` root y)

instance Heap BinomialHeap where
  empty = BH []
  
  isEmpty (BH xs) = null xs
  
  merge (BH xs) (BH ys) = BH $ go xs ys
    where go [] ys = ys
          go xs [] = xs
          go xs@(x:xts) ys@(y:yts) =
            case rank x `compare` rank y of
              LT -> x:go xts ys
              GT -> y:go yts xs
              EQ -> go xts $ go yts [link x y]
  
  insert v h = merge h $ BH [(0, T v [])]
  
  findMin (BH xs) = go xs
    where go [] = Nothing
          go xs = Just . root . findMinTree $ xs
          
  deleteMin (BH xs) = go xs
    where go [] = BH []
          {- beware the different order of trees in a heap and
             subtrees in a tree -}
          go xs = BH subtrees `merge` BH trees
            where minTree = findMinTree xs
                  subtrees = zip childrenRanks (reverse $ children minTree)
                  trees = L.deleteBy rootEq minTree xs
                  rootEq x y = root x == root y