module LeftistHeap (
  LeftistHeap,
  fromList
) where

import Heap

data LeftistHeap a = E
                   | T Int a (LeftistHeap a) (LeftistHeap a)
                   deriving (Show)
                     
{- The rank of any node x is the length of the shortest path
from x to a node without two children. The rank of a node 
with zero or one child is 1, while the rank of E is 0. -}
rank :: LeftistHeap a -> Int
rank E = 0
rank (T r _ _ _) = r

makeT :: a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a
makeT v x y
    | rx >= ry  = T (ry+1) v x y
    | otherwise = T (rx+1) v y x
  where rx = rank x
        ry = rank y


instance Heap LeftistHeap where
  empty = E
  
  isEmpty E = True
  isEmpty _ = False
  
  insert v x = merge (makeT v E E) x
  
  merge x E = x
  merge E y = y
  merge x@(T _ vx xL xR) y@(T _ vy yL yR)
    | vx <= vy  = makeT vx xL (merge xR y)
    | otherwise = makeT vy yL (merge yR x)

  findMin E = Nothing
  findMin (T _ v _ _) = Just v

  deleteMin E = E
  deleteMin (T _ _ x y) = merge x y


-- exercise 3.2
insert' :: Ord a => a -> LeftistHeap a -> LeftistHeap a
insert' v E = makeT v E E
insert' v h@(T _ u x y)
  | v <= u    = makeT v E h
  | otherwise = makeT u x (insert' v y)
                

-- exercise 3.3
{- For a list of length n, the number of merges is
   n/2 + n/4 + n/8 + ... + n/n
We need to put merge into account. This yields
   n/2 + n(log2)/4 + n(log4)/8 + ... + n(log(n)-1)/n
==>
   n/2 + nlog2 * sum(x/2^(x+1)) | x <- [1..logn]
The sum converges to 1, so it runs in O(n) time.

In contrast, fold gives O(nlog(n)) runtime. 
   log1 + log2 + log3 + log4 + ... + logn
==>
   log(n!), which is O(nlogn) -}

fromList' :: Ord a => [a] -> LeftistHeap a
fromList' [] = E
fromList' xs = go $ map (\v -> makeT v E E) xs
  where go [h] = h
        go hs = go $ map mergePair pairs
          where pairs = groupEvery 2 hs
                mergePair [h] = h
                mergePair [x,y] = merge x y

groupEvery :: Int -> [a] -> [[a]]
groupEvery n xs = go xs
  where go [] = []
        go xs = let (prefix,suffix) = splitAt n xs
                in prefix:go suffix

-- exercise 3.4a
{- The proof is almost the same as exercise 3.1, I won't
repeat here. -}

-- exercise 3.4b
data WeightBiasedLeftistHeap a = WE
                               | WT Int a 
                                 (WeightBiasedLeftistHeap a) 
                                 (WeightBiasedLeftistHeap a)
                               deriving (Show)
                                        
weight :: WeightBiasedLeftistHeap a -> Int                                        
weight WE = 0
weight (WT w _ _ _) = w

makeWT :: Ord a => a ->
          WeightBiasedLeftistHeap a -> 
          WeightBiasedLeftistHeap a -> 
          WeightBiasedLeftistHeap a
makeWT v x y
    | wx >= wy  = WT nw v x y
    | otherwise = WT nw v y x
  where nw = wx+wy+1
        wx = weight x
        wy = weight y
        
instance Heap WeightBiasedLeftistHeap where
  empty = WE
  
  isEmpty WE = True
  isEmpty _ = False
  
  merge x WE = x
  merge WE y = y
  merge x@(WT _ vx xL xR) y@(WT _ vy yL yR)
    | vx <= vy  = makeWT vx xL (merge xR y)
    | otherwise = makeWT vy yL (merge yR x)

  insert v x = merge (makeWT v WE WE) x

  findMin WE = Nothing
  findMin (WT _ v _ _) = Just v
  
  deleteMin WE = WE
  deleteMin (WT _ _ x y) = merge x y


-- exercise 3.4c
-- FIXME: What does `top-down` mean by the author?

{- Use an accumulator to transform merge to tail recursive. 

If you have different options of what `top-down` means,
this probably isn't the right anser. -}
data PartialHeap a = PH a (WeightBiasedLeftistHeap a)
                   deriving (Show)

{- I think this exercise is not meant for a lazy language. -}
mergeT :: (Ord a) => 
          WeightBiasedLeftistHeap a ->
          WeightBiasedLeftistHeap a ->
          WeightBiasedLeftistHeap a
mergeT x y = go [] x y          
  where go acc x WE = build acc x
        go acc WE y = build acc y
        go acc x@(WT _ vx xL xR) y@(WT _ vy yL yR)
            | vx <= vy  = go (push vx xL) xR y
            | otherwise = go (push vy yL) yR x
          where push v x = PH v x:acc
        
        -- What about foldr?
        build phs x = foldl step x phs
          where step y (PH v x) = makeWT v x y
                

-- exercise 3.4d
{-
I don't see any advantages of mergeT in a lazy environment, I suspect
my mergeT function is wrong. On the other hand, the original merge
function seems to exploit lazyness quite well. I mean if you visit
a leftist heap top-down, the subtrees are generated on demand.

For a concurrent environment, I'm sorry... I see none. But again,
the original merge function can generate subtrees in parallel.
-}