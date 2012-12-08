{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}

module UnbalancedSet (
  UnbalancedSet
) where

import Set
import Control.Monad.Error

data UnbalancedSet a = E 
                     | T (UnbalancedSet a) a (UnbalancedSet a)
                     deriving (Show)
                              
instance (Ord a) => Set UnbalancedSet a where
  empty = E
  
  _ `member` E  = False
  e `member` (T left x right)
    | e < x     = e `member` left
    | e > x     = e `member` right
    | otherwise = True

  insert e E    = T E e E
  insert e t@(T left x right)
    | e < x     = T (insert e left) x right
    | e > x     = T left x (insert e right)
    | otherwise = t

-- exercise 2.2
member' _ E = False
member' e t@(T _ x _) = go t x
  where go (T lt v gt) c | e < v     = go lt c
                         | otherwise = go gt v
        go E c                       = c == e
        
-- exercise 2.3
insert' e t = either (const t) id (go t)
  where go (T lt x gt)
          | e < x = do 
            lt' <- go lt
            return (T lt' x gt)
          | e > x = do
            gt' <- go gt
            return (T lt x gt')
          | otherwise = throwError "already exists"
        go E = return (T E e E)
        
-- exercise 2.4
insertBetter e E = T E e E
insertBetter e t@(T _ x _) = either (const t) id (go t x)
  where go (T lt v gt) c
          | e < v = do
            lt' <- go lt c
            return (T lt' v gt)
          | otherwise = do
            gte <- go gt v
            return (T lt v gte)
        go E c | c == e = throwError "already exists"
               | otherwise = return (T E e E)
                             
-- exercise 2.5a
complete x n
  | n == 0 = T E x E
  | n > 0 = let sub = complete x (n-1) in T sub x sub
  | otherwise = E
                
-- exercise 2.5b
balanced :: (Integral b) => a -> b -> UnbalancedSet a
balanced x n
    | n <= 0 = E
    | full = complete x (log-1)
    | otherwise = let (left,right) = create2 x u v
                      u = remain `div` 2
                      v = remain - u
                      remain = n - 1
                  in T left x right
  where (full,log) = isComplete n

create2 :: (Integral b) => a -> b -> b -> (UnbalancedSet a, UnbalancedSet a)
create2 x m n
  | m == n = let t = balanced x m in (t, t)
  | otherwise = (large,small)
    where large = balanced x (max m n)
          small = balanced x (min m n)
                           
isComplete :: (Integral a) => a -> (Bool, Integer)
isComplete n = let log = truncate $ logBase 2 (fromIntegral (n+1))
               in (2^log == n+1,log)

-- exercise 2.6
data MapNode k v = Node {
  nodeKey :: k,
  nodeValue :: v
} deriving (Show)

instance (Eq k) => Eq (MapNode k v) where
  (Node k1 _) == (Node k2 _) = k1 == k2
  
instance (Ord k) => Ord (MapNode k v) where
  compare (Node k1 _) (Node k2 _) = k1 `compare` k2
  
class Map m k v | k v -> m where
  fromList :: [(k,v)] -> m k v
  lookupMap :: m k v -> k -> Maybe v
  
data UnbalancedTreeMap k v = M {
  tree :: UnbalancedSet (MapNode k v)
} deriving (Show)

instance (Eq k, Ord k) => Map UnbalancedTreeMap k v where
  fromList = foldl put (M E)
    where put m (k,v) = M $ insert (Node k v) (tree m)
          
  lookupMap (M E) k = Nothing
  lookupMap (M tree) k = maybe Nothing (Just . nodeValue) $ lookupSet tree (Node k undefined)
    
lookupSet :: (Eq a, Ord a) => UnbalancedSet a -> a -> Maybe a
lookupSet E x = Nothing
lookupSet (T lt v gt) x
  | v == x = Just v
  | x < v  = lookupSet lt x
  | x > v  = lookupSet gt x