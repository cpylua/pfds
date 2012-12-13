{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module RedBlackSet (
  RedBlackSet
) where

import Set

data Color = Red | Black deriving (Show)
data RedBlackSet a = E |
                     T Color (RedBlackSet a) a (RedBlackSet a)
                     deriving (Show)
                              
instance Ord a => Set RedBlackSet a where
  empty = E
  
  member x s = go s
    where go E = False
          go (T _ lt v gt) =
            case x `compare` v of
              LT -> go lt
              EQ -> True
              GT -> go gt
              
  -- exercise 3.10
  insert x s = blacken . snd $ (ins s)              
    where ins E = (LT,T Red E x E)
          ins s@(T clr lt v gt) =
            case x `compare` v of
              LT -> case ins lt of
                (LT,lt') -> (LT,llbalance clr lt' v gt)
                (EQ,lt') -> (EQ,s)
                (GT,lt') -> (LT,lrbalance clr lt' v gt)
              EQ -> (EQ,s)
              GT -> case ins gt of
                (LT,gt') -> (GT,rlbalance clr lt v gt')
                (EQ,gt') -> (EQ,s)
                (GT,gt') -> (GT,rrbalance clr lt v gt')
          blacken (T _ a x b) = T Black a x b
            
balance :: Color -> RedBlackSet a -> a -> RedBlackSet a -> RedBlackSet a
balance Black (T Red (T Red a x b) y c) z d =
  T Red (T Black a x b) y (T Black c z d)
balance Black (T Red a x (T Red b y c)) z d =
  T Red (T Black a x b) y (T Black c z d)
balance Black a x (T Red b y (T Red c z d)) =
  T Red (T Black a x b) y (T Black c z d)
balance Black a x (T Red (T Red b y c) z d) =
  T Red (T Black a x b) y (T Black c z d)
balance clr a x b = T clr a x b

llbalance :: Color -> RedBlackSet a -> a -> RedBlackSet a -> RedBlackSet a
llbalance Black (T Red (T Red a x b) y c) z d =
  T Red (T Black a x b) y (T Black c z d)
llbalance clr a x b = T clr a x b  
  
lrbalance :: Color -> RedBlackSet a -> a -> RedBlackSet a -> RedBlackSet a
lrbalance Black (T Red a x (T Red b y c)) z d =
  T Red (T Black a x b) y (T Black c z d)
lrbalance clr a x b = T clr a x b  

rrbalance :: Color -> RedBlackSet a -> a -> RedBlackSet a -> RedBlackSet a
rrbalance Black a x (T Red b y (T Red c z d)) =
  T Red (T Black a x b) y (T Black c z d)
rrbalance clr a x b = T clr a x b  

rlbalance :: Color -> RedBlackSet a -> a -> RedBlackSet a -> RedBlackSet a
rlbalance Black a x (T Red (T Red b y c) z d) =
  T Red (T Black a x b) y (T Black c z d)
rlbalance clr a x b = T clr a x b

-- exercise 3.9
{- Code stolen from `COnstructing Red-Black Trees` by Ralf Hinze. 
   The paper is definitely worth reading. -}
data Digit a = One a (RedBlackSet a) |
             Two a (RedBlackSet a) a (RedBlackSet a)             
             deriving (Show)
                      
incr :: Digit a -> [Digit a] -> [Digit a]
incr d [] = [d]
incr (One a1 t1) (One a2 t2 : ts) = Two a1 t1 a2 t2 : ts
incr (One a1 t1) (Two a2 t2 a3 t3 : ts) =
  One a1 t1 : incr (One a2 (T Black t2 a3 t3)) ts

add :: a -> [Digit a] -> [Digit a]
add x ts = incr (One x E) ts

link :: RedBlackSet a -> Digit a -> RedBlackSet a
link t (One a t1) = T Black t a t1
link t (Two a1 t1 a2 t2) = T Black (T Red t a1 t1) a2 t2

fromOrdList :: [a] -> RedBlackSet a
fromOrdList = linkAll . foldr add []
  where linkAll = foldl link E
        
