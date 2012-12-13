{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Set (Set(..)) where

class Set s a | a -> s where
  empty :: s a
  insert :: a -> s a -> s a
  member :: a -> s a -> Bool
  
  fromList :: [a] -> s a
  fromList = foldr insert empty