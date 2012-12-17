module PhysicistsQueue (PhysicistsQueue) where

import Prelude hiding (head,tail)
import Queue

data PhysicistsQueue a = Q [a] Int [a] Int [a] deriving (Show)

instance Queue PhysicistsQueue where
  empty = Q [] 0 [] 0 []
  isEmpty (Q _ lenf _ _ _) = lenf == 0
  
  snoc (Q w lenf f lenr r) x = check w lenf f (lenr+1) (x:r)
  
  head (Q [] _ _ _ _) = error "empty queue"
  head (Q (x:_) _ _ _ _) = x
  
  tail (Q [] _ _ _ _) = error "empty queue"
  tail (Q (_:w) lenf (_:f) lenr r) = check w (lenf-1) f lenr r
  
check :: [a] -> Int -> [a] -> Int -> [a] -> PhysicistsQueue a
check w lenf f lenr r
  | lenr <= lenf = checkw w lenf f lenr r
  | otherwise    = checkw f (lenf+lenr) (f ++ reverse r) 0 []
                   
checkw :: [a] -> Int -> [a] -> Int -> [a] -> PhysicistsQueue a
checkw [] lenf f lenr r = Q f lenf f lenr r
checkw w lenf f lenr r = Q w lenf f lenr r