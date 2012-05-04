module Permutations where

import Diff (mynub)
import Data.List (sort, delete, findIndex, inits, tails)

type Permutation = [Integer]

isValidPermutation :: Permutation -> Bool
isValidPermutation p = len == length (mynub p) && maximum p == fromIntegral len
    where len = length p

order :: Permutation -> Integer
order p = fromIntegral (length p)

invert :: Permutation -> Permutation
invert p = map snd $ sort $ zip p [1..]

-- apply permutation 'perm' to the list
applyPermutation :: Permutation -> [a] -> [a]
applyPermutation perm xs = map (\x -> xs!!(fromIntegral x - 1)) $ invert perm

idPermutation n = [1..n]
lastPermutation n = reverse [1..n]

-- composition
(*>) :: Permutation -> Permutation -> Permutation
(*>) q p =  map (\x -> q!!(fromIntegral x - 1)) p 

-- alias for 'addTransposition'
(%>) :: Permutation -> (Integer, Integer) -> Permutation
(%>) = addTransposition 

addTransposition :: Permutation -> (Integer, Integer) -> Permutation
addTransposition p (a,b) = zipWith f [1..] p
    where 
    f n x | n == a = p!!(fromIntegral b - 1)
          | n == b = p!!(fromIntegral a - 1)
          | otherwise = x

fromCycle :: [[Integer]] -> Permutation
fromCycle c = (map snd) $ 
              sort (concatMap (\xs -> zip xs (tail xs ++ [head xs])) c)
                                                            
toCycle :: Permutation -> [[Integer]]
toCycle p = mynub $ map findCycle [1..fromIntegral $ length p]
    where
    findCycle n = arrange $ findEnd n n
    findEnd start x
        | next == start = [start]
        | True = next:(findEnd start next)
        where next = p!!(fromIntegral x-1)
    arrange xs = dropWhile (/=m) xs ++ takeWhile (/=m) xs
        where m = minimum xs
  
applyPath :: Permutation -> [(Integer, Integer)] -> Permutation
applyPath xs ys = foldl (%>) xs (reverse ys)

path :: Permutation -> [(Integer, Integer)]
path p = case toFix of
           Nothing -> []
           Just a  -> let b = (fromIntegral a+1, fromIntegral a+2) in 
                              b:(path $ p %> b)
    where
    n = order p
    toFix = findIndex id $ zipWith (<) p (tail p)

allperms :: Integer -> [Permutation]
allperms n = perms [1..n]
    where
    perms [] = [[]]
    perms (x:xs) = [ p ++ [x] ++ s | xs' <- perms xs, (p, s) <- zip (inits xs') (tails xs') ]


