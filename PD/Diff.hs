module Diff where
import Data.List (sort, group)

mynub :: (Ord a) => [a] -> [a]
mynub = (map head).group.sort

prolong :: (Num a) => Int -> [a] -> [a]
prolong n xs = take n $ xs ++ repeat 0
