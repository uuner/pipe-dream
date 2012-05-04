module PipeDream where
import Permutations
import Data.List (transpose, intersperse, sort, group, groupBy)
import Diff (prolong)

newtype PipeDream = PD [[Int]] deriving (Eq, Ord)

instance Show PipeDream where
    show (PD []) = "\nemptyPD\n"
    show (PD [[]]) = "\nemptyPD\n"
    show p = "\n/" ++ replicate l '-' ++ "\\\n" ++
             concatMap (\a -> "|" ++ 
                       concatMap (\x -> if x == 0 then " " else "+") a 
                       ++ " |\n") (toRows p)
              ++ "\\" ++ replicate l '-' ++ "/\n" 
              where l = lengthPD p
 
isValidPD :: PipeDream -> Bool
isValidPD (PD x) = (and $ zipWith (==) (reverse (map length x)) [1..])
                   && (all (\y -> all (\a -> a == 0 || a == 1) y) x)

lengthPD :: PipeDream -> Int
lengthPD (PD x) = 1 + length (head x)

showPD :: PipeDream -> String
showPD (PD p) = show p
                             
toRows :: PipeDream -> [[Int]]
toRows d = transpose $ toColumns d

toColumns :: PipeDream -> [[Int]]
toColumns (PD x) = map (prolong l) x
    where
    l = (length $ head x) + 1

fromColumns :: [[Int]] -> PipeDream
fromColumns xs = PD $ zipWith (\n s -> take n s) [m-1,m-2..1] (init xs)
    where m = length xs

fromRows :: [[Int]] -> PipeDream
fromRows xs = fromColumns (transpose xs)

--i - row, j - column
getDream (PD x) i j = x!!(j-1)!!(i-1)

start :: Int -> PipeDream -> Int
start i d = 1 + (length $ takeWhile (/=0) row)
  where row = (toRows d)!!(i-1)

bigJ :: Int -> PipeDream -> [Int]
bigJ i d = [x | x <- [1..(s-1)], (col!!(x-1)!!i) /= 1] 
  where 
  s = start i d
  col = toColumns d

mitosis :: Int -> PipeDream -> [PipeDream]
mitosis i d@(PD a) = [offspring p | p <- bigJ i d]
    where 
    replace i x xs = take (i-1) xs ++ x:(drop i xs)
    movedown i xs = take (i-1) xs ++ 
                    (if xs!!i == 1 then [xs!!(i-1),1] else [0,xs!!(i-1)]) ++ 
                    (drop (i+1) xs)
    offspring p = PD (map (\(xs,j) -> f j xs) $ zip a [1..])
        where
        f j xs
          | j == p = replace i 0 xs
          | j < p  = movedown i xs
          | otherwise = xs 

lastPD :: Integer -> PipeDream
lastPD n = PD $ reverse $ map (\x -> replicate (fromIntegral x) 1) [1..n - 1] 

newtype Polynomial = PL [([Int], Int)]

instance Show Polynomial where
    show poly 
        = let (PL p) = foldPoly poly 
          in concat $
             intersperse "+" $
             map (\(xs, n) -> (if n /= 1 then show n ++ "*" else []) ++ 
                   concatMap (\(a,b) -> if b == 0 then [] else
                                        "x_" ++ show a ++ 
                                         (if b /= 1 
                                          then "^{"++ show b ++ "}" 
                                          else "")) 
                             (zip [1..] xs)) p

foldPoly (PL p) = PL $ map (\xs -> (fst (head xs), sum (map snd xs))) $
                     groupBy (\x y -> fst x == fst y) $
             sort p
 
perm2pipes :: Permutation -> [PipeDream]
perm2pipes perm
    | perm == lastPermutation n = [lastPD n]
    | True = (foldl (.) id $  
              map (\x -> concatMap (mitosis $ fromIntegral x)) way) [lastPD n]
    where
    n = order perm
    way = map fst (path perm)

 
pipes2poly :: [PipeDream] -> Polynomial
pipes2poly ps = PL (map monom ps)
    where 
    monom p = (map (\xs -> length $ filter (==1) xs) (toRows p), 1)

polyList (PL p) = p

coefNub xs = map (\x -> (length x, head x)) $ group $ sort xs

newtype TupleNewline a b c d = T4 (a,b,c,d) deriving (Eq, Ord)
instance (Show a, Show b, Show c, Show d) => Show (TupleNewline a b c d) where
    show (T4 (a,b,c,d)) = "(" ++ 
                          show a ++ ",\n" ++ 
                          show b ++ ",\n" ++ 
                          show c ++ ",\n" ++ 
                          show d ++ ")\n"

allfor n = map (\p -> let pp = perm2pipes p
                          poly = pipes2poly pp
                          ls = map fst (polyList poly)
                      in T4 (p, pp, poly, ls)) 
                $ allperms n

