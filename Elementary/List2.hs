module Elementary.List2 where

import Data.List (group)

multipleList :: Num t => t -> [t] -> [t]
multipleList m (n : ns) = (m * n) : multipleList m ns
multipleList _ _ = []

doubleList :: [Integer] -> [Integer]
doubleList = multipleList 2

takeInt :: (Eq t, Num t) => t -> [a] -> [a]
takeInt 0 _ = []
takeInt _ [] = []
takeInt n (x : xs) = x : takeInt (n - 1) xs

dropInt :: (Eq t, Num t) => t -> [a] -> [a]
dropInt 0 xs = xs
dropInt _ [] = []
dropInt n (x : xs) = dropInt (n - 1) xs

sumInt :: Num p => [p] -> p
sumInt (x : xs) = x + sumInt xs
sumInt _ = 0

scanSum :: Num a => [a] -> [a]
scanSum [] = []
scanSum [x] = [x]
scanSum (x : y : xs) = x : scanSum ((x + y) : xs)

scanSum2 :: [Integer] -> [Integer]
scanSum2 = go 0
  where
    go total [] = []
    go total (x : xs) = total' : go total' xs
      where
        total' = x + total

diffs :: Num a => [a] -> [a]
diffs (x : y : xs) = (y - x) : diffs (y : xs)
-- diffs [] = []
-- diffs [x] = []
diffs _ = []

diffs2 :: Num a => [a] -> [a]
diffs2 [] = []
diffs2 (x : xs) = go (x : xs) xs
  where
    go _ [] = []
    go [] _ = []
    go (x : xs) (y : ys) = (y - x) : go xs ys

apply2Integers :: (Integer -> Integer) -> [Integer] -> [Integer]
apply2Integers _ [] = []
apply2Integers f (x : xs) = f x : apply2Integers f xs

--
-- For Map
--

negateList :: [Integer] -> [Integer]
negateList = map negate

divisors :: Integral a => a -> [a]
divisors p = [f | f <- [1 .. p], p `mod` f == 0]

negateDivisors :: [Integer] -> [[Integer]]
negateDivisors = map (negateList . divisors)

{-
REL: Run Length Encoding.
     Input "aaaabbaaa" output [(4, 'a'), (2, 'b'), (3, 'a')]
-}
myRLEEncoder :: String -> [(Int, Char)]
myRLEEncoder = map pairREL . group
  where
    pairREL s = (length s, head s)

myRLEDecoder :: [(Int, Char)] -> String
myRLEDecoder = concatMap extendREL
  where
    extendREL (n, c) = replicate n c
