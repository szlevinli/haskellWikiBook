module Elementary.Recursion where

{-
myReplicate 3 'w' = 'w' : myReplicate 2 'w'
  myReplicate 2 'w' = 'w' : myReplicate 1 'w'
    myReplicate 1 'w' = 'w' : myReplicate 0 'w'
      myReplicate 0 'w' = []
    myReplicate 1 'w' = 'w' : []
  myReplicate 2 'w' = 'w' : 'w' : []
myReplicate 3 'w' = 'w' : w' : 'w' : []
-}
myReplicate :: (Eq t, Num t) => t -> a -> [a]
myReplicate 0 _ = []
myReplicate n a = a : myReplicate (n - 1) a

(!!) :: (Eq t, Num t) => [p] -> t -> p
[] !! _ = error "Index too large"
(x : _) !! 0 = x
(x : xs) !! n = xs Elementary.Recursion.!! (n - 1)

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys

myZip2 :: [a] -> [b] -> [(a, b)]
myZip2 (x : xs) (y : ys) = (x, y) : myZip xs ys
myZip2 _ _ = []

myLength :: Num t => [a] -> t
myLength xs = go 0 xs
  where
    go acc [] = acc
    go acc (_ : xs) = go (acc + 1) xs

myLength2 :: Num p => [a] -> p
myLength2 (_ : xs) = 1 + myLength2 xs
myLength2 _ = 0