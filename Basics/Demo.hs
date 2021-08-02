module Basics.Demo where

pts :: Int -> Int
pts 1 = 10
pts 2 = 6
pts x
  | x <= 6 = 7 - x
  | otherwise = 0

main :: IO ()
main = do
  putStrLn "The base?"
  base <- getLine
  putStrLn "The height?"
  height <- getLine
  let calcArea base height = read base * read height / 2
  let area = show (calcArea base height)
  putStrLn ("The area of that triangle is " ++ area)

doGuessing :: (Ord a, Read a) => a -> IO ()
doGuessing num = do
  putStrLn "Enter your guess:"
  guess <- getLine
  if read guess < num
    then do
      putStrLn "Too low!"
      doGuessing num
    else
      if read guess > num
        then do
          putStrLn "Too high!"
          doGuessing num
        else putStrLn "You Win!"

main2 :: IO ()
main2 = do
  putStrLn "Enter your name:"
  name <- getLine
  if name == "Simon" || name == "John" || name == "Phil"
    then do
      putStrLn "I think Haskell is a great programming language."
      main2
    else
      if name == "Koen"
        then do
          putStrLn "I think debugging Haskell is fun."
          main2
        else putStrLn "Sorry, I don't know you."

main3 :: IO ()
main3 = do
  putStrLn "Enter your name:"
  name <- getLine
  putStrLn (message name)
  where
    greatLanguage = "I think Haskell is a great programming language."
    message "Simon" = greatLanguage
    message "John" = greatLanguage
    message "Phil" = greatLanguage
    message "Koen" = "I think debugging Haskell is fun."
    message _ = "Sorry, I don't know you."

factorial :: (Eq p, Num p) => p -> p
factorial 0 = 1
factorial n = n * factorial (n - 1)

doubleFactorial :: (Eq p, Num p) => p -> p
doubleFactorial 0 = 1
doubleFactorial 1 = 1
doubleFactorial n = n * doubleFactorial (n - 2)

factorial2 :: (Ord p, Num p) => p -> p
factorial2 n = go n 1
  where
    go n res
      | n > 1 = go (n - 1) (res * n)
      | otherwise = res

-- power 2 3 = power 2 2 * 2
--  power 2 2 = power 2 1 * 2
--    power 2 1 = power 2 0 * 2
--      power 2 0 = 1
--    power 2 1 = 1 * 2
--  power 2 2 = 1 * 2 * 2
-- power 2 3 = 1 * 2 * 2 * 2
power :: (Eq t, Num t, Num p) => p -> t -> p
power _ 0 = 1
power x y = power x (y - 1) * x

{-
仅使用 `plusOne` 函数实现加法算法
-}
plusOne :: Num a => a -> a
plusOne x = x + 1

addition :: (Eq t1, Num t1, Num t2) => t2 -> t1 -> t2
addition x 0 = x
addition x y = addition (plusOne x) (y - 1)

-- log2 4 = log2 2 + 1
--  log2 2 = log2 1 + 1
--    log2 1 = 0
--  log2 2 = 0 + 1
-- log2 4 = 0 + 1 + 1
log2 :: (Num p, Integral t) => t -> p
log2 1 = 0
log2 x = log2 (quot x 2) + 1
