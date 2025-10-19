module Main where

-- ==========================================
-- HC6T1: Factorial (Recursive)
-- ==========================================
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- ==========================================
-- HC6T2: Fibonacci (Recursive)
-- ==========================================
fibonacci :: (Integral a) => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- ==========================================
-- HC6T3: Sum of Elements Using foldr
-- ==========================================
sumFoldr :: Num a => [a] -> a
sumFoldr = foldr (+) 0

-- ==========================================
-- HC6T4: Product of Elements Using foldl
-- ==========================================
productFoldl :: Num a => [a] -> a
productFoldl = foldl (*) 1

-- ==========================================
-- HC6T5: Reverse a List (Recursive)
-- ==========================================
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- ==========================================
-- HC6T6: Element Exists in List
-- ==========================================
elementExists :: (Eq a) => a -> [a] -> Bool
elementExists _ [] = False
elementExists y (x:xs)
  | y == x    = True
  | otherwise = elementExists y xs

-- ==========================================
-- HC6T7: List Length
-- ==========================================
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

-- ==========================================
-- HC6T8: Filter Even Numbers
-- ==========================================
filterEvens :: Integral a => [a] -> [a]
filterEvens = filter even

-- ==========================================
-- HC6T9: Map Implementation
-- ==========================================
mapCustom :: (a -> b) -> [a] -> [b]
mapCustom _ [] = []
mapCustom f (x:xs) = f x : mapCustom f xs

-- ==========================================
-- HC6T10: Digits of a Number (Recursive)
-- ==========================================
digits :: Integral a => a -> [a]
digits n
  | n < 10    = [n]
  | otherwise = digits (n `div` 10) ++ [n `mod` 10]

-- ==========================================
-- MAIN (for testing)
-- ==========================================
main :: IO ()
main = do
  putStrLn "===== Haskell Chapter 6 Practical Tasks ====="

  putStrLn "\nHC6T1: Factorial"
  print (factorial 5)  -- 120

  putStrLn "\nHC6T2: Fibonacci"
  print (fibonacci 10)  -- 55

  putStrLn "\nHC6T3: Sum using foldr"
  print (sumFoldr [1,2,3,4,5])  -- 15

  putStrLn "\nHC6T4: Product using foldl"
  print (productFoldl [1,2,3,4,5])  -- 120

  putStrLn "\nHC6T5: Reverse List"
  print (reverseList [1,2,3,4,5])  -- [5,4,3,2,1]

  putStrLn "\nHC6T6: Element Exists"
  print (elementExists 3 [1,2,3,4,5])  -- True
  print (elementExists 6 [1,2,3,4,5])  -- False

  putStrLn "\nHC6T7: List Length"
  print (listLength [10,20,30,40])  -- 4

  putStrLn "\nHC6T8: Filter Even Numbers"
  print (filterEvens [1..10])  -- [2,4,6,8,10]

  putStrLn "\nHC6T9: Map Implementation"
  print (mapCustom (*2) [1,2,3,4])  -- [2,4,6,8]

  putStrLn "\nHC6T10: Digits of a Number"
  print (digits 12345)  -- [1,2,3,4,5]
