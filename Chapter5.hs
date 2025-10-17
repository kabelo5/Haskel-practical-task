-- HC5 – Improving and Combining Functions
-- Paste this into Main.hs and click Run

import Data.Char (isUpper)

-- HC5T1: Using applyTwice (actually apply 3 times)
applyThrice :: (a -> a) -> a -> a
applyThrice f = f . f . f

-- HC5T2: Filtering Odd Numbers (1..30)
odds1to30 :: [Int]
odds1to30 = filter odd [1..30]

-- HC5T3: Checking for Uppercase Letters at word start
anyStartsUpper :: [String] -> Bool
anyStartsUpper = any (\w -> not (null w) && isUpper (head w))

-- HC5T4: Using Lambda Functions
biggerThan10 :: (Ord a, Num a) => a -> Bool
biggerThan10 = \x -> x > 10

-- HC5T5: Partial Application
multiplyByFive :: Num a => a -> a
multiplyByFive = (* 5)

-- HC5T6: Function Composition (square then keep even)
evenSquares :: [Int] -> [Int]
evenSquares = filter even . map (^2)

-- HC5T7: The $ Operator
result :: Int
result = sum $ map (*2) $ filter (>3) [1..10]

-- HC5T8: Point-Free Style
addFive :: Num a => a -> a
addFive = (+ 5)

-- HC5T9: Higher-Order Function to Transform a List (apply f twice to each)
transformList :: (a -> a) -> [a] -> [a]
transformList f = map (f . f)

-- HC5T10: Combine filter, map, any → any squared value > 50?
-- (Uses all three as requested.)
hasSquareGreaterThan50 :: (Num a, Ord a) => [a] -> Bool
hasSquareGreaterThan50 = any (const True) . filter (>50) . map (\x -> x*x)

-- ---- Demo ----
main :: IO ()
main = do
  putStrLn "HC5T1 applyThrice (+1) 10  =>"
  print (applyThrice (+1) 10)                   -- 13

  putStrLn "HC5T2 odds1to30 =>"
  print odds1to30

  putStrLn "HC5T3 anyStartsUpper [\"apple\",\"Banana\",\"cherry\"] =>"
  print (anyStartsUpper ["apple","Banana","cherry"])  -- True

  putStrLn "HC5T4 biggerThan10 9 / 11 =>"
  print (biggerThan10 9, biggerThan10 11)

  putStrLn "HC5T5 multiplyByFive 7 =>"
  print (multiplyByFive 7)                      -- 35

  putStrLn "HC5T6 evenSquares [1..10] =>"
  print (evenSquares [1..10])

  putStrLn "HC5T7 result =>"
  print result

  putStrLn "HC5T8 addFive 12 =>"
  print (addFive 12)                            -- 17

  putStrLn "HC5T9 transformList (*2) [1,2,3] =>"
  print (transformList (*2) [1,2,3])            -- [4,8,12]

  putStrLn "HC5T10 hasSquareGreaterThan50 [3,4,5,6] =>"
  print (hasSquareGreaterThan50 [3,4,5,6])      -- True
