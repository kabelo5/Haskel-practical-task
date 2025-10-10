module Main where
import Text.Printf (printf)

-- =========================
-- HC3T1 - Check if number is positive, negative, or zero
-- =========================
checkNumber :: Int -> String
checkNumber n =
  if n > 0 then "Positive"
  else if n < 0 then "Negative"
  else "Zero"

-- =========================
-- HC3T2 - Determine grade using guards
-- =========================
grade :: Int -> String
grade score
  | score >= 90 = "A"
  | score >= 80 = "B"
  | score >= 70 = "C"
  | score >= 60 = "D"
  | otherwise   = "F"

-- =========================
-- HC3T3 - Convert RGB to Hex using let bindings
-- =========================
rgbToHex :: (Int, Int, Int) -> String
rgbToHex (r, g, b) =
  let rh = printf "%02X" r
      gh = printf "%02X" g
      bh = printf "%02X" b
  in rh ++ gh ++ bh

-- =========================
-- HC3T4 - Calculate triangle area using Heron’s formula
-- =========================
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c =
  let s = (a + b + c) / 2
  in sqrt (s * (s - a) * (s - b) * (s - c))

-- =========================
-- HC3T5 - Determine triangle type using guards
-- =========================
triangleType :: Float -> Float -> Float -> String
triangleType a b c
  | a == b && b == c = "Equilateral"
  | a == b || b == c || a == c = "Isosceles"
  | otherwise = "Scalene"

-- =========================
-- HC3T6 - Check leap year using if-then-else
-- =========================
isLeapYear :: Int -> Bool
isLeapYear year =
  if year `mod` 400 == 0 then True
  else if year `mod` 100 == 0 then False
  else if year `mod` 4 == 0 then True
  else False

-- =========================
-- HC3T7 - Determine season based on month using guards
-- =========================
season :: Int -> String
season m
  | m == 12 || m == 1 || m == 2 = "Winter"
  | m >= 3 && m <= 5 = "Spring"
  | m >= 6 && m <= 8 = "Summer"
  | m >= 9 && m <= 11 = "Autumn"
  | otherwise = "Invalid month"

-- =========================
-- HC3T8 - Calculate BMI category using where
-- =========================
bmiCategory :: Float -> Float -> String
bmiCategory weight height
  | bmi < 18.5 = "Underweight"
  | bmi < 25.0 = "Normal"
  | bmi < 30.0 = "Overweight"
  | otherwise  = "Obese"
  where bmi = weight / (height ^ 2)

-- =========================
-- HC3T9 - Find maximum of three numbers using let
-- =========================
maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c =
  let maxAB = max a b
      maxABC = max maxAB c
  in maxABC

-- =========================
-- HC3T10 - Check if a string is a palindrome using recursion and guards
-- =========================
isPalindrome :: String -> Bool
isPalindrome str
  | length str <= 1 = True
  | head str == last str = isPalindrome (init (tail str))
  | otherwise = False

-- =========================
-- MAIN FUNCTION - Run all tests
-- =========================
main :: IO ()
main = do
  putStrLn "===== HC3T1 - Check Number ====="
  putStrLn $ "checkNumber 5 = " ++ checkNumber 5
  putStrLn $ "checkNumber (-3) = " ++ checkNumber (-3)
  putStrLn $ "checkNumber 0 = " ++ checkNumber 0

  putStrLn "\n===== HC3T2 - Grade ====="
  putStrLn $ "grade 95 = " ++ grade 95
  putStrLn $ "grade 72 = " ++ grade 72
  putStrLn $ "grade 50 = " ++ grade 50

  putStrLn "\n===== HC3T3 - RGB to Hex ====="
  putStrLn $ "rgbToHex (255,0,127) = " ++ rgbToHex (255, 0, 127)
  putStrLn $ "rgbToHex (0,255,64) = " ++ rgbToHex (0, 255, 64)

  putStrLn "\n===== HC3T4 - Triangle Area ====="
  putStrLn $ "triangleArea 3 4 5 = " ++ show (triangleArea 3 4 5)
  putStrLn $ "triangleArea 7 8 9 = " ++ show (triangleArea 7 8 9)

  putStrLn "\n===== HC3T5 - Triangle Type ====="
  putStrLn $ "triangleType 3 3 3 = " ++ triangleType 3 3 3
  putStrLn $ "triangleType 5 5 8 = " ++ triangleType 5 5 8
  putStrLn $ "triangleType 6 7 8 = " ++ triangleType 6 7 8

  putStrLn "\n===== HC3T6 - Leap Year ====="
  putStrLn $ "isLeapYear 2000 = " ++ show (isLeapYear 2000)
  putStrLn $ "isLeapYear 1900 = " ++ show (isLeapYear 1900)
  putStrLn $ "isLeapYear 2024 = " ++ show (isLeapYear 2024)

  putStrLn "\n===== HC3T7 - Season ====="
  putStrLn $ "season 3 = " ++ season 3
  putStrLn $ "season 7 = " ++ season 7
  putStrLn $ "season 11 = " ++ season 11

  putStrLn "\n===== HC3T8 - BMI Category ====="
  putStrLn $ "bmiCategory 70 1.75 = " ++ bmiCategory 70 1.75
  putStrLn $ "bmiCategory 90 1.8 = " ++ bmiCategory 90 1.8

  putStrLn "\n===== HC3T9 - Max of Three ====="
  putStrLn $ "maxOfThree 10 20 15 = " ++ show (maxOfThree 10 20 15)
  putStrLn $ "maxOfThree 5 25 10 = " ++ show (maxOfThree 5 25 10)

  putStrLn "\n===== HC3T10 - Palindrome ====="
  putStrLn $ "isPalindrome \"racecar\" = " ++ show (isPalindrome "racecar")
  putStrLn $ "isPalindrome \"haskell\" = " ++ show (isPalindrome "haskell")
  putStrLn $ "isPalindrome \"madam\" = " ++ show (isPalindrome "madam")
