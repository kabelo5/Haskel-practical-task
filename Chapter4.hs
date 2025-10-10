module Main where

-- HC4T1
weatherReport :: String -> String
weatherReport "sunny"  = "It's a bright and beautiful day!"
weatherReport "rainy"  = "Don't forget your umbrella!"
weatherReport "cloudy" = "A bit gloomy, but no rain yet!"
weatherReport _        = "Weather unknown"

-- HC4T2
dayType :: String -> String
dayType "Saturday" = "It's a weekend!"
dayType "Sunday"   = "It's a weekend!"
dayType "Monday"   = "It's a weekday."
dayType "Tuesday"  = "It's a weekday."
dayType "Wednesday"= "It's a weekday."
dayType "Thursday" = "It's a weekday."
dayType "Friday"   = "It's a weekday."
dayType _          = "Invalid day"

-- HC4T3
gradeComment :: Int -> String
gradeComment grade
  | grade >= 90 && grade <= 100 = "Excellent!"
  | grade >= 70 && grade <= 89  = "Good job!"
  | grade >= 50 && grade <= 69  = "You passed."
  | grade >= 0  && grade <= 49  = "Better luck next time."
  | otherwise                   = "Invalid grade"

-- HC4T4 and HC4T5
specialBirthdayWithAge :: Int -> String
specialBirthdayWithAge 1  = "First birthday – how special!"
specialBirthdayWithAge 18 = "You're an adult now!"
specialBirthdayWithAge 21 = "Cheers to 21!"
specialBirthdayWithAge 50 = "Half a century!"
specialBirthdayWithAge age = "Happy " ++ show age ++ "th birthday!"

-- HC4T6
whatsInsideThisList :: [a] -> String
whatsInsideThisList []        = "The list is empty."
whatsInsideThisList [x]       = "The list has one element."
whatsInsideThisList [x, y]    = "The list has two elements."
whatsInsideThisList _         = "The list has many elements."

-- HC4T7
firstAndThird :: [a] -> (a, a)
firstAndThird (x : _ : z : _) = (x, z)
firstAndThird _ = error "List must have at least three elements."

-- HC4T8
describeTuple :: (String, Int, Bool) -> String
describeTuple (name, age, isStudent) =
  name ++ " is " ++ show age ++ " years old and " ++
  (if isStudent then "is a student." else "is not a student.")

-- Main
main :: IO ()
main = do
  putStrLn "===== HC4T1 ====="
  print $ weatherReport "sunny"
  print $ weatherReport "snowy"

  putStrLn "\n===== HC4T2 ====="
  print $ dayType "Monday"
  print $ dayType "Sunday"

  putStrLn "\n===== HC4T3 ====="
  print $ gradeComment 95
  print $ gradeComment 45

  putStrLn "\n===== HC4T4 / HC4T5 ====="
  print $ specialBirthdayWithAge 21
  print $ specialBirthdayWithAge 30

  putStrLn "\n===== HC4T6 ====="
  print $ whatsInsideThisList ([] :: [Int])
  print $ whatsInsideThisList [1]
  print $ whatsInsideThisList [1,2]
  print $ whatsInsideThisList [1,2,3]

  putStrLn "\n===== HC4T7 ====="
  print $ firstAndThird [10,20,30,40]

  putStrLn "\n===== HC4T8 ====="
  print $ describeTuple ("Alice", 25, True)
