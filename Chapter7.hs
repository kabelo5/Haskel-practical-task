module Main where

-- ==========================================
-- HC7T1: Implement an Eq Instance for a Custom Data Type
-- ==========================================
data Color = Red | Green | Blue
  deriving (Show, Read, Enum, Bounded)

instance Eq Color where
  Red   == Red   = True
  Green == Green = True
  Blue  == Blue  = True
  _     == _     = False

-- ==========================================
-- HC7T2: Implement an Ord Instance for a Custom Data Type
-- (Red < Green < Blue)
-- ==========================================
instance Ord Color where
  compare Red Green = LT
  compare Red Blue  = LT
  compare Green Blue = LT
  compare Green Red = GT
  compare Blue Red  = GT
  compare Blue Green = GT
  compare _ _ = EQ

-- ==========================================
-- HC7T3: Function Using Multiple Constraints
-- ==========================================
compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues x y
  | x >= y    = x
  | otherwise = y

-- ==========================================
-- HC7T4: Custom Type with Show and Read
-- ==========================================
data Shape = Circle Double | Rectangle Double Double
  deriving (Eq, Read, Show)

-- ==========================================
-- Add Ord Instance for Shape (compare by area)
-- ==========================================
instance Ord Shape where
  compare s1 s2 = compare (area s1) (area s2)
    where
      area (Circle r) = pi * r * r
      area (Rectangle w h) = w * h

-- ==========================================
-- HC7T5: Function with Num Constraint
-- ==========================================
squareArea :: Num a => a -> a
squareArea side = side * side

-- ==========================================
-- HC7T6: Using Integral and Floating Type Classes
-- ==========================================
circleCircumference :: (Floating a, Real a) => a -> a
circleCircumference r = 2 * pi * r

-- ==========================================
-- HC7T7: Bounded and Enum
-- ==========================================
nextColor :: Color -> Color
nextColor c
  | c == maxBound = minBound
  | otherwise = succ c

-- ==========================================
-- HC7T8: Parse a Value from a String Using Read
-- ==========================================
parseShape :: String -> Maybe Shape
parseShape str =
  case reads str :: [(Shape, String)] of
    [(s, "")] -> Just s
    _         -> Nothing

-- ==========================================
-- HC7T9: Type Class with Multiple Instances
-- ==========================================
class Describable a where
  describe :: a -> String

instance Describable Bool where
  describe True  = "This is True."
  describe False = "This is False."

instance Describable Shape where
  describe (Circle r) = "A Circle with radius " ++ show r
  describe (Rectangle w h) = "A Rectangle with width " ++ show w ++ " and height " ++ show h

-- ==========================================
-- HC7T10: Function with Multiple Type Class Constraints
-- ==========================================
describeAndCompare :: (Describable a, Ord a) => a -> a -> String
describeAndCompare x y =
  if x >= y
  then "Larger value: " ++ describe x
  else "Larger value: " ++ describe y

-- ==========================================
-- MAIN (for testing)
-- ==========================================
main :: IO ()
main = do
  putStrLn "===== Haskell Chapter 7 Practical Tasks ====="

  putStrLn "\nHC7T1 & HC7T2: Eq and Ord for Color"
  print (Red == Red)           -- True
  print (Red < Green)          -- True
  print (Blue > Green)         -- True

  putStrLn "\nHC7T3: compareValues"
  print (compareValues 5 10)   -- 10

  putStrLn "\nHC7T4: Shape Show and Read"
  let s1 = Circle 3.5
  let s2 = Rectangle 4.0 6.0
  print s1
  print (read "Rectangle 4.0 6.0" :: Shape)

  putStrLn "\nHC7T5: squareArea"
  print (squareArea 5)         -- 25

  putStrLn "\nHC7T6: circleCircumference"
  print (circleCircumference 5) -- ≈ 31.4159

  putStrLn "\nHC7T7: nextColor"
  print (nextColor Red)        -- Green
  print (nextColor Green)      -- Blue
  print (nextColor Blue)       -- Red (wraps around)

  putStrLn "\nHC7T8: parseShape"
  print (parseShape "Circle 2.5")     -- Just (Circle 2.5)
  print (parseShape "Triangle 5 5")   -- Nothing

  putStrLn "\nHC7T9: Describable Type Class"
  print (describe True)
  print (describe s1)
  print (describe s2)

  putStrLn "\nHC7T10: describeAndCompare"
  print (describeAndCompare (Rectangle 3 4) (Rectangle 5 6))
