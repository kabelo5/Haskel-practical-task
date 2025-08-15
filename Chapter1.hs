import Data.List (sortBy)
import Data.Ord (comparing)

-- ============================
-- HC1T1 - Task 1: Function Composition
-- ============================

double :: Num a => a -> a
double x = x * 2

increment :: Num a => a -> a
increment x = x + 1

doubleThenIncrement :: Num a => a -> a
doubleThenIncrement = increment . double

-- ============================
-- HC1T2 - Task 2: Pure Function Example
-- ============================

circleArea :: Floating a => a -> a
circleArea r = pi * r ^ 2

-- ============================
-- HC1T3 - Task 3: Checking if a Number is Greater than 18
-- ============================

greaterThan18 :: (Ord a, Num a) => a -> Bool
greaterThan18 n = n > 18

-- ============================
-- HC1T4 - Task 4: Composing a Function to Process Player Data
-- ============================

extractPlayers :: [(String, Int)] -> [String]
extractPlayers players = [name | (name, _) <- players]

sortByScore :: [(String, Int)] -> [(String, Int)]
sortByScore = sortBy (flip (comparing snd))

topThree :: [(String, Int)] -> [(String, Int)]
topThree players = take 3 players

getTopThreePlayers :: [(String, Int)] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore

-- ============================
-- HC1T5 - Task 5: Laziness in Haskell
-- ============================

infiniteNumbers :: [Integer]
infiniteNumbers = [1..]

firstN :: Int -> [Integer]
firstN n = take n infiniteNumbers

-- ============================
-- HC1T6 - Task 6: Using Type Signatures
-- ============================

addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

-- ============================
-- HC1T7 - Task 7: Converting Fahrenheit to Celsius
-- ============================

fToC :: Floating a => a -> a
fToC f = (f - 32) * (5 / 9)

-- ============================
-- HC1T8 - Task 8: Higher-Order Functions
-- ============================

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- ============================
-- Main Function to Run All Tasks
-- ============================

main :: IO ()
main = do
    putStrLn "=== HC1T1: Function Composition ==="
    print (doubleThenIncrement 3) -- Expected 7

    putStrLn "\n=== HC1T2: Pure Function Example ==="
    print (circleArea 5) -- Expected ~78.54

    putStrLn "\n=== HC1T3: Greater Than 18 ==="
    print (greaterThan18 20) -- True
    print (greaterThan18 10) -- False

    putStrLn "\n=== HC1T4: Top Three Players ==="
    let players = [("Alice", 50), ("Bob", 70), ("Charlie", 60), ("David", 80)]
    print (getTopThreePlayers players) -- ["David","Bob","Charlie"]

    putStrLn "\n=== HC1T5: Laziness in Haskell ==="
    print (firstN 5) -- [1,2,3,4,5]

    putStrLn "\n=== HC1T6: Add Numbers ==="
    print (addNumbers 3 4) -- 7

    putStrLn "\n=== HC1T7: Fahrenheit to Celsius ==="
    print (fToC 212) -- 100.0

    putStrLn "\n=== HC1T8: Apply Twice ==="
    print (applyTwice (+3) 5) -- 11
