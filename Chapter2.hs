-- ============================
-- HC2T1 - Task 1: Checking Types in GHCi
-- ============================
-- Expected Types:
-- 42             :: Int
-- 3.14           :: Double
-- "Haskell"      :: String
-- 'Z'            :: Char
-- True && False  :: Bool

-- We can't dynamically check types in main,
-- but we can display the values and their expected types.

-- ============================
-- HC2T2 - Task 2: Function Type Signatures
-- ============================

add :: Int -> Int -> Int
add x y = x + y

isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

concatStrings :: String -> String -> String
concatStrings s1 s2 = s1 ++ s2

-- ============================
-- HC2T3 - Task 3: Immutable Variables
-- ============================

myAge :: Int
myAge = 21

piValue :: Double
piValue = 3.14159

greeting :: String
greeting = "Hello, Haskell!"

isHaskellFun :: Bool
isHaskellFun = True

-- ============================
-- HC2T4 - Task 4: Converting Between Infix and Prefix
-- ============================
-- Prefix Notation:
-- 5 + 3          → (+) 5 3
-- 10 * 4         → (*) 10 4
-- True && False  → (&&) True False
--
-- Infix Notation:
-- (+) 7 2        → 7 + 2
-- (*) 6 5        → 6 * 5
-- (&&) True False → True && False

-- ============================
-- HC2T5 - Task 5: Defining and Using Functions
-- ============================

circleArea :: Float -> Float
circleArea r = pi * r * r

maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c = max a (max b c)

-- ============================
-- HC2T6 - Task 6: Understanding Int vs Integer
-- ============================

smallNumber :: Int
smallNumber = 262

bigNumber :: Integer
bigNumber = 2127

-- Note: In GHCi, try evaluating:
-- 2^64 :: Int
-- (This causes overflow on most systems.)

-- ============================
-- HC2T7 - Task 7: Boolean Expressions
-- ============================

boolExpr1 :: Bool
boolExpr1 = True && True     -- True

boolExpr2 :: Bool
boolExpr2 = False || False   -- False

boolExpr3 :: Bool
boolExpr3 = not False        -- True

boolExpr4 :: Bool
boolExpr4 = 5 > 10           -- False

-- ============================
-- Main Function to Run All Tasks
-- ============================

main :: IO ()
main = do
    putStrLn "=== HC2T1: Checking Types ==="
    putStrLn "42 :: Int"
    putStrLn "3.14 :: Double"
    putStrLn "\"Haskell\" :: String"
    putStrLn "'Z' :: Char"
    putStrLn "True && False :: Bool"

    putStrLn "\n=== HC2T2: Function Signatures and Functions ==="
    print (add 5 7)                     -- Expected 12
    print (isEven 10)                   -- Expected True
    print (isEven 7)                    -- Expected False
    print (concatStrings "Hello" " World") -- Expected "Hello World"

    putStrLn "\n=== HC2T3: Immutable Variables ==="
    print myAge                         -- Expected 21
    print piValue                       -- Expected 3.14159
    print greeting                      -- Expected "Hello, Haskell!"
    print isHaskellFun                  -- Expected True

    putStrLn "\n=== HC2T5: Defining and Using Functions ==="
    print (circleArea 5)                -- Expected ~78.54
    print (maxOfThree 3 7 5)            -- Expected 7
    print (maxOfThree 10 15 12)         -- Expected 15

    putStrLn "\n=== HC2T6: Int vs Integer ==="
    print smallNumber                   -- Expected 262
    print bigNumber                     -- Expected 2127
    putStrLn "Try in GHCi: 2^64 :: Int (will overflow!)"

    putStrLn "\n=== HC2T7: Boolean Expressions ==="
    print boolExpr1                     -- Expected True
    print boolExpr2                     -- Expected False
    print boolExpr3                     -- Expected True
    print boolExpr4                     -- Expected False
