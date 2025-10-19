module Main where

-- ==========================================
-- HC8T1: Type Synonyms and Basic Function
-- ==========================================
type Address = String
type Value = Int

generateTx :: Address -> Address -> Value -> String
generateTx fromAddr toAddr val =
  "From: " ++ fromAddr ++ ", To: " ++ toAddr ++ ", Value: " ++ show val


-- ==========================================
-- HC8T2: New Types and Data Constructors
-- ==========================================
data PaymentMethod = Cash | Card | Cryptocurrency deriving (Show)

data Person = Person
  { personName :: String
  , personAddress :: (String, Int)
  , paymentMethod :: PaymentMethod
  } deriving (Show)

bob :: Person
bob = Person "Bob" ("Main Street", 42) Cash


-- ==========================================
-- HC8T3: Algebraic Data Types and Functions
-- ==========================================
data Shape = Circle Float | Rectangle Float Float deriving (Show)

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h


-- ==========================================
-- HC8T4: Record Syntax for Employee
-- ==========================================
data Employee = Employee
  { name :: String
  , experienceInYears :: Float
  } deriving (Show)

richard :: Employee
richard = Employee { name = "Richard", experienceInYears = 7.5 }


-- ==========================================
-- HC8T5: Record Syntax for Person
-- ==========================================
data PersonRec = PersonRec
  { pname :: String
  , age :: Int
  , isEmployed :: Bool
  } deriving (Show)

person1 :: PersonRec
person1 = PersonRec { pname = "Alice", age = 30, isEmployed = True }

person2 :: PersonRec
person2 = PersonRec { pname = "John", age = 25, isEmployed = False }


-- ==========================================
-- HC8T6: Record Syntax for Shape Variants
-- ==========================================
data ShapeVariant
  = CircleShape { center :: (Float, Float), color :: String, radius :: Float }
  | RectangleShape { width :: Float, height :: Float, color :: String }
  deriving (Show)

circleExample :: ShapeVariant
circleExample = CircleShape { center = (0, 0), color = "Red", radius = 5.0 }

rectangleExample :: ShapeVariant
rectangleExample = RectangleShape { width = 10, height = 5, color = "Blue" }


-- ==========================================
-- HC8T7: Data Types and Describing Animals
-- ==========================================
data Animal = Dog String | Cat String deriving (Show)

describeAnimal :: Animal -> String
describeAnimal (Dog name) = "This is a dog named " ++ name ++ "."
describeAnimal (Cat name) = "This is a cat named " ++ name ++ "."

dog1 :: Animal
dog1 = Dog "Buddy"

cat1 :: Animal
cat1 = Cat "Mittens"


-- ==========================================
-- HC8T8: Type Synonyms and Greeting Function
-- ==========================================
type Name = String
type Age = Int

greet :: Name -> Age -> String
greet name age = "Hello, my name is " ++ name ++ " and I am " ++ show age ++ " years old."


-- ==========================================
-- HC8T9: Record Type and Transaction Function
-- ==========================================
data Transaction = Transaction
  { from :: Address
  , to :: Address
  , amount :: Value
  , transactionId :: String
  } deriving (Show)

createTransaction :: Address -> Address -> Value -> String
createTransaction fromAddr toAddr val =
  let txId = fromAddr ++ "-" ++ toAddr ++ "-" ++ show val
      _ = Transaction { from = fromAddr, to = toAddr, amount = val, transactionId = txId }
  in txId


-- ==========================================
-- HC8T10: Deriving Show for Book
-- ==========================================
data Book = Book
  { title :: String
  , author :: String
  , year :: Int
  } deriving (Show)

bookExample :: Book
bookExample = Book { title = "Learn You a Haskell", author = "Miran Lipovaca", year = 2011 }


-- ==========================================
-- MAIN (for testing)
-- ==========================================
main :: IO ()
main = do
  putStrLn "===== Haskell Chapter 8 Practical Tasks ====="

  putStrLn "\nHC8T1: Type Synonyms and Basic Function"
  print (generateTx "Alice" "Bob" 100)

  putStrLn "\nHC8T2: New Types and Data Constructors"
  print bob

  putStrLn "\nHC8T3: Algebraic Data Types and Functions"
  print (area (Circle 5))
  print (area (Rectangle 10 5))

  putStrLn "\nHC8T4: Record Syntax for Employee"
  print richard

  putStrLn "\nHC8T5: Record Syntax for Person"
  print person1
  print person2

  putStrLn "\nHC8T6: Record Syntax for Shape Variants"
  print circleExample
  print rectangleExample

  putStrLn "\nHC8T7: Data Types and Describing Animals"
  print (describeAnimal dog1)
  print (describeAnimal cat1)

  putStrLn "\nHC8T8: Type Synonyms and Greeting Function"
  print (greet "Kabelo" 22)

  putStrLn "\nHC8T9: Record Type and Transaction Function"
  print (createTransaction "Alice" "Bob" 500)

  putStrLn "\nHC8T10: Deriving Show for Book"
  print bookExample
