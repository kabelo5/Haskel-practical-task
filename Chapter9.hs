module Main where

-- ==========================================
-- HC9T1: Define a Parametric Type Synonym
-- ==========================================
type Entity a = (String, a)
-- Example: Entity Int -> ("Alice", 1001)


-- ==========================================
-- HC9T2: Implement a Parametric Data Type
-- ==========================================
data Box a = Empty | Has a deriving (Show)


-- ==========================================
-- HC9T3: Function to Add Values in a Box
-- ==========================================
addN :: Num a => a -> Box a -> Box a
addN n Empty = Empty
addN n (Has x) = Has (x + n)


-- ==========================================
-- HC9T4: Extract a Value from a Box
-- ==========================================
extract :: a -> Box a -> a
extract def Empty = def
extract _ (Has x) = x


-- ==========================================
-- HC9T5: Parametric Data Type with Record Syntax
-- ==========================================
data Shape a
  = Circle { color :: a, radius :: Float }
  | Rectangle { color :: a, width :: Float, height :: Float }
  deriving (Show)

circleExample :: Shape String
circleExample = Circle { color = "Red", radius = 5.0 }

rectangleExample :: Shape String
rectangleExample = Rectangle { color = "Blue", width = 10, height = 4 }


-- ==========================================
-- HC9T6: Recursive Data Type for Tweets
-- ==========================================
data Tweet = Tweet
  { content :: String
  , likes :: Int
  , comments :: [Tweet]
  } deriving (Show)

-- Example tweets for testing
tweet3 :: Tweet
tweet3 = Tweet "I love Haskell!" 3 []

tweet2 :: Tweet
tweet2 = Tweet "Functional Programming Rocks!" 5 [tweet3]

tweet1 :: Tweet
tweet1 = Tweet "Hello World!" 10 [tweet2]


-- ==========================================
-- HC9T7: Engagement Function for Tweets
-- ==========================================
engagement :: Tweet -> Int
engagement (Tweet _ likes comments) =
  likes + sum (map engagement comments)


-- ==========================================
-- HC9T8: Recursive Sequence Data Type
-- ==========================================
data Sequence a = End | Node a (Sequence a) deriving (Show)

-- Example sequence: Node 1 (Node 2 (Node 3 End))


-- ==========================================
-- HC9T9: Check for Element in a Sequence
-- ==========================================
elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ End = False
elemSeq x (Node y rest)
  | x == y    = True
  | otherwise = elemSeq x rest


-- ==========================================
-- HC9T10: Binary Search Tree Data Type
-- ==========================================
data BST a = EmptyBST | NodeBST a (BST a) (BST a) deriving (Show)

-- Example BST:
-- NodeBST 5 (NodeBST 3 EmptyBST EmptyBST) (NodeBST 7 EmptyBST EmptyBST)


-- ==========================================
-- MAIN (for testing all tasks)
-- ==========================================
main :: IO ()
main = do
  putStrLn "===== Haskell Chapter 9 Practical Tasks ====="

  putStrLn "\nHC9T1: Parametric Type Synonym"
  let entity1 :: Entity Int
      entity1 = ("Alice", 1234)
  print entity1

  putStrLn "\nHC9T2: Parametric Data Type (Box)"
  print (Has 10)
  print (Empty :: Box Int)

  putStrLn "\nHC9T3: Add Values in a Box"
  print (addN 5 (Has 10))
  print (addN 5 Empty)

  putStrLn "\nHC9T4: Extract a Value from a Box"
  print (extract 0 (Has 50))
  print (extract 100 Empty)

  putStrLn "\nHC9T5: Parametric Shape with Record Syntax"
  print circleExample
  print rectangleExample

  putStrLn "\nHC9T6: Recursive Data Type for Tweets"
  print tweet1

  putStrLn "\nHC9T7: Engagement Function for Tweets"
  print (engagement tweet1)  -- should sum likes from all nested tweets

  putStrLn "\nHC9T8: Recursive Sequence Data Type"
  let seq1 = Node 1 (Node 2 (Node 3 End))
  print seq1

  putStrLn "\nHC9T9: Check for Element in a Sequence"
  print (elemSeq 2 seq1)
  print (elemSeq 5 seq1)

  putStrLn "\nHC9T10: Binary Search Tree Data Type"
  let bst = NodeBST 5 (NodeBST 3 EmptyBST EmptyBST) (NodeBST 7 EmptyBST EmptyBST)
  print bst
