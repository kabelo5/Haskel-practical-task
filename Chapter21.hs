
-- Chapter 21: Readers, Writers, and State — HC21T1..HC21T17

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Char (toUpper)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import Data.Semigroup ((<>))
import Data.Monoid (Sum(..))

--------------------------------------------------------------------------------
-- Writer
--------------------------------------------------------------------------------

newtype Writer w a = Writer { runWriter :: (a, w) }

instance Functor (Writer w) where
  fmap f (Writer (a, w)) = Writer (f a, w)

instance Monoid w => Applicative (Writer w) where
  pure a = Writer (a, mempty)
  Writer (f, w1) <*> Writer (a, w2) = Writer (f a, w1 <> w2)

instance Monoid w => Monad (Writer w) where
  Writer (a, w1) >>= k =
    let Writer (b, w2) = k a
    in Writer (b, w1 <> w2)

tell :: Monoid w => w -> Writer w ()
tell w = Writer ((), w)

listen :: Monoid w => Writer w a -> Writer w (a, w)
listen (Writer (a, w)) = Writer ((a, w), w)

pass :: Monoid w => Writer w (a, w -> w) -> Writer w a
pass (Writer ((a, f), w)) = Writer (a, f w)

--------------------------------------------------------------------------------
-- Reader
--------------------------------------------------------------------------------

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader r) where
  pure a = Reader (\_ -> a)
  Reader rf <*> Reader ra = Reader (\r -> rf r (ra r))

instance Monad (Reader r) where
  Reader ra >>= k = Reader $ \r ->
    runReader (k (ra r)) r

ask :: Reader r r
ask = Reader id

local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader ra) = Reader (ra . f)

--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------

newtype State s a = MkState { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (MkState run) = MkState $ \s -> let (a, s') = run s in (f a, s')

instance Applicative (State s) where
  pure a = MkState (\s -> (a, s))
  MkState rf <*> MkState ra = MkState $ \s ->
    let (f, s1) = rf s
        (a, s2) = ra s1
    in (f a, s2)

instance Monad (State s) where
  MkState ra >>= k = MkState $ \s ->
    let (a, s1) = ra s
    in runState (k a) s1

get :: State s s
get = MkState (\s -> (s, s))

put :: s -> State s ()
put s = MkState (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = MkState (\s -> ((), f s))

evalState :: State s a -> s -> a
evalState st s0 = fst (runState st s0)

execState :: State s a -> s -> s
execState st s0 = snd (runState st s0)

--------------------------------------------------------------------------------
-- HC21T1: Writer Logging Calculator
--------------------------------------------------------------------------------

addW, subW, mulW :: Int -> Int -> Writer [String] Int
addW x y = let r = x + y in tell ["add " ++ show x ++ " " ++ show y ++ " = " ++ show r] >> pure r
subW x y = let r = x - y in tell ["sub " ++ show x ++ " " ++ show y ++ " = " ++ show r] >> pure r
mulW x y = let r = x * y in tell ["mul " ++ show x ++ " " ++ show y ++ " = " ++ show r] >> pure r

calcDemo :: Writer [String] Int
calcDemo = do
  a <- addW 3 4
  b <- mulW a 10
  subW b 5

--------------------------------------------------------------------------------
-- HC21T2: Writer — Law Checks
--------------------------------------------------------------------------------

checkWriterLaws :: Bool
checkWriterLaws =
  let v = Writer (42 :: Int, ["v"])
      lhs1 = (pure id <*> v) :: Writer [String] Int
      rhs1 = v
      m  = Writer (2 :: Int, ["m"])
      k x = Writer (x + 1, ["k"])
      h x = Writer (x * 2, ["h"])
      lhs2 = (m >>= k) >>= h
      rhs2 = m >>= (\x -> k x >>= h)
  in runWriter lhs1 == runWriter rhs1 && runWriter lhs2 == runWriter rhs2

--------------------------------------------------------------------------------
-- HC21T3: Writer — listen / pass
--------------------------------------------------------------------------------

redactSecrets :: Writer [String] a -> Writer [String] a
redactSecrets w =
  pass $ do
    (a, logs) <- listen w
    let f = map (\ln -> if "SECRET" `isInfixOf` map toUpper ln then "[REDACTED]" else ln)
    pure (a, f)

--------------------------------------------------------------------------------
-- HC21T4: Writer with Sum Int
--------------------------------------------------------------------------------

addCount, subCount, mulCount :: Int -> Int -> Writer (Sum Int) Int
addCount x y = tell (Sum 1) >> pure (x + y)
subCount x y = tell (Sum 1) >> pure (x - y)
mulCount x y = tell (Sum 1) >> pure (x * y)

calcCount :: Writer (Sum Int) Int
calcCount = do
  a <- addCount 3 4
  b <- mulCount a 10
  subCount b 5

--------------------------------------------------------------------------------
-- HC21T5: Reader Configurable Greeting
--------------------------------------------------------------------------------

data Config = Config { greetPrefix :: String, shout :: Bool } deriving (Show)

greet :: String -> Reader Config String
greet name = do
  cfg <- ask
  let base = greetPrefix cfg ++ " " ++ name
  pure $ if shout cfg then map toUpper base ++ "!" else base

greetWithFlip :: String -> Reader Config (String, String)
greetWithFlip name = do
  normal <- greet name
  flipped <- local (\c -> c { shout = not (shout c) }) (greet name)
  pure (normal, flipped)

--------------------------------------------------------------------------------
-- HC21T6: Reader Composition
--------------------------------------------------------------------------------

composeReaders :: Reader Config String
composeReaders = do
  a <- greet "Ada"
  b <- greet "Byron"
  pure (a ++ " & " ++ b)

--------------------------------------------------------------------------------
-- HC21T7: Reader Env Refactor
--------------------------------------------------------------------------------

type Env = M.Map String String

getConn :: Reader Env String
getConn = do
  env <- ask
  pure $ "db://" ++ M.findWithDefault "localhost" "DB_HOST" env
               ++ ":" ++ M.findWithDefault "5432" "DB_PORT" env

getConnWithOverridePort :: String -> Reader Env (String, String)
getConnWithOverridePort newPort = do
  normal <- getConn
  overridden <- local (M.insert "DB_PORT" newPort) getConn
  pure (normal, overridden)

--------------------------------------------------------------------------------
-- HC21T8 is covered by get/put/modify primitives above
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- HC21T9: mapCount
--------------------------------------------------------------------------------

mapCount :: (a -> b) -> [a] -> State Int [b]
mapCount f []     = pure []
mapCount f (x:xs) = do
  modify (+1)
  bs <- mapCount f xs
  pure (f x : bs)

--------------------------------------------------------------------------------
-- HC21T10: Vending Machine
--------------------------------------------------------------------------------

data VendingState = MkVendingState { items :: Int, credit :: Int } deriving (Show)

price :: Int
price = 7

insertCoin :: Int -> State VendingState ()
insertCoin c = do
  s <- get
  put s { credit = credit s + c }

vend :: State VendingState String
vend = do
  s <- get
  if items s <= 0
     then pure "Sold out"
     else if credit s < price
            then pure "Insufficient credit"
            else do
              put s { items = items s - 1, credit = credit s - price }
              pure "Vended"

getChange :: State VendingState Int
getChange = do
  s <- get
  put s { credit = 0 }
  pure (credit s)

vendingSequence :: State VendingState (String, Int)
vendingSequence = do
  insertCoin 5
  insertCoin 5
  msg <- vend
  chg <- getChange
  pure (msg, chg)

--------------------------------------------------------------------------------
-- HC21T11: Undo Stack
--------------------------------------------------------------------------------

setValue :: Int -> State (Int, [Int]) ()
setValue new = do
  (cur, hist) <- get
  put (new, cur:hist)

undo :: State (Int, [Int]) ()
undo = do
  (cur, hist) <- get
  case hist of
    (h:hs) -> put (h, hs)
    []     -> pure ()

undoScript :: State (Int,[Int]) (Int, [Int])
undoScript = do
  setValue 10
  setValue 20
  setValue 30
  undo
  undo
  get

--------------------------------------------------------------------------------
-- HC21T12: Random Walk
--------------------------------------------------------------------------------

randomStep :: State (Int, (Int, Int)) ()
randomStep = MkState $ \(seed, (x,y)) ->
  let seed' = (1103515245 * seed + 12345) `mod` 2147483648
      dir   = seed' `mod` 4
      (dx,dy) = case dir of
        0 -> (0, 1)
        1 -> (0,-1)
        2 -> (-1,0)
        _ -> (1, 0)
      pos' = (x + dx, y + dy)
  in ((), (seed', pos'))

randomWalk :: Int -> State (Int, (Int, Int)) [(Int, Int)]
randomWalk n = do
  (_, start) <- get
  go n start [start]
  where
    go 0 _ path = pure (reverse path)
    go k _ path = do
      randomStep
      (_, p') <- get
      go (k-1) p' (p':path)

--------------------------------------------------------------------------------
-- HC21T13: Reader + Writer — Configurable Logging
--------------------------------------------------------------------------------

step :: String -> Reader Config (Writer [String] ())
step msg = do
  cfg <- ask
  let prefix = if shout cfg then map toUpper (greetPrefix cfg) else greetPrefix cfg
  pure $ tell [prefix ++ ": " ++ msg]

stepsDemo :: Reader Config (Writer [String] ())
stepsDemo = do
  a <- step "connect"
  b <- step "run query"
  c <- local (\c -> c { shout = True, greetPrefix = "dbg" }) (step "debug info")
  pure (a >> b >> c)

--------------------------------------------------------------------------------
-- HC21T14: State + Writer — Instrumented State (State s (Writer w a))
--------------------------------------------------------------------------------

inc :: Int -> State Int (Writer [String] Int)
inc n = do
  modify (+ n)
  s <- get
  pure (tell ["inc " ++ show n ++ " -> " ++ show s] >> pure s)

dec :: Int -> State Int (Writer [String] Int)
dec n = do
  modify (subtract n)
  s <- get
  pure (tell ["dec " ++ show n ++ " -> " ++ show s] >> pure s)

instrumentedDemo :: State Int (Writer [String] Int)
instrumentedDemo = do
  _ <- inc 5
  _ <- dec 2
  inc 10

-- Note: With State s (Writer w a) you run State first then extract logs.
-- If you used Writer w (State s a) you'd accumulate logs "outside" but composing
-- with plain State functions can feel less direct.

--------------------------------------------------------------------------------
-- HC21T15: Reader + State — Environment-Driven State Machine
--------------------------------------------------------------------------------

-- Use Config threshold derived from greetPrefix length + shout flag (example).
tick :: Reader Config (State Int Bool)
tick = do
  cfg <- ask
  pure $ do
    modify (+1)
    s <- get
    pure (s >= length (greetPrefix cfg) + (if shout cfg then 1 else 0))

runTicks :: Int -> Reader Config [Bool]
runTicks n = do
  t <- tick
  pure $ evalState (sequence [ t | _ <- [1..n] ]) 0

--------------------------------------------------------------------------------
-- HC21T16: Laws
--------------------------------------------------------------------------------

checkWriterAssoc :: Bool
checkWriterAssoc =
  let m  = Writer (1 :: Int, ["m"])
      k x = Writer (x+1, ["k"])
      h x = Writer (x*2, ["h"])
      lhs = (m >>= k) >>= h
      rhs = m >>= (\x -> k x >>= h)
  in runWriter lhs == runWriter rhs

checkReaderIdentities :: Bool
checkReaderIdentities =
  let m :: Reader Config String
      m = greet "test"

      r = Config "hi" False

      f :: Config -> Config
      f c = c { shout = not (shout c) }

      k :: Config -> Reader Config String
      k cfg0 = Reader $ \cfg ->
        "len=" ++ show (length (greetPrefix cfg0)) ++ ":" ++ greetPrefix cfg

      lhs1 = runReader (local id m) r
      rhs1 = runReader m r

      lhs2 = runReader (local f (ask >>= k)) r
      rhs2 = runReader (ask >>= (\x -> local f (k (f x)))) r
  in lhs1 == rhs1 && lhs2 == rhs2

--------------------------------------------------------------------------------
-- HC21T17: Refactor Legacy Env/Log/State Code
--------------------------------------------------------------------------------

legacyRefactored :: Reader Env (State Int (Writer [String] Int))
legacyRefactored = do
  env <- ask
  pure $ do
    s0 <- get
    let startMsg = "starting at " ++ show s0
        portMsg  = "using port " ++ M.findWithDefault "5432" "DB_PORT" env
    modify (+1)
    s1 <- get
    pure $ do
      tell [startMsg, portMsg, "counter = " ++ show s1]
      pure s1

--------------------------------------------------------------------------------
-- Main Demo — runs HC21T1..HC21T17 samples
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "HC21T1: calcDemo"
  print $ runWriter calcDemo

  putStrLn "\nHC21T2: Writer law check"
  print checkWriterLaws

  putStrLn "\nHC21T3: redactSecrets"
  print $ runWriter (redactSecrets calcDemo)

  putStrLn "\nHC21T4: Writer with Sum count"
  print $ runWriter calcCount

  putStrLn "\nHC21T5: greetWithFlip"
  print $ runReader (greetWithFlip "Alice") (Config "Hello" False)

  putStrLn "\nHC21T6: composeReaders"
  print $ runReader composeReaders (Config "Hi" True)

  putStrLn "\nHC21T7: getConn with override"
  print $ runReader (getConnWithOverridePort "9999") (M.fromList [("DB_HOST","db.local")])

  putStrLn "\nHC21T8: get/put/modify primitives are defined and used elsewhere."

  putStrLn "\nHC21T9: mapCount"
  print $ runState (mapCount (+1) [1..5]) 0

  putStrLn "\nHC21T10: vendingSequence"
  print $ runState vendingSequence (MkVendingState 3 0)

  putStrLn "\nHC21T11: undoScript"
  print $ runState undoScript (0,[])

  putStrLn "\nHC21T12: randomWalk (5 steps)"
  print $ evalState (randomWalk 5) (42, (0,0))

  putStrLn "\nHC21T13: Reader+Writer configurable logging"
  let ((), logsRW) = runWriter (runReader stepsDemo (Config "app" False))
  mapM_ print logsRW

  putStrLn "\nHC21T14: State+Writer instrumented state"
  let (w, finalS) = runState instrumentedDemo 0
      (lastVal, logs2) = runWriter w
  print finalS
  print lastVal
  mapM_ putStrLn logs2
  putStrLn "Note: layering as State s (Writer w a) => run State, then collect logs."

  putStrLn "\nHC21T15: Reader+State ticks"
  let cfgA = Config "abc" False
      cfgB = Config "abc" True
  print (runReader (runTicks 6) cfgA)
  print (runReader (runTicks 6) cfgB)

  putStrLn "\nHC21T16: law checks (Writer assoc, Reader identities)"
  print checkWriterAssoc
  print checkReaderIdentities

  putStrLn "\nHC21T17: legacy refactor demo"
  let (w3, st1) = runState (runReader legacyRefactored (M.fromList [("DB_PORT","15432")])) 0
      (resultCounter, logs3) = runWriter w3
  print st1
  print resultCounter
  mapM_ putStrLn logs3
