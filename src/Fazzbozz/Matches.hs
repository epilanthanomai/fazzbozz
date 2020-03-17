module Fazzbozz.Matches (
  PredicateState(..),

  ModuloState(..),
  isModulo,

  FibonacciState(..),
  isFibonacci,
  checkFibonacci,
  fibs,
  defaultFibonacciEnv,
  defaultFibonacciState,

  HappyState(..),
  isHappy,
  checkHappy,
  defaultHappyEnv,
  defaultHappyState,

  EnclosedState(..),
  enclose,
) where

import Control.Monad.Trans.State
import Data.List
import Data.Tuple

import qualified Data.Map as Map

import Fazzbozz.Base

-- helpers

constState :: (a -> b -> c) -> a -> b -> (c, a)
constState f s x = (f s x, s)

-- predicate

newtype PredicateState = PredicateState (Integer -> Bool)

instance FazzState PredicateState where
  matchFazz = constState matchPredicate
    where matchPredicate (PredicateState f) = f

-- modulo

newtype ModuloState = ModuloState Integer deriving (Eq, Show)

divisibleBy :: Integral a => a -> a -> Bool
a `divisibleBy` b = (a `mod` b) == 0

isModulo :: Integral a => a -> a -> Bool
isModulo = flip divisibleBy

instance FazzState ModuloState where
  matchFazz = constState matchModulo
    where matchModulo (ModuloState n) = (`divisibleBy` n)

-- fibonacci

type FibonacciEnv = [Integer]

isFibonacci :: (Ord a, Num a) => a -> Bool
isFibonacci n = evalState (checkFibonacci n) defaultFibonacciEnv

fibs :: Num n => [n]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

defaultFibonacciEnv :: Num n => [n]
defaultFibonacciEnv = fibs

checkFibonacci :: Ord a => a -> State [a] Bool
checkFibonacci n = do
  ns <- get
  let ns'@(n' : _) = dropWhile (<n) ns
  let match = (n' == n)
  put ns'
  return match

-- outgoing "State" wrappers for FibonacciEnv

newtype FibonacciState = FibonacciState FibonacciEnv deriving (Eq, Show)

defaultFibonacciState :: FibonacciState
defaultFibonacciState = FibonacciState defaultFibonacciEnv

matchFibonacci :: FibonacciState -> Integer -> (Bool, FibonacciState)
matchFibonacci (FibonacciState s) n = FibonacciState <$> runState (checkFibonacci n) s

instance FazzState FibonacciState where
  matchFazz = matchFibonacci

-- happy

type HappyEnv = Map.Map Integer Bool

isHappy :: Integer -> Bool
isHappy n = evalState (checkHappy n) defaultHappyEnv

nextHappy :: Integral a => a -> a
nextHappy = sum . map square . digits 10
  where square n = n * n

digits :: Integral a => a -> a -> [a]
digits base = unfoldr $ digits' base
  where
    digits' _ 0 = Nothing
    digits' base n = Just . swap $ n `divMod` base

defaultHappyEnv :: HappyEnv
defaultHappyEnv = Map.fromList [(1, True), (4, False)]

checkHappy :: Integer -> State HappyEnv Bool
checkHappy n = do
  oldState <- get
  let maybeMatch = Map.lookup n oldState
  case maybeMatch of
    Just result -> return result
    Nothing -> do
      let next = nextHappy n
      let (result, resultState) = runState (checkHappy next) oldState
      put $ Map.insert n result resultState
      return result

-- outgoing "State" wrappers for HappyEnv

newtype HappyState = HappyState HappyEnv deriving (Eq, Show)

defaultHappyState :: HappyState
defaultHappyState = HappyState defaultHappyEnv

matchHappy :: HappyState -> Integer -> (Bool, HappyState)
matchHappy (HappyState s) n = HappyState <$> runState (checkHappy n) s

instance FazzState HappyState where
  matchFazz = matchHappy

-- enclosed

newtype EnclosedState = EnclosedState (Integer -> (Bool, EnclosedState))

enclose :: FazzState s => s -> EnclosedState
enclose s = EnclosedState $ \n -> fmap enclose $ matchFazz s n

matchEnclosed :: EnclosedState -> Integer -> (Bool, EnclosedState)
matchEnclosed (EnclosedState f) n = f n

instance FazzState EnclosedState where
  matchFazz = matchEnclosed
