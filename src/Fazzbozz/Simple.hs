module Fazzbozz.Simple (
  fazzbozz,
  makeState,

  CombinedState(..),
  defaultCombinedState,
  checkCombined,
  checkSpecifier,
) where

import Control.Monad.Trans.State (State(..), get, put)

import Fazzbozz.CmdOptions (MatchPredicateSpecifier(..), MatchSpecifier(..))
import Fazzbozz.Core (labelCheck, sfazzbozz, sfazzbozz')
import Fazzbozz.Matches (
    EnclosedState(..),  FibonacciState(..), HappyState(..), ModuloState(..),
    PredicateState(..), defaultFibonacciState, defaultHappyState, enclose,
    matchFibonacci, matchHappy, isModulo
  )

fazzbozz :: [(String, Integer -> Bool)] -> Integer -> String
fazzbozz preds = fst . sfazzbozz states
  where
    states = map (fmap PredicateState) preds

makeState :: MatchPredicateSpecifier Integer -> EnclosedState
makeState (ModuloPredicate n) = enclose $ ModuloState n
makeState FibonacciPredicate = enclose defaultFibonacciState
makeState HappyPredicate = enclose defaultHappyState

-- cumulative state for match specifiers

data CombinedState = CombinedState {
  fibonacciState :: FibonacciState,
  happyState :: HappyState
}

defaultCombinedState :: CombinedState
defaultCombinedState = CombinedState defaultFibonacciState defaultHappyState

checkCombined :: MatchPredicateSpecifier Integer -> Integer -> State CombinedState Bool
checkCombined (ModuloPredicate m) n = return $ m `isModulo` n
checkCombined FibonacciPredicate n = do
  s @ (CombinedState { fibonacciState=fs }) <- get
  let (result, newFs) = matchFibonacci fs n
  put $ s { fibonacciState = newFs }
  return result
checkCombined HappyPredicate n = do
  s @ (CombinedState { happyState=hs }) <- get
  let (result, newHs) = matchHappy hs n
  put $ s { happyState = newHs }
  return result

-- e.g.:
-- matchSpecs :: [MatchSpecifier Integer]
-- matchSpecs = [
--   ("fazz", MatchPredicate 3),
--   ("happy", HappyPredicate)
-- ]
-- fazz :: Integer -> State CombinedState String
-- fazz = sfazzbozz' $ combineChecks $ map checkSpecifier matchSpecs
-- evalState (mapM fazz [1..10]) defaultCombinedState ==
--   ["happy", "2", "3", "4", "bozz", "6", "happy", "8", "9", "bozzhappy"]
checkSpecifier :: MatchSpecifier Integer -> Integer -> State CombinedState (Maybe String)
checkSpecifier (label, predicateSpec) = labelCheck label $ checkCombined $ predicateSpec
