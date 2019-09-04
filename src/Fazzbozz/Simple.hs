module Fazzbozz.Simple (
  fazzbozz,
  makeState,
) where

import Fazzbozz.CmdOptions
import Fazzbozz.Core
import Fazzbozz.Matches

fazzbozz :: [(String, Integer -> Bool)] -> Integer -> String
fazzbozz preds = fst . sfazzbozz states
  where
    states = map (fmap PredicateState) preds

makeState :: MatchPredicateSpecifier Integer -> EnclosedState
makeState (ModuloPredicate n) = enclose $ ModuloState n
makeState FibonacciPredicate = enclose defaultFibonacciState
makeState HappyPredicate = enclose defaultHappyState
