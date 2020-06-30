module TestMatches (fazzbozzMatchTests) where

import qualified Data.Map as Map
import Test.HUnit ((~:), (~=?))

import Fazzbozz.Base (matchFazz)
import Fazzbozz.Matches (
    FibonacciState(..), HappyState(..), ModuloState(..),
    defaultFibonacciState, isFibonacci, defaultHappyState, isHappy, isModulo
  )

moduloTests = [
    "positive" ~: isModulo 3 6 ~=? True,
    "negative" ~: isModulo 3 5 ~=? False
  ]

moduloStateTests = [
    "positive" ~: ModuloState 3 `matchFazz` 6 ~=? (True, ModuloState 3),
    "negative" ~: ModuloState 3 `matchFazz` 5 ~=? (False, ModuloState 3)
  ]

fibonacciTests = [
    "positive" ~: isFibonacci 21 ~=? True,
    "negative" ~: isFibonacci 20 ~=? False
  ]

fibonacciStateTests = [
    "default" ~: let (FibonacciState f) = defaultFibonacciState in take 5 f ~=? [0, 1, 1, 2, 3],
    "positive head" ~: FibonacciState [5, 8, 13, 21] `matchFazz` 5 ~=? (True, FibonacciState [5, 8, 13, 21]),
    "positive body" ~: FibonacciState [5, 8, 13, 21] `matchFazz` 8 ~=? (True, FibonacciState [8, 13, 21]),
    "negative head" ~: FibonacciState [5, 8, 13, 21] `matchFazz` 4 ~=? (False, FibonacciState [5, 8, 13, 21]),
    "negative body" ~: FibonacciState [5, 8, 13, 21] `matchFazz` 10 ~=? (False, FibonacciState [13, 21])
  ]

happyTests = [
    "default positive" ~: isHappy 1 ~=? True,
    "default negative" ~: isHappy 4 ~=? False,
    "accumulated positive" ~: isHappy 7 ~=? True,
    "accumulated negative" ~: isHappy 8 ~=? False
  ]

happyStateTests = [
    "default" ~: defaultHappyState ~=? HappyState (Map.fromList [(1, True), (4, False)]),
    "positive present" ~: defaultHappyState `matchFazz` 1 ~=? (True, HappyState (Map.fromList [(1, True), (4, False)])),
    "positive recurse" ~: defaultHappyState `matchFazz` 10 ~=? (True, HappyState (Map.fromList [(1, True), (4, False), (10, True)])),
    "positive multirecurse" ~: defaultHappyState `matchFazz` 31 ~=? (True, HappyState (Map.fromList [(1, True), (4, False), (10, True), (31, True)])),
    "negative present" ~: defaultHappyState `matchFazz` 4 ~=? (False, HappyState (Map.fromList [(1, True), (4, False)])),
    "negative recurse" ~: defaultHappyState `matchFazz` 20 ~=? (False, HappyState (Map.fromList [(1, True), (4, False), (20, False)])),
    "negative multirecurse" ~: defaultHappyState `matchFazz` 42 ~=? (False, HappyState (Map.fromList [(1, True), (4, False), (20, False), (42, False)]))
  ]

fazzbozzMatchTests = [
    "modulo" ~: moduloTests,
    "ModuloState" ~: moduloStateTests,
    "fibonacci" ~: fibonacciTests,
    "FibonacciState" ~: fibonacciStateTests,
    "happy" ~: happyTests,
    "HappyState" ~: happyStateTests
  ]
