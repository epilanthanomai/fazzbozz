module Fazzbozz (
  fazzbozz,
  sfazzbozz,
  sfazzbozz',

  checkSpecifier,
  combineChecks,

  FazzState(..),
  scanM,
  makeState,

  ModuloState(..),
  isModulo,

  FibonacciState(..),
  isFibonacci,
  fibs,
  defaultFibonacciState,

  HappyState(..),
  isHappy,
  defaultHappyState,

  EnclosedState(..),
  enclose,

  CombinedState(..),
  checkCombined,
  defaultCombinedState,
) where

import Fazzbozz.Base (FazzState(..))
import Fazzbozz.Core (combineChecks, sfazzbozz, sfazzbozz', scanM)
import Fazzbozz.Matches (
    EnclosedState(..), FibonacciState(..), HappyState(..), ModuloState(..),
    enclose, fibs, defaultFibonacciState, defaultHappyState, isFibonacci,
    isHappy, isModulo
  )
import Fazzbozz.Simple (
    CombinedState(..), checkCombined, checkSpecifier, defaultCombinedState,
    fazzbozz, makeState
  )
