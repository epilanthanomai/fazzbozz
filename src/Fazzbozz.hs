module Fazzbozz (
  fazzbozz,
  sfazzbozz,

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
) where

import Fazzbozz.Base (FazzState(..))
import Fazzbozz.Core (sfazzbozz, scanM)
import Fazzbozz.Matches (
    EnclosedState(..), FibonacciState(..), HappyState(..), ModuloState(..),
    enclose, fibs, defaultFibonacciState, defaultHappyState, isFibonacci,
    isHappy, isModulo
  )
import Fazzbozz.Simple (fazzbozz, makeState)
