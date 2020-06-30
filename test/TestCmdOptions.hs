module TestCmdOptions (optionsTests) where

import Test.HUnit (Test(..), (~:), (~=?))
import Options.Applicative (defaultPrefs, execParserPure, getParseResult)
import Fazzbozz.CmdOptions (CmdOptions(..), MatchPredicateSpecifier(..), opts)

parseCmdLine :: (Integral n, Read n, Show n) => [String] -> Maybe (CmdOptions n)
parseCmdLine = getParseResult <$> execParserPure defaultPrefs opts

runTest :: (Eq t, Show t, Integral n, Read n, Show n) => (CmdOptions n -> t) -> String -> [String] -> Maybe t -> Test
runTest field name cmdLine expect = name ~: field <$> parseCmdLine cmdLine ~=? expect

numberTest = [
    test "default number" [] $ Just 20,
    test "specify number" ["-n", "100"] $ Just 100,

    test "invalid number" ["-n", "one hundred"] Nothing,
    test "missing number" ["-n"] Nothing
  ]
  where test = runTest number

patternsTest = [
    test "default patterns" [] $ Just [("fazz", ModuloPredicate 3), ("bozz", ModuloPredicate 5)],

    test "modulo pattern" ["-p", "2:test"] $ Just [("test", ModuloPredicate 2)],
    test "modulo missing label" ["-p", "2"] Nothing,
    test "modulo extra args" ["-p", "2:a:b"] Nothing,

    test "fibonacci pattern" ["-p", "fib:test"] $ Just [("test", FibonacciPredicate)],
    test "fibonacci missing label" ["-p", "fib"] Nothing,
    test "fibonacci extra args" ["-p", "fib:a:b"] Nothing,

    test "happy pattern" ["-p", "happy:test"] $ Just [("test", HappyPredicate)],
    test "happy missing label" ["-p", "happy"] Nothing,
    test "happy extra args" ["-p", "happy:a:b"] Nothing,

    test "multiple patterns" ["-p", "2:a", "-p", "fib:b"] $
      Just [("a", ModuloPredicate 2), ("b", FibonacciPredicate)],
    test "invalid pattern type" ["-p", "x:test"] Nothing
  ]
  where test = runTest matchSpecs

optionsTests = [
    "number" ~: numberTest,
    "patterns" ~: patternsTest
  ]
