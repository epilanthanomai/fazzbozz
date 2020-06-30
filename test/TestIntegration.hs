module TestIntegration (integrationTests) where

import Test.HUnit ((~:), (~=?))
import Options.Applicative (defaultPrefs, execParserPure, getParseResult)

import Fazzbozz.CmdOptions (CmdOptions(..), opts)
import Fazzbozz.Core (scanM, sfazzbozz)
import Fazzbozz.Simple (makeState)

parseCmdLine = getParseResult . execParserPure defaultPrefs opts
fazzbozzForOptions (CmdOptions n matchSpecs) = scanM sfazzbozz states [1..n]
  where
    states = map (fmap makeState) matchSpecs

fazzbozzForArgs args = fazzbozzForOptions <$> parseCmdLine args

integrationTests = [
    "default args" ~: fazzbozzForArgs [] ~=?
      Just ["1", "2", "fazz", "4", "bozz", "fazz", "7", "8", "fazz", "bozz",
            "11", "fazz", "13", "14", "fazzbozz", "16", "17", "fazz", "19", "bozz"],
    "specify count" ~: fazzbozzForArgs ["-n", "16"] ~=?
      Just ["1", "2", "fazz", "4", "bozz", "fazz", "7", "8",
            "fazz", "bozz", "11", "fazz", "13", "14", "fazzbozz", "16"],
    "numeric pattern" ~: fazzbozzForArgs ["-p", "7:foo"] ~=?
      Just ["1", "2", "3", "4", "5", "6", "foo", "8", "9", "10",
            "11", "12", "13", "foo", "15", "16", "17", "18", "19", "20"],
    "fibonacci pattern" ~: fazzbozzForArgs ["-p", "fib:foo"] ~=?
      Just ["foo", "foo", "foo", "4", "foo", "6", "7", "foo", "9", "10",
            "11", "12", "foo", "14", "15", "16", "17", "18", "19", "20"],
    "happy pattern" ~: fazzbozzForArgs ["-p", "happy:h"] ~=?
      Just ["h", "2", "3", "4", "5", "6", "h", "8", "9", "h", "11", "12",
            "h", "14", "15", "16", "17", "18", "h", "20"],
    "multiple patterns" ~: fazzbozzForArgs ["-n", "10", "-p", "3:foo", "-p", "2:bar"] ~=?
      Just ["1", "bar", "foo", "bar", "5", "foobar", "7", "bar", "foo", "bar"],

    "invalid arg: bad option" ~: fazzbozzForArgs ["-x"] ~=? Nothing,
    "invalid arg: bad count" ~: fazzbozzForArgs ["-n", "twenty"] ~=? Nothing,
    "invalid arg: bad pattern" ~: fazzbozzForArgs ["-p", "twelve:foo"] ~=? Nothing
  ]
