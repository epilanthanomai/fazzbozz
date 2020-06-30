import System.Exit (exitFailure, exitSuccess)
import Test.HUnit ((~:), failures, errors, runTestTT, test)

import TestCmdOptions (optionsTests)
import TestCore (fazzbozzCoreTests)
import TestIntegration (integrationTests)
import TestMatches (fazzbozzMatchTests)
import TestSimple (fazzbozzSimpleTests)

suite = test [
    "options" ~: optionsTests,
    "fazzbozz core" ~: fazzbozzCoreTests,
    "fazzbozz matches" ~: fazzbozzMatchTests,
    "fazzbozz simple" ~: fazzbozzSimpleTests,
    "integration" ~: integrationTests
  ]

main = do
  results <- runTestTT suite
  if errors results + failures results > 0
    then exitFailure
    else exitSuccess
