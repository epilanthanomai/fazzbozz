module TestSimple (fazzbozzSimpleTests) where

import Test.HUnit ((~:), (~=?))
import Fazzbozz.Simple (fazzbozz)

fazzbozzFunctionTests = [
    "no matches" ~: fazzbozz [] 1 ~=? "1",
    "trivial negative match" ~: fazzbozz [neverMatch] 1 ~=? "1",
    "trivial positive match" ~: fazzbozz [alwaysFoo] 1 ~=? "foo",
    "multiple trivial matches" ~: fazzbozz [neverMatch, alwaysFoo] 1 ~=? "foo",

    "nontrivial negative match" ~: fazzbozz [evenBar] 1 ~=? "1",
    "nontrivial positive match" ~: fazzbozz [evenBar] 2 ~=? "bar",

    "multiple positive matches" ~: fazzbozz [alwaysFoo, evenBar] 2 ~=? "foobar"
  ]
  where
    neverMatch = ("x", const False)
    alwaysFoo = ("foo", const True)
    evenBar = ("bar", even)

fazzbozzSimpleTests = [
    "fazzbozz" ~: fazzbozzFunctionTests
  ]
