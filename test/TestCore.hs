module TestCore (fazzbozzCoreTests) where

import Test.HUnit ((~:), (~=?))
import Fazzbozz.Core (scanM)

scanMTests = [
    "collect" ~: scanM collect [] [1,2,3] ~=? [[1], [1,2], [1,2,3]],
    "product of last two" ~: scanM productTwo 0 [1,2,3,4] ~=? [0,2,6,12],
    "check word prefixes" ~: scanM checkWord "" "android" ~=?
      [True,True,True,False,False,False,True]
  ]
  where
    collect st v = (st', st')
      where st' = st ++ [v]
    productTwo last v = (last * v, v)
    checkWord w c = (newWord `elem` ["a", "an", "and", "android"], newWord)
      where
        newWord = w ++ [c]

fazzbozzCoreTests = [
    "scanM" ~: scanMTests
  ]
