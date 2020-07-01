module Fazzbozz.Core (
  sfazzbozz,
  scanM,
  labelBool,
  labelToMaybe,
  groupChecks,
  labelCheck,
  collectCheckResults,
  combineChecks,
  injectN,
  sfazzbozz',
) where

import Control.Monad (foldM, guard)
import Control.Monad.Trans.State (State)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Tuple (swap)

import Fazzbozz.Base (FazzState(..), mapFst)

type Label = String
type Labeled s = (Label, s)

sfazzbozz :: FazzState s => [Labeled s] -> Integer -> (String, [Labeled s])
sfazzbozz ss n = mapFst collectResults $ unzip $ fazzAll ss
  where
    collectResults = fromMaybe (show n) . mconcat
    fazzAll = map $ fazzOne n

fazzOne :: FazzState s => Integer -> Labeled s -> (Maybe String, Labeled s)
fazzOne n = mapFst labelToMaybe . dupFst . fmap (flip matchFazz $ n)

dupFst :: (a, (b, c)) -> ((a, b), (a, c))
dupFst (a, (b, c)) = ((a, b), (a, c))

labelToMaybe :: Labeled Bool -> Maybe String
labelToMaybe (label, b) = label <$ guard b

labelBool :: String -> Bool -> Maybe String
labelBool s = (s <$) . guard

-- e.g.:
-- fazzCheck :: Integral a => a -> State () (Maybe String)
-- fazzCheck = labelCheck "fazz" $ checkModulo 3
-- evalState (mapM fazzCheck [1..6]) () ==
--   [Nothing,Nothing,Just "fazz",Nothing,Nothing,Just "fazz"]
labelCheck :: String -> (a -> State b Bool) -> a -> State b (Maybe String)
labelCheck = fmap . fmap . labelBool

-- e.g.:
-- checks :: Integral a => a -> State () [Maybe String]
-- checks = groupChecks [
--     labelCheck "fazz" $ checkModulo 3,
--     labelCheck "bozz" $ checkModulo 5
--   ]
-- evalState (mapM checks [1..6]) () ==
--   [[Nothing, Nothing],
--    [Nothing, Nothing],
--    [Just "fazz", Nothing],
--    [Nothing, Nothing],
--    [Nothing, Just "bozz"],
--    [Just "fazz", Nothing]]
groupChecks :: [a -> State s r] -> a -> State s [r]
groupChecks cs n = traverse ($ n) cs

collectCheckResults :: [Maybe String] -> Maybe String
collectCheckResults ms =
  case catMaybes ms of [] -> Nothing
                       ss -> Just $ mconcat ss

combineChecks :: [a -> State s (Maybe String)] -> a -> State s (Maybe String)
combineChecks cs = (fmap . fmap) collectCheckResults (groupChecks cs)

injectN :: Show a => a -> Maybe String -> String
injectN = fromMaybe . show

-- e.g.:
-- checks :: Integral a => a -> State () (Maybe String)
-- checks = combineChecks [
--   labelCheck "fazz" $ checkModulo 3,
--   labelCheck "bozz" $ checkModulo 5
-- ]
-- fazz :: Integral a => a -> State () String
-- fazz :: sfazzbozz' checks
-- evalState (mapM fazz [1..6]) () ==
--   ["1", "2", "fazz", "4", "bozz", "fazz"]
sfazzbozz' :: Show t => (t -> State s (Maybe String)) -> t -> State s String
sfazzbozz' check n = ((fmap . fmap) (injectN n) check) n

scanM :: Foldable t => (a -> b -> (c, a)) -> a -> t b -> [c]
scanM f s ns = fst $ foldM f' s ns
  where f' s n = mapFst pure $ f s n
