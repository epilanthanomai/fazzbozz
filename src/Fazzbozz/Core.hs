module Fazzbozz.Core (
  sfazzbozz,
  scanM,
  labelBool,
  labelToMaybe,
) where

import Control.Monad (foldM, guard)
import Control.Monad.Trans.State (State)
import Data.Maybe (fromMaybe)
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

scanM :: Foldable t => (a -> b -> (c, a)) -> a -> t b -> [c]
scanM f s ns = fst $ foldM f' s ns
  where f' s n = mapFst pure $ f s n
