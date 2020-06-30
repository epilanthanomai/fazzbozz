module Fazzbozz.CmdOptions (
    CmdOptions(..),
    MatchSpecifier,
    MatchPredicateSpecifier(..),
    opts
  ) where

import Control.Monad (guard)
import Data.List.Split (splitOn)
import Data.Semigroup ((<>))
import Options.Applicative (
    Parser(..), ParserInfo(..),
    (<**>),
    auto, header, help, helper, eitherReader, fullDesc, info, long, many,
    metavar, option, progDesc, short, showDefault, value
  )
import Text.Read (readMaybe)

type PatternParser n = [String] -> [MatchSpecifier n]
type MatchSpecifier n = (String, MatchPredicateSpecifier n)

data MatchPredicateSpecifier n =
  ModuloPredicate n |
  FibonacciPredicate |
  HappyPredicate
  deriving (Show, Eq)

data CmdOptions n = CmdOptions {
    number :: n,
    matchSpecs :: [MatchSpecifier n]
  }

cmdOptions :: (Integral n, Read n, Show n) => Parser (CmdOptions n)
cmdOptions = CmdOptions
    <$> option auto (
          long "number" <>
          short 'n' <>
          help "number of values to output" <>
          showDefault <>
          value 20 )
    <*> (fmap fillDefaultMatchSpecs $ many $ option readOption (
          long "pattern" <>
          short 'p' <>
          help "show a label on some pattern" <>
          metavar "PAT:LABEL" ))
  where readOption = eitherReader parseOption

-- This is almost certainly done for us in a library somewhere
fillDefaultMatchSpecs :: Integral n => [MatchSpecifier n] -> [MatchSpecifier n]
fillDefaultMatchSpecs [] = defaultMatchSpecs
fillDefaultMatchSpecs p = p

defaultMatchSpecs :: Integral n => [MatchSpecifier n]
defaultMatchSpecs = [
    ("fazz", ModuloPredicate 3),
    ("bozz", ModuloPredicate 5)
  ]

parseOption :: (Integral n, Read n) => String -> Either String (MatchSpecifier n)
parseOption opt =
  case matches of
    match : _ -> Right match
    [] -> Left "Pattern must be PAT:LABEL"
  where
    patternArguments = splitOn ":" opt
    matches = mconcat $ patternParsers <*> [patternArguments]

opts :: (Integral n, Read n, Show n) => ParserInfo (CmdOptions n)
opts = info (cmdOptions <**> helper) (
          fullDesc <>
          progDesc "Print fizzbuzz numbers" <>
          header "fazzbozz - an overengineered fizzbuzz" )

-- parser utils

parseSimplePattern :: (String -> Maybe t) -> (t -> MatchPredicateSpecifier n) -> PatternParser n
parseSimplePattern parseName makeMatchPred args = do
  [rawName, label] <- return args
  Just val <- return $ parseName rawName
  return $ (label, makeMatchPred val)

parseNamedPattern :: String -> MatchPredicateSpecifier n -> PatternParser n
parseNamedPattern name matchPred = parseSimplePattern parseName $ const matchPred
  where parseName = guard <$> (== name)

-- parsers

patternParsers :: (Read n, Integral n) => [PatternParser n]
patternParsers = [parseCountPattern, parseFibonacciPattern, parseHappyPattern]

parseCountPattern :: (Read n, Integral n) => PatternParser n
parseCountPattern = parseSimplePattern readMaybe ModuloPredicate

parseFibonacciPattern :: PatternParser n
parseFibonacciPattern = parseNamedPattern "fib" FibonacciPredicate

parseHappyPattern :: PatternParser n
parseHappyPattern = parseNamedPattern "happy" HappyPredicate
