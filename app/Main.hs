import Control.Monad.Trans.State (evalState)
import Options.Applicative (execParser)

import Fazzbozz (checkSpecifier, combineChecks, defaultCombinedState, sfazzbozz')
import Fazzbozz.CmdOptions (CmdOptions(..), opts)

main = execParser opts >>= printFazzbozz

printFazzbozz :: CmdOptions Integer -> IO ()
printFazzbozz (CmdOptions n matchSpecs) =
  let fazz = sfazzbozz' $ combineChecks $ map checkSpecifier matchSpecs
  in mapM_ putStrLn $ evalState (mapM fazz [1..n]) defaultCombinedState
