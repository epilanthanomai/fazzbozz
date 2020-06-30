import Options.Applicative (execParser)

import Fazzbozz (makeState, scanM, sfazzbozz)
import Fazzbozz.CmdOptions (CmdOptions(..), opts)

main = execParser opts >>= printFazzbozz

printFazzbozz :: CmdOptions Integer -> IO ()
printFazzbozz (CmdOptions n matchSpecs) =
  mapM_ putStrLn $ scanM sfazzbozz states [1..n]
    where
      states = map (fmap makeState) matchSpecs
