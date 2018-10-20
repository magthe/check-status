module BuildEnv
  ( getBuildEnv
  ) where

import Data.Maybe (fromMaybe)
import Language.Haskell.TH (ExpQ, runIO, stringE)
import System.Environment (lookupEnv)

getBuildEnv :: String -> String -> ExpQ
getBuildEnv def varname = runIO (fromMaybe def <$> lookupEnv varname) >>= stringE
