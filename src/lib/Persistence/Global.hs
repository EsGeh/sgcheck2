module Persistence.Global(
	Entry(..),
	configPath,
	hiddenFileEnding,
	logDir,
	entry_pathOnThis,
) where

import Data
import Utils
--import System.FilePath as Path( (</>), (<.>) )
import qualified System.FilePath as Path
--import Utils.Path

type Path = Path.FilePath


configPath, hiddenFileEnding :: String

configPath = "config"
hiddenFileEnding = "sgcheck2"
logDir = "logs"
