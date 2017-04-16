module Persistence.Global(
	configPath,
	hiddenFileEnding,
	logDir,
) where

import Utils
--import System.FilePath as Path( (</>), (<.>) )
import qualified System.FilePath as Path
--import Utils.Path

type Path = Path.FilePath


configPath, hiddenFileEnding :: String

configPath = "config"
hiddenFileEnding = "sgcheck2"
logDir = "logs"
