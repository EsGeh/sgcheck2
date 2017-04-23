module Persistence.Global(
	configPath,
	hiddenFileEnding,
	logDir,
) where

--import System.FilePath as Path( (</>), (<.>) )
import qualified System.FilePath as Path

type Path = Path.FilePath


configPath, hiddenFileEnding, logDir :: Path

configPath = "config"
hiddenFileEnding = "sgcheck2"
logDir = "logs"
