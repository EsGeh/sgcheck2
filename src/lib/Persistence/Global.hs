module Persistence.Global(
	FileSys(..), Entry(..),
	configPath, hiddenFileEnding
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

data FileSys
	= FileSys {
		fs_memorizeFile ::
			Settings
			-> Path -- src
			-> Path -- dest
			-> ErrT IO (),
		fs_lookupFile ::
			Path -> ErrT IO Entry,
		fs_list ::
			ErrT IO [Entry]
	}
