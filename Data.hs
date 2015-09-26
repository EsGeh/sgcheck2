module Data(
	module Data.Entry,
	module Data.Settings,

	MemorizeFile, LookupFile,
) where


import Data.Entry
import Data.Settings

import Global


type MemorizeFile =
	Settings
	-> Path -- src
	-> Path -- dest
	-> ErrT IO ()

type LookupFile =
	Path -> ErrT IO Entry
