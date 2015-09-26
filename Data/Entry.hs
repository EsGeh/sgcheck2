module Data.Entry where

import Global


data Entry
	= Entry {
		entry_path :: Path
	}
	deriving( Show )

entry_toText :: Entry -> String
entry_toText = path_toStr . entry_path
