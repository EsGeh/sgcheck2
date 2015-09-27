module Data.Entry where

import Global


data Entry
	= Entry {
		entry_path :: Path
		--entry_syncInfo :: SyncInfo
	}
	deriving( Show )

data SyncInfo
	= NotChanged
	| NewerOnThis
	| NewerOnServer
	deriving( Show )

entry_toText :: Entry -> String
entry_toText = path_toStr . entry_path
