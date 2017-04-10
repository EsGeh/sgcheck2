module Data.Entry(
	Entry(..),
	entry_toText,
) where

--import Utils
import qualified Utils.Path as Path


type Path = Path.Path

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
entry_toText = Path.path_toStr . entry_path
