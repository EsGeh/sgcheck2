module Data.Entry(
	Entry(..),
	entry_toText,
) where

--import Utils
--import System.FilePath as Path( (</>), (<.>) )
import qualified System.FilePath as Path

type Path = Path.FilePath


data Entry
	= Entry {
		entry_path :: Path
		--entry_syncInfo :: SyncInfo
	}
	deriving( Show, Read )

{-
data SyncInfo
	= NotChanged
	| NewerOnThis
	| NewerOnServer
	deriving( Show )
-}

entry_toText :: Entry -> String
entry_toText = entry_path
