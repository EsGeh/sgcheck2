{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Data(
	Command(..),
	CommandType(..),
	HasCommmandType(..),
	cmdType_fromStr, cmdType_toStr,
	cmdType_listAll,
	CopyCommandParams(..),
	CopyFlags(..), defCopyFlags,
	ListParams(..),
		defListParamsMarkChanged, defListParamsMarkChangedWithMarker, defListParamsRSyncOut,
	Output(..),
	ChangedInfo(..), MarkInfo(..),
	RSyncOutFormat(..),
	--SimpleListDescr(..), defSimpleListDescr,
	module Data.Entry,
	module Data.Settings,

	MemorizeFile, LookupFile, ListEntries,
) where


import Data.Entry
import Data.Settings
import Global

import Control.Monad.Identity
import Data.Tuple( swap )


data Command
	= CmdOut CopyCommandParams
	| CmdIn CopyCommandParams
	| CmdListFiles ListParams
	| CmdShowConfig
	| CmdWriteConfig
	deriving( Show )

data CommandType
	= Out
	| In
	| ListFiles
	| ShowConfig
	| WriteConfig
	deriving( Eq, Ord, Enum, Bounded, Show )

cmdType_listAll :: [CommandType]
cmdType_listAll = [(minBound :: CommandType) .. ]

class HasCommmandType a where
	cmd_type :: a -> CommandType

instance HasCommmandType Command where
	cmd_type cmd =
		case cmd of
			CmdOut _ -> Out
			CmdIn _ -> In
			CmdListFiles _ -> ListFiles
			CmdShowConfig -> ShowConfig
			CmdWriteConfig -> WriteConfig

cmdType_fromStr :: String -> Maybe CommandType
cmdType_fromStr str =
	lookup str cmdList

cmdType_toStr :: CommandType -> String
cmdType_toStr c =
	maybe (error "cmdType_fromStr error!") id $
	lookup c $ map swap cmdList

data CopyCommandParams
	= CopyCommandParams {
		copyCmd_file :: Path,
		copyCmd_flags :: CopyFlags
	}
	deriving( Show )

data CopyFlags
	= CopyFlags {
		copyFlags_simulate :: Bool,
		copyFlags_printCommand :: Bool
	}
	deriving( Show )

defCopyFlags =
	CopyFlags {
		copyFlags_simulate = False,
		copyFlags_printCommand = False
	}


data ListParams
	= ListParams {
		listParams_entry :: EntryDescr,
		listParams_subEntry :: Maybe EntryDescr
	}
	deriving( Show )

listParams_mapToEntry f = runIdentity . listParams_mapToEntryM (return . f)
listParams_mapToEntryM f x = do
	new <- f $ listParams_entry x
	return $ x{ listParams_entry = new }

type EntryDescr = [Output]

defListParamsMarkChanged = defListParamsMarkChangedWithMarker $ MarkInfo "*" "!"

defListParamsMarkChangedWithMarker marker =
	ListParams {
		listParams_entry = [Path, Changed $ Mark marker],
		listParams_subEntry = Nothing
	}

defListParamsRSyncOut =
	ListParams {
		listParams_entry = [Path, Changed $ RSyncOut defRSyncOutF],
		listParams_subEntry = Nothing
	}

data Output
	= Str String
	| Path
	| ThisPath
	| ServerPath
	| Changed ChangedInfo
	deriving( Show )

data ChangedInfo
	= Mark MarkInfo
	| RSyncOut RSyncOutFormat
	deriving( Show )

data MarkInfo
	= MarkInfo {
		markInfo_this :: String,
		markInfo_server :: String
	}
	deriving( Show )

data RSyncOutFormat
	= RSyncOutFormat {
		rsyncF_interperseLines :: String
	}
	deriving( Show )

defRSyncOutF = RSyncOutFormat "\n\t"

type MemorizeFile =
	Settings
	-> Path -- src
	-> Path -- dest
	-> ErrT IO ()

type LookupFile =
	Path -> ErrT IO Entry

type ListEntries =
	ErrT IO [Entry]

cmdList :: [(String, CommandType)]
cmdList =
	[ ("out", Out)
	, ("in", In)
	, ("list", ListFiles)
	, ("showConfig", ShowConfig)
	, ("writeConfig", WriteConfig)
	]
