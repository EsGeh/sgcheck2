{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Data(
	Command(..),
	CommandType(..),
	HasCommmandType(..),
	cmdType_fromStr, cmdType_toStr,
	cmdType_listAll,
	CopyCommandParams(..),
	CopyFlags(..), defCopyFlags,
	copyFlags_mapToRSyncOpts, copyFlags_mapToRSyncOptsM,
	ListParams,
		defListParamsMarkChanged,
		defListParamsMarkChangedWithMarker,
		defListParamsRSyncOut,
		--defListParamsMarkChanged, defListParamsMarkChangedWithMarker, defListParamsRSyncOut,
	Output(..),
	SimpleOutputInfo(..),
	--ChangedInfo(..), MarkInfo(..),
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
		copyFlags_printCommand :: Bool,
		copyFlags_addRSyncOpts :: [String]
	}
	deriving( Show )

defCopyFlags =
	CopyFlags {
		copyFlags_simulate = False,
		copyFlags_printCommand = False,
		copyFlags_addRSyncOpts = []
	}

copyFlags_mapToRSyncOpts f =
	runIdentity . copyFlags_mapToRSyncOptsM (return . f)

copyFlags_mapToRSyncOptsM f x =
	do
		new <- f (copyFlags_addRSyncOpts x)
		return $ x{ copyFlags_addRSyncOpts = new }

type ListParams = [Output]

defListParamsMarkChanged = defListParamsMarkChangedWithMarker "*" "!"

defListParamsMarkChangedWithMarker this server =
	[ SimpleOutput $ Path
	{-
	, SimpleOutput $ Str "\t"
	, SimpleOutput $ ServerPath
	-}
	, IfChangedOnThis $ [Left $ Str this]
	, IfChangedOnServer $ [Left $ Str server]
	]

defListParamsRSyncOut =
	[ SimpleOutput $ Path
	, IfChangedOnThis $
		[ Left $ Str "\n\t"
		, Right $ defRSyncOutF
		]
	, IfChangedOnServer $
		[ Left $ Str "\n\t-- these files have changed on the server! --\n\t"
		, Right $ defRSyncOutF
		]
	]

data Output
	= SimpleOutput SimpleOutputInfo
	| IfChangedOnThis [Either SimpleOutputInfo RSyncOutFormat]
	| IfChangedOnServer [Either SimpleOutputInfo RSyncOutFormat]
	deriving( Show )

data SimpleOutputInfo
	= Str String
	| Path
	| ThisPath
	| ServerPath
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
