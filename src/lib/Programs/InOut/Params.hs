{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Programs.InOut.Params(
	CopyCommandParams(..),
	CopyFlags(..), defCopyFlags,
	copyFlags_mapToRSyncOpts, copyFlags_mapToRSyncOptsM,
	ListParams,
		defListParamsMarkChanged,
		defListParamsMarkChangedWithMarker,
		defListParamsRSyncOut,
	Output(..),
	SimpleOutputInfo(..),
	RSyncOutFormat(..),
) where

import qualified System.FilePath as Path

import Control.Monad.Identity

type Path = Path.FilePath


data CopyCommandParams
	= CopyCommandParams {
		copyCmd_file :: Path,
		copyCmd_flags :: CopyFlags
	}
	deriving( Show, Eq, Ord )

data CopyFlags
	= CopyFlags {
		copyFlags_simulate :: Bool,
		copyFlags_printCommand :: Bool,
		copyFlags_printRSyncOut :: Bool,
		copyFlags_addRSyncOpts :: [String]
	}
	deriving( Show, Eq, Ord )

type ListParams = [Output]

data Output
	= SimpleOutput SimpleOutputInfo
	| IfChangedOnThis [Either SimpleOutputInfo RSyncOutFormat]
	| IfChangedOnServer [Either SimpleOutputInfo RSyncOutFormat]
	deriving( Show, Eq, Ord )

data SimpleOutputInfo
	= Str String -- constant string (seperator)
	| Path -- pathOnThis
	| ThisPath -- thisPath </> pathOnThis
	| ServerPath -- serverPath </> pathOnServer
	deriving( Show, Eq, Ord )

data RSyncOutFormat
	= RSyncOutFormat {
		rsyncF_interperseLines :: String
	}
	deriving( Show, Eq, Ord )

defCopyFlags =
	CopyFlags {
		copyFlags_simulate = False,
		copyFlags_printCommand = False,
		copyFlags_printRSyncOut = True,
		copyFlags_addRSyncOpts = []
	}

copyFlags_mapToRSyncOpts f =
	runIdentity . copyFlags_mapToRSyncOptsM (return . f)

copyFlags_mapToRSyncOptsM f x =
	do
		new <- f (copyFlags_addRSyncOpts x)
		return $ x{ copyFlags_addRSyncOpts = new }

defListParamsMarkChanged = defListParamsMarkChangedWithMarker "*" "!"

defListParamsMarkChangedWithMarker this server =
	[ SimpleOutput $ Path
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

defRSyncOutF = RSyncOutFormat "\n\t"
