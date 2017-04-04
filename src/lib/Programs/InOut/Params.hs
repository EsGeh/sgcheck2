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

import Utils

import Control.Monad.Identity


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

type ListParams = [Output]

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

defRSyncOutF = RSyncOutFormat "\n\t"
