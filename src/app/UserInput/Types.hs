{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module UserInput.Types where

import Programs.InOut.Params
import Utils
import Data.Tuple( swap )


data UserInput
	= UserInput {
		ui_cmd :: Command,
		ui_configDir :: Maybe Path
	}
	deriving( Show )

data GeneralOptions =
	GeneralOptions {
		genOpts_configDir :: Maybe Path
	}
	deriving( Show )

defGeneralOptions = GeneralOptions $ Nothing

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

cmdList :: [(String, CommandType)]
cmdList =
	[ ("out", Out)
	, ("in", In)
	, ("list", ListFiles)
	, ("showConfig", ShowConfig)
	, ("writeConfig", WriteConfig)
	]
