{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module UserInput.Types where

import Programs.InOut.Params
import Data.Tuple( swap )
--
--import System.FilePath as Path( (</>), (<.>) )
import qualified System.FilePath as Path

type Path = Path.FilePath


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
	| CmdAdd Path
	| CmdShowConfig
	| CmdWriteConfig
	deriving( Show )

data CommandType
	= Out
	| In
	| ListFiles
	| Add
	| ShowConfig
	| WriteConfig
	deriving( Eq, Ord, Enum, Bounded, Show )

class HasCommmandType a where
	cmd_type :: a -> CommandType

instance HasCommmandType Command where
	cmd_type cmd =
		case cmd of
			CmdOut _ -> Out
			CmdIn _ -> In
			CmdAdd _ -> Add
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
