{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module UserInput.Types where

import Data
import Global


data UserInput
	= UserInput {
		ui_cmd :: Command,
		ui_configDir :: Maybe Path
	}
	deriving( Show )

-- options

{-
data Options
	= CopyOptions (CopyOptionsInfo CopyFlags)
	| ListFileOptions GeneralOptions
	| ShowConfigOptions GeneralOptions
	| WriteConfgOptions GeneralOptions
	deriving( Show )

data CopyOptions
	= CopyOptions {
		opts_copyFlags :: CopyFlags,
		opts_general :: GeneralOptions
	}
	deriving( Show )

defCopyOptions = CopyOptions defCopyFlags defGeneralOptions

copyOpts_mapToCopyFlags f = runIdentity . copyOpts_mapToCopyFlagsM (return . f)
copyOpts_mapToCopyFlagsM f x =
	do
		new <- f $ opts_copyFlags x
		return $ x{ opts_copyFlags = new }

copyOpts_mapToGeneral f = runIdentity . copyOpts_mapToGeneralM (return . f)
copyOpts_mapToGeneralM f x =
	do
		new <- f $ opts_general x
		return $ x{ opts_general = new }
-}

data GeneralOptions =
	GeneralOptions {
		genOpts_configDir :: Maybe Path
	}
	deriving( Show )

defGeneralOptions = GeneralOptions $ Nothing
