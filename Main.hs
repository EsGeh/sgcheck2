module Main where

import UserInput
import Data
import qualified Persistence
import qualified Programs.InOut as Programs
import Programs.Settings
import Global

import Control.Monad.Trans.Maybe

--import Programs.Settings
{-
import Data
import Programs.InOut
import Programs.Settings

import Global
import Args
-}

--import Text.Read(readEither)
import System.Exit
import System.Environment


main :: IO ()
main = do
	eitherErr <- runExceptT execProgram
	case eitherErr of
		Left err ->
			do
				putStrLn $ err
				exitFailure
		Right _ -> return ()

execProgram :: ErrT IO ()
execProgram = do
	ui <- userInputFromCmdArgs =<< lift getArgs
	let
		cmd = ui_cmd ui
	configDir <- Persistence.calcConfigDir $ ui_configDir ui
	if cmd_type cmd == WriteConfig
		then Persistence.createConfig configDir
		else
			Persistence.withSettings configDir $ \settings ->
				Persistence.withFileSys configDir $ \fileSys ->
					runMaybeT $
					case cmd of
						CmdOut file ->
							Programs.checkOut file settings (Persistence.fs_memorizeFile fileSys)
						CmdIn file ->
							Programs.checkIn file settings (Persistence.fs_lookupFile fileSys)
						CmdListFiles listArgs ->
							Programs.list settings listArgs
								(Persistence.fs_list fileSys)
						CmdShowConfig ->
							showSettings settings
						CmdWriteConfig ->
							return $ settings
