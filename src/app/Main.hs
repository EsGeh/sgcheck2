module Main where

import UserInput
import UserInput.Types
import qualified Persistence
import qualified Programs.InOut as Programs
import Programs.Settings
import Utils

import Control.Monad.Trans.Maybe

import System.Exit( exitFailure )
import System.Environment( getArgs )


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
execProgram =
	do
		userInput <- userInputFromCmdArgs =<< lift getArgs
		configDir <- calcConfigDir $ ui_configDir userInput
		let cmd = ui_cmd userInput
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
