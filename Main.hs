module Main where

import UserInput
import Data
import qualified Persistence
import Programs.InOut
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
							checkOut file settings (Persistence.fs_memorizeFile fileSys)
						CmdIn file ->
							checkIn file settings (Persistence.fs_lookupFile fileSys)
						CmdListFiles ->
							do
								(lift . lift . putStrLn . unlines . map entry_toText)
									=<<
										(lift $ Persistence.fs_list fileSys)
								MaybeT $ return $ Nothing
						CmdShowConfig ->
							showSettings settings
						CmdWriteConfig ->
							return $ settings

{-
execProgram :: ErrT IO ()
execProgram = do
	allData <-
		userInputFromCmdArgs =<< lift getArgs
	let
		cmdArgs = data_programInput allData
		storeSettings = data_storeSettings allData
	maybeNewSettings <-
		runMaybeT $ execFromCmd cmdArgs
	maybe
		(return ())
		storeSettings
		maybeNewSettings

execFromCmd :: CommandArgs -> MaybeT (ErrT IO) Settings
execFromCmd cmdArgs =
	let
		cmd = cmdArgs_cmd $ cmdArgs
		settings = cmdArgs_settings $ cmdArgs
		memorizeFile = cmdArgs_memorizeFile $ cmdArgs
		lookupFile = cmdArgs_lookupFile $ cmdArgs
	in
-}
