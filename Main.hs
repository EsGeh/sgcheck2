module Main where

--import Programs.Settings
import Data
import Programs.InOut
import Programs.Settings

import Global
import Args

import Text.Read(readEither)
import Control.Monad
import Control.Monad.Trans.Maybe
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
	allData <-
		userInputFromCmdArgs =<< lift getArgs
	--lift $ print allData
	let
		cmdArgs = data_programInput allData
		storeSettings = data_storeSettings allData
	newSettings <-
		runMaybeT $ execFromCmd cmdArgs
	maybe
		(storeSettings $ cmdArgs_settings $ data_programInput $ allData)
		storeSettings
		newSettings

execFromCmd :: CommandArgs -> MaybeT (ErrT IO) Settings
execFromCmd cmdArgs =
	let
		cmd = cmdArgs_cmd $ cmdArgs
		settings = cmdArgs_settings $ cmdArgs
		memorizeFile = cmdArgs_memorizeFile $ cmdArgs
		lookupFile = cmdArgs_lookupFile $ cmdArgs
	in
		case cmd of
			CmdOut file ->
				checkOut file settings memorizeFile
			CmdIn file ->
				checkIn file settings lookupFile
			CmdListFiles ->
				undefined
			CmdShowConfig ->
				showSettings settings
			CmdWriteConfig ->
				MaybeT $ return Nothing
			--ListFiles -> undefined
