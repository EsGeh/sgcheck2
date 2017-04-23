{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Programs.InOut.Utils(
	CopyFileParams(..),
	--ioSanityCheckParams,
	out_copyParams,
	in_copyParams,
	execCmd,
	execCmd',
	entryFromPathOnServer,
) where

import Data
import Utils

import System.Process
import System.Exit

import Prelude as P hiding( FilePath )

import System.FilePath as Path( (</>) {-, (<.>) -} )
import qualified System.FilePath as Path


data CopyFileParams
	= CopyFileParams {
		copyParams_cmd :: (String, [String]),
		copyParams_fullCommand :: String
	}
	deriving( Show, Eq, Ord )

type Path = Path.FilePath

entryFromPathOnServer :: Settings -> Path -> Entry
entryFromPathOnServer settings pathRelToOrigin =
	let
		entry_pathOnServer = pathRelToOrigin
		entry_serverPath = serverPath settings
		entry_thisPath = thisPath settings
		entry_excludePattern = []
		entry_execBeforeOut = Nothing
		entry_execBeforeIn = Nothing
	in Entry{..}

out_copyParams :: [String] -> Entry -> CopyFileParams
out_copyParams options entry@Entry{..} =
	let
		copyParams_cmd =
			("rsync", rsyncArgs)
		copyParams_fullCommand =
			uncurry showCommandForUser copyParams_cmd
		rsyncArgs =
			options
			++ map ("--exclude="++) entry_excludePattern
			++ [src, rsyncDest]
		src = entry_serverPath </> entry_pathOnServer
		rsyncDest = Path.takeDirectory $
			entry_thisPath </> (entry_pathOnThis entry)
	in
		CopyFileParams{..}

in_copyParams :: [String] -> Entry -> CopyFileParams
in_copyParams options entry@Entry{..} =
	let
		copyParams_cmd =
			("rsync", rsyncArgs)
		copyParams_fullCommand = uncurry showCommandForUser copyParams_cmd
		rsyncArgs =
			options
			++ map ("--exclude="++) entry_excludePattern
			++ [src, rsyncDest]
		src =
			entry_thisPath </> (entry_pathOnThis entry)
		rsyncDest =
			Path.takeDirectory $
				entry_serverPath </> entry_pathOnServer
	in
		CopyFileParams{..}

execCmd ::
	MonadIO m =>
	CopyFileParams -> (String -> String -> ErrT m ()) -> ErrT m String
execCmd copyParams writeLog =
		(runExceptT $ execCmd' $ copyParams) >>= \case
			Left (_, stdOut, stdErr) ->
				do
					writeLog
						(copyParams_fullCommand copyParams)
						(unlines [stdOut, "errors:", stdErr])
					throwE $ unlines $
						[ "rsync failed!"
						, "rsync stdout:", stdOut
						, "rsync stderr:", stdErr
						]
			Right stdOut -> return stdOut

execCmd' :: MonadIO m => CopyFileParams -> ExceptT (ExitCode,String,String) m String
execCmd' CopyFileParams{..} =
	do
		(exitCode, stdOut, stdErr) <- liftIO $ readProcessWithExitCode (fst copyParams_cmd) (snd copyParams_cmd) ""
		case exitCode of
			(ExitSuccess) ->
				return $ stdOut
			_ ->
				throwE $ (exitCode, stdOut, stdErr)
