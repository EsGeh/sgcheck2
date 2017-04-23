{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Programs.InOut where

import Data
import Programs.InOut.Params
import Persistence
import qualified Programs.InOut.Utils as Utils
import Utils

import Control.Monad.Trans.Maybe
--import Control.Monad.Error
import Data.Char
import Data.List
import Prelude as P hiding( FilePath )

import System.FilePath as Path( (</>) {- (<.>) -})
import qualified System.FilePath as Path

import qualified System.Directory as SysDir

type Path = Path.FilePath


type MemorizeFile =
	Entry
	-> ErrT IO ()

type LookupFile =
	Path -> ErrT IO Entry

type ListEntries =
	ErrT IO [Entry]


{- |
add a file to list
constraints:

	assert $ entry with same local path DOESNT exist
	assert $ remote file exists
	assert $ local file exists
-}

add :: Path -> Settings -> FileSys -> MaybeT (ErrT IO) Settings
add path settings fs =
	let
		memorizeFile = fs_memorizeFile fs
		--writeLog = fs_writeLogFile fs
		lookupFile = fs_lookupFile fs
		entry :: Entry
		entry = Utils.entryFromPathOnServer settings path
		sanityCheck =
			do
				maybeExistingEntry <-
					fmap Just (lookupFile $ entry_pathOnThis entry)
					`catchE` (const $ return Nothing)
				case maybeExistingEntry of
					Nothing -> return ()
					Just existingEntry ->
						when (
							entry_pathOnThis existingEntry == entry_pathOnThis entry
						) $
							throwE $ concat $ ["entry \"", entry_pathOnThis existingEntry, "\" already exists, and pointing to \"", entry_serverPath existingEntry </> entry_pathOnServer existingEntry, "\"" ]
				assertExistsLocalWithHint "(maybe you want to use \"out\"?" True entry
				assertExistsOnServer entry
	in
		do
			{-
			liftIO $ putStrLn $ "entry: " ++ show entry
			liftIO $ putStrLn $ "copyParams: " ++ show copyParams
			-}
			lift $ sanityCheck
			lift $ memorizeFile entry
			MaybeT $ return Nothing

{- |
synchronize server -> local
constraints:

if entry with same local path exists:
	assert $ path on server must be equal
	assert $ remote file exists
	assert $ local file exists
else:
	assert $ remote file exists
	assert $ local file DOESNT exist
-}

checkOut :: CopyCommandParams -> Settings -> FileSys -> MaybeT (ErrT IO) Settings
checkOut CopyCommandParams{ copyCmd_flags=CopyFlags{..},.. } settings fs =
	let
		options =
			["-azv", "-u"]
			++ (if copyFlags_simulate then ["-n"] else [])
			++ copyFlags_addRSyncOpts
		memorizeFile = fs_memorizeFile fs
		lookupFile = fs_lookupFile fs
		writeLog = fs_writeLogFile fs
		{-
		entry :: Entry
		entry = Utils.entryFromPathOnServer settings copyCmd_file
		-}
		-- |sanity check, and create entry:
		getEntry =
			let defEntry = Utils.entryFromPathOnServer settings copyCmd_file
			in
			do
				maybeExistingEntry <-
					fmap Just (lookupFile $ entry_pathOnThis defEntry)
					`catchE` (const $ return Nothing)
				case maybeExistingEntry of
					Nothing ->
						do
							assertExistsLocal False defEntry
							assertExistsOnServer defEntry
							return $ defEntry
					Just existingEntry ->
						do
							execPrepareCmd copyFlags_printCommand (writeLog existingEntry) $ entry_execBeforeOut existingEntry
							when (
									entry_pathOnThis existingEntry == entry_pathOnThis defEntry
									&& entry_pathOnServer existingEntry /= entry_pathOnServer defEntry
								) $
								throwE $ concat $ ["entry \"", entry_pathOnThis existingEntry, "\" already exists, and pointing to \"", entry_serverPath existingEntry </> entry_pathOnServer existingEntry, "\"" ]
							assertExistsLocal True existingEntry
							assertExistsOnServer existingEntry
							return $ existingEntry
	in
		do
			{-
			liftIO $ putStrLn $ "entry: " ++ show entry
			liftIO $ putStrLn $ "copyParams: " ++ show copyParams
			-}
			entry <- lift $ getEntry
			let
				copyParams :: Utils.CopyFileParams
				copyParams = Utils.out_copyParams options entry
			when copyFlags_printCommand $
				liftIO $ putStrLn $ "executing: " ++ Utils.copyParams_fullCommand copyParams
			stdOut <- lift $ Utils.execCmd copyParams (writeLog entry)
			when (not $ copyFlags_simulate) $
				do
					lift $ writeLog
						entry
						(Utils.copyParams_fullCommand copyParams)
						stdOut
					lift $ memorizeFile entry
			when copyFlags_printRSyncOut $
				liftIO $ putStrLn $ unlines ["rsync output:", stdOut]
			MaybeT $ return Nothing

{- |
synchronize local -> server
constraints:

assert $ entry with same local path exists
	assert $ remote file exists
	assert $ local file exists
-}

checkIn :: CopyCommandParams -> Settings -> FileSys -> MaybeT (ErrT IO) Settings
checkIn CopyCommandParams{ copyCmd_flags=CopyFlags{..},.. } _ fs =
	let
		options =
			["-azv", "-u", "--delete"]
			++ (if copyFlags_simulate then ["-n"] else [])
			++ copyFlags_addRSyncOpts
		lookupFile = fs_lookupFile fs
		writeLog = fs_writeLogFile fs
		sanityCheck entry =
			do
				assertExistsLocal True entry
				assertExistsOnServer entry
	in
		do
			(entry :: Entry) <-
				lift $ lookupFile $
				copyCmd_file
			lift $ execPrepareCmd copyFlags_printCommand (writeLog entry) $ entry_execBeforeIn entry
			lift $ sanityCheck entry
			let
				copyParams :: Utils.CopyFileParams
				copyParams = Utils.in_copyParams options entry
			when copyFlags_printCommand $
				liftIO $ putStrLn $ "executing: " ++ Utils.copyParams_fullCommand copyParams
			{-
			liftIO $ putStrLn $ "entry: " ++ show entry
			liftIO $ putStrLn $ "copyParams: " ++ show copyParams
			-}
			stdOut <- lift $ Utils.execCmd copyParams (writeLog entry)
			when (not $ copyFlags_simulate) $
				do
					lift $ writeLog
						entry
						(Utils.copyParams_fullCommand copyParams)
						stdOut
			when copyFlags_printRSyncOut $
				liftIO $ putStrLn $ unlines ["rsync output:", stdOut]
			MaybeT $ return Nothing

list :: Settings -> ListParams -> ListEntries -> MaybeT (ErrT IO) Settings
list _ listParams listEntries =
	(>> (MaybeT $ return Nothing)) $
	lift $ listEntries >>= \entries ->
		forM entries $ \entry ->
			let
				flags = ["-az", "-i", "-n", "-u"]
				outParams =
					Utils.out_copyParams flags entry
				inParams =
					Utils.in_copyParams flags entry
			in
				do
					--liftIO $ putStrLn $ "rendering: " ++ show entry
					let
						inRes =
							do
								execPrepareCmd False (\_ _ -> return ()) $ entry_execBeforeIn entry
								catch $ Utils.execCmd' $ inParams
						outRes =
							do
								execPrepareCmd False (\_ _ -> return ()) $ entry_execBeforeOut entry
								catch $ Utils.execCmd' $ outParams
					renderRes <-
						fmap concat $
						mapM (renderEntryOutput entry inRes outRes) $
						listParams
					unless (null renderRes) $
						liftIO $ putStrLn renderRes
	where
		catch x =
			x `catchE` \(_, stdOut, stdErr) ->
				throwE $ unlines $
						[ "rsync failed!"
						, "rsync stdout:", stdOut
						, "rsync stderr:", stdErr
						]

renderEntryOutput :: Entry -> ErrT IO String -> ErrT IO String -> Output -> ErrT IO String
renderEntryOutput entry@Entry{..} inRes outRes x =
	case x of
		SimpleOutput info -> return $ simpleInfo info
		IfChangedOnThis l ->
			fmap (trim isSpace) inRes >>= \trimmedInRes ->
				return $
				if trimmedInRes == "" then ""
				else changeInfo trimmedInRes l
		IfChangedOnServer l ->
			fmap (trim isSpace) outRes >>= \trimmedOutRes ->
				return $
				if trimmedOutRes == "" then ""
				else changeInfo trimmedOutRes l
	where
		simpleInfo info=
			case info of
				Str s -> s
				Path -> entry_pathOnThis entry
				ThisPath -> entry_thisPath </> entry_pathOnThis entry
				ServerPath -> entry_serverPath </> entry_pathOnServer
		changeInfo rsyncRet =
			concatMap $ either simpleInfo $ flip rsyncInfo rsyncRet
		rsyncInfo f =
			intercalate (rsyncF_interperseLines f) .
			lines

execPrepareCmd ::
	MonadIO m =>
	Bool -> (String -> String -> ErrT m ()) -> Maybe Path -> ErrT m ()
execPrepareCmd doPrint writeLog str =
	case
			str >>= \cmd ->
			do
				let copyParams_fullCommand = cmd
				copyParams_cmd <- uncons $ words cmd
				return $ Utils.CopyFileParams{..}
		of
		Just params -> 
			do
				when doPrint $
					liftIO $ putStrLn $ "executing: " ++ Utils.copyParams_fullCommand params
				stdOut <- Utils.execCmd params writeLog
				unless (null $ stdOut) $ liftIO $ putStrLn $ unlines ["output:", stdOut]
		Nothing -> return ()

assertExistsLocal ::
	MonadIO m =>
	Bool -> Entry -> ErrT m ()
assertExistsLocal =
	assertExistsLocalWithHint ""

assertExistsLocalWithHint ::
	--MonadError String m =>
	MonadIO m =>
	String -> Bool -> Entry -> ErrT m ()
assertExistsLocalWithHint hint does entry =
	do
		let localPath =
			entry_thisPath entry </> entry_pathOnThis entry
		existsLocal <- liftIO $ SysDir.doesPathExist $ localPath
		when ((if does then not else id) $ existsLocal) $
			throwE $
				concat [ "file or directory \"", localPath, "\" does ", if does then "not " else "", "exist locally!"]
				++
				if not $ null hint
				then " " ++ hint
				else ""

assertExistsOnServer ::
	MonadIO m =>
	Entry -> ErrT m ()
assertExistsOnServer entry =
	do
		let remotePath =
			entry_serverPath entry </> entry_pathOnServer entry
		existsRemote <- liftIO $ SysDir.doesPathExist $ remotePath
		when (not $ existsRemote) $
			throwE $ concat $ [ "file or directory \"", remotePath, "\" does not exist on the server!" ]

-- |trim at the beginning and the end
trim :: (a -> Bool) -> [a] -> [a]
trim cond = f . f
	where
		f = reverse . dropWhile cond
