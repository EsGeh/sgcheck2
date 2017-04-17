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
import Control.Monad.Error
import Data.Char
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
		writeLog = fs_writeLogFile fs
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
		entry :: Entry
		entry = Utils.entryFromPathOnServer settings copyCmd_file
		copyParams :: Utils.CopyFileParams
		copyParams = Utils.out_copyParams options entry
		sanityCheck =
			do
				maybeExistingEntry <-
					fmap Just (lookupFile $ entry_pathOnThis entry)
					`catchE` (const $ return Nothing)
				case maybeExistingEntry of
					Nothing ->
						do
							assertExistsLocal False entry
							assertExistsOnServer entry
					Just existingEntry ->
						do
							when (
									entry_pathOnThis existingEntry == entry_pathOnThis entry
									&& entry_pathOnServer existingEntry /= entry_pathOnServer entry
								) $
								throwE $ concat $ ["entry \"", entry_pathOnThis existingEntry, "\" already exists, and pointing to \"", entry_serverPath existingEntry </> entry_pathOnServer existingEntry, "\"" ]
							assertExistsLocal True entry
							assertExistsOnServer entry
	in
		do
			{-
			liftIO $ putStrLn $ "entry: " ++ show entry
			liftIO $ putStrLn $ "copyParams: " ++ show copyParams
			-}
			lift $ sanityCheck
			when copyFlags_printCommand $
				liftIO $ putStrLn $ "executing: " ++ Utils.copyParams_fullCommand copyParams
			stdOut <- execCopyCmd copyParams (writeLog entry)
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
				{-
					let localPath = entry_pathOnThis entry
					existsLocal <- liftIO $ SysDir.doesPathExist $ localPath
					when existsLocal $
						throwE $ concat $ [ "file or directory \"", localPath, "\" exists locally! (use \"add\", to add it to the list of managed files...)"]
					let remotePath = entry_pathOnServer entry
					existsRemote <- liftIO $ SysDir.doesPathExist $ remotePath
					when (not $ existsRemote) $
						throwE $ concat $ [ "file or directory \"", remotePath, "\" does not exist on the server!" ]
				-}

{- |
synchronize local -> server
constraints:

assert $ entry with same local path exists
	assert $ remote file exists
	assert $ local file exists
-}

checkIn :: CopyCommandParams -> Settings -> FileSys -> MaybeT (ErrT IO) Settings
checkIn CopyCommandParams{ copyCmd_flags=CopyFlags{..},.. } settings fs =
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
			stdOut <- execCopyCmd copyParams (writeLog entry)
			when (not $ copyFlags_simulate) $
				do
					lift $ writeLog
						entry
						(Utils.copyParams_fullCommand copyParams)
						stdOut
			when copyFlags_printRSyncOut $
				liftIO $ putStrLn $ unlines ["rsync output:", stdOut]
			MaybeT $ return Nothing

execCopyCmd copyParams writeLog =
		(runExceptT $ Utils.execCmd $ copyParams) >>= \case
			Left (_, stdOut, stdErr) ->
				do
					lift $ writeLog
						(Utils.copyParams_fullCommand copyParams)
						(unlines [stdOut, "errors:", stdErr])
					lift $ throwE $ unlines $
						[ "rsync failed!"
						, "rsync stdout:", stdOut
						, "rsync stderr:", stdErr
						]
			Right stdOut -> return stdOut

list :: Settings -> ListParams -> ListEntries -> MaybeT (ErrT IO) Settings
list settings listParams listEntries =
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
					inRes <-
						catch $ Utils.execCmd $ inParams
					outRes <-
						catch $ Utils.execCmd $ outParams
					let renderRes =
						P.concat $
						map (renderEntryOutput entry inRes outRes) $ listParams
					when (not $ null renderRes) $
						liftIO $ putStrLn renderRes
	where
		catch x =
			x `catchE` \(_, stdOut, stdErr) ->
				throwE $ unlines $
						[ "rsync failed!"
						, "rsync stdout:", stdOut
						, "rsync stderr:", stdErr
						]

renderEntryOutput :: Entry -> String -> String -> Output -> String
renderEntryOutput entry@Entry{..} inRes outRes x =
	let
		trimmedInRes = trim isSpace inRes
		trimmedOutRes = trim isSpace outRes
	in
		case x of
			SimpleOutput info -> simpleInfo info
			IfChangedOnThis l ->
				if trimmedInRes == "" then ""
				else changeInfo trimmedInRes l
			IfChangedOnServer l ->
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
				concatMap (either simpleInfo (flip rsyncInfo rsyncRet))
			rsyncInfo f =
				let
					concStr = rsyncF_interperseLines f
				in
					foldl1 (\a b-> a ++ concStr ++ b)
					. lines

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
