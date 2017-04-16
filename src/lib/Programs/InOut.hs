{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Programs.InOut where

import Data
import Programs.InOut.Params
import Persistence
import qualified Programs.InOut.Utils as Utils
import Utils

import Control.Monad.Trans.Maybe
import Data.Char
import Prelude as P hiding( FilePath )

import System.FilePath as Path( (</>) {- (<.>) -})
import qualified System.FilePath as Path

type Path = Path.FilePath


type MemorizeFile =
	Entry
	-> ErrT IO ()

type LookupFile =
	Path -> ErrT IO Entry

type ListEntries =
	ErrT IO [Entry]


checkOut :: CopyCommandParams -> Settings -> FileSys -> MaybeT (ErrT IO) Settings
checkOut CopyCommandParams{ copyCmd_flags=CopyFlags{..},.. } settings fs =
	let
		options =
			["-azv", "-u"]
			++ (if copyFlags_simulate then ["-n"] else [])
			++ copyFlags_addRSyncOpts
		memorizeFile = fs_memorizeFile fs
		writeLog = fs_writeLogFile fs
		entry :: Entry
		entry = Utils.entryFromPathOnServer settings copyCmd_file
		copyParams :: Utils.CopyFileParams
		copyParams = Utils.out_copyParams options entry
	in
		do
			{-
			liftIO $ putStrLn $ "entry: " ++ show entry
			liftIO $ putStrLn $ "copyParams: " ++ show copyParams
			-}
			lift $ assertConfigDidntChange settings entry
			sanityCheckOutParams entry
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

sanityCheckOutParams entry = return ()

checkIn :: CopyCommandParams -> Settings -> FileSys -> MaybeT (ErrT IO) Settings
checkIn CopyCommandParams{ copyCmd_flags=CopyFlags{..},.. } settings fs =
	let
		options =
			["-azv", "-u", "--delete"]
			++ (if copyFlags_simulate then ["-n"] else [])
			++ copyFlags_addRSyncOpts
		lookupFile = fs_lookupFile fs
		writeLog = fs_writeLogFile fs
	in
		do
			(entry :: Entry) <-
				lift $ lookupFile $
				copyCmd_file
			lift $ assertConfigDidntChange settings entry
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

assertConfigDidntChange settings entry =
	do
		when (entry_thisPath entry /= thisPath settings) $
			throwE "thisPath has changed! (please update manually!)"
		when (entry_serverPath entry /= serverPath settings) $
			throwE "serverPath has changed! (please update manually!)"

sanityCheckInParams entry = return ()

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

-- |trim at the beginning and the end
trim :: (a -> Bool) -> [a] -> [a]
trim cond = f . f
	where
		f = reverse . dropWhile cond
