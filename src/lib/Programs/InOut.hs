module Programs.InOut where

import Data
import Programs.InOut.Params
import Utils

import Filesystem.Path
import Filesystem.Path.CurrentOS
import qualified System.Directory as D

import System.Process
import System.Exit

import Control.Monad.Trans.Maybe

import Text.Parsec
import Data.Char


import Prelude as P hiding( FilePath )


type MemorizeFile =
	Settings
	-> Path -- src
	-> Path -- dest
	-> ErrT IO ()

type LookupFile =
	Path -> ErrT IO Entry

type ListEntries =
	ErrT IO [Entry]

{-
outOptions :: [String]
outOptions = ["-avz"]
inOptions :: [String]
inOptions = ["-avz"]
-}

checkOut :: CopyCommandParams -> Settings -> MemorizeFile -> MaybeT (ErrT IO) Settings
checkOut copyCmdParams settings memorizeFile =
	let
		file = copyCmd_file copyCmdParams
		flags = copyCmd_flags copyCmdParams
		options =
			(["-azv", "-u"]++) $
			(++copyFlags_addRSyncOpts flags) $
			map snd $
			filter (\(cond, _) -> cond flags) $
			[ (copyFlags_simulate, "-n")
			]
	in
		do
			lift $ checkParams settings file
			-- lift $ checkRSync
			let
				cmdParams :: CopyFileParams
				cmdParams =
					uncurry (copyParams options) $
					(outOptionsFromFileName settings $ file :: (Path, Path))
			when (copyFlags_printCommand flags) $
				lift2 $ putStrLn $ "executing: " ++ copyParams_fullCommand cmdParams
			cmdRet <- runExceptT $ uncurry execCmd $ copyParams_cmd cmdParams
			case cmdRet of
				Left (_, stdOut, stdErr) ->
					lift $ throwE $ unlines $
						[ "rsync failed!"
						, "rsync stdout:", stdOut
						, "rsync stderr:", stdErr
						]
				Right stdOut ->
					liftIO $ putStrLn $ unlines ["rsync output:", stdOut]
			let
				src = copyParams_src cmdParams
				dest = copyParams_dest cmdParams
			lift $ uncurry (memorizeFile settings) $ (src,dest)
			return settings

checkIn :: CopyCommandParams -> Settings -> LookupFile -> MaybeT (ErrT IO) Settings
checkIn copyCmdParams settings lookupFile =
	let
		file = copyCmd_file copyCmdParams
		flags = copyCmd_flags copyCmdParams
		options =
			(["-azv", "-u", "--delete"] ++) $
			(++copyFlags_addRSyncOpts flags) $
			map snd $
			filter (\(cond, _) -> cond flags) $
			[ (copyFlags_simulate, "-n")
			]
	in
		do
			lift $ checkParams settings file
			-- lift $ checkRSync
			--cmdParams :: CopyFileParams
			cmdParams <- lift $
				liftM (
					uncurry (copyParams options)
					. inOptionsFromFileName settings
					. pathFromEntry
				) $
				lookupFile file
			when (copyFlags_printCommand flags) $
				lift2 $ putStrLn $ "executing: " ++ copyParams_fullCommand cmdParams
			cmdRet <- runExceptT $ uncurry execCmd $ copyParams_cmd cmdParams
			case cmdRet of
				Left (_, stdOut, stdErr) ->
					lift $ throwE $ unlines $
						[ "rsync failed!"
						, "rsync stdout:", stdOut
						, "rsync stderr:", stdErr
						]
				Right stdOut ->
					liftIO $ putStrLn $ unlines ["rsync output:", stdOut]
			MaybeT $ return Nothing

list :: Settings -> ListParams -> ListEntries -> MaybeT (ErrT IO) Settings
list settings listParams listEntries =
	do
		lift $
			mapM_ (lift . putStrLn <=< catch . (infoFromEntry settings listParams)) =<< listEntries
		MaybeT $ return $ Nothing
	where
		catch x =
			x `catchE` \(_, stdOut, stdErr) ->
				throwE $ unlines $
						[ "rsync failed!"
						, "rsync stdout:", stdOut
						, "rsync stderr:", stdErr
						]

infoFromEntry ::
	MonadIO m =>
	Settings -> ListParams -> Entry
	-> ExceptT (ExitCode,String,String) m String
infoFromEntry settings listParams entry =
	let
		flags = ["-az", "-i", "-n", "-u"]
		file = pathFromEntry entry
		checkInParams =
			uncurry (copyParams flags) $
			inOptionsFromFileName settings $
			file
		checkOutParams =
			uncurry (copyParams $ flags) $
			--(\(src, dest) -> (src </> path_fromStr "", dest </> file)) $
			outOptionsFromFileName settings $
			file
	in
		do
			-- liftIO $ putStrLn $ "in" ++ show checkInParams
			-- liftIO $ putStrLn $ "out" ++ show checkOutParams
			inRes <- uncurry execCmd $ copyParams_cmd checkInParams
			outRes <- uncurry execCmd $ copyParams_cmd checkOutParams
			return $ calcOutput checkInParams inRes outRes
		where
			calcOutput inParams inRes outRes =
				P.concat $
				map toOutput $ listParams
				where
					toOutput :: Output -> String
					toOutput x =
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
										Path -> path_toStr $ pathFromEntry entry
										ThisPath -> path_toStr $ copyParams_src inParams
										ServerPath -> path_toStr $ copyParams_dest inParams
								changeInfo rsyncRet =
									P.concat .
									map (either simpleInfo (flip rsyncInfo rsyncRet))
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

-- filename relative to 'serverPath settings'
outOptionsFromFileName :: Settings -> Path -> (Path, Path)
outOptionsFromFileName settings fileName =
	(src,dest)
	where
		src = serverPath settings </> fileName
		dest = thisPath settings </> filename fileName

pathFromEntry :: Entry -> Path
pathFromEntry = filename . entry_path

-- filename relative to 'thisPath settings'
inOptionsFromFileName :: Settings -> Path -> (FilePath, FilePath)
inOptionsFromFileName settings fileName =
	(src, dest)
	where
		src = thisPath settings </> fileName
		dest = serverPath settings </> fileName -- entry_path entry

parseSettingsTransform :: Parsec [(String,String)] () (Maybe String -> Maybe String)
parseSettingsTransform = do
	(k,v) <- anyToken
	return $ case k of
		"ORIGIN" -> \_ -> Just v
		_ -> fail "unknown key!"


data CopyFileParams
	= CopyFileParams {
		copyParams_cmd :: (String, [String]),
		copyParams_fullCommand :: String,
		copyParams_src :: Path,
		copyParams_dest :: Path,
		copyParams_options :: [String]
	}
	deriving( Show )

{-|
	'copyParams opt "a/x" "b/x"' creates b/x with the same content as a/x
	a/x can be a file or a directory.

	! WARNING: 'copyFile opt "a/x" "b/y"' creates b/x with the same content as a/x
	! WARNING: src and dest must not end with "/"
-}
copyParams :: [String] -> Path -> Path -> CopyFileParams
copyParams options src dest =
	let
		rsyncDest = directory $ dest
		rsyncArgs =
			(options ++ [encodeString src, encodeString rsyncDest])
		rsyncCmd =
			("rsync", rsyncArgs)
	in
		CopyFileParams {
			copyParams_cmd = rsyncCmd,
			copyParams_fullCommand = uncurry showCommandForUser rsyncCmd,
			copyParams_src = src,
			copyParams_dest = dest,
			copyParams_options = options
		}

execCmd :: MonadIO m => String -> [String] -> ExceptT (ExitCode,String,String) m String
execCmd cmd args =
	do
		(exitCode, stdOut, stdErr) <- liftIO $ readProcessWithExitCode cmd args ""
		case exitCode of
			(ExitSuccess) ->
				return $ stdOut
			_ ->
				throwE $ (exitCode, stdOut, stdErr)

checkParams :: Settings -> Path -> ErrT IO ()
checkParams settings file = do
	exists <- liftM2 (||)
		(lift $ D.doesFileExist (encodeString $ serverPath settings </> file))
		(lift $ D.doesDirectoryExist (encodeString $ serverPath settings </> file))
	when (not exists) $ throwE $ "file \'" ++ encodeString file ++ "\' not found!"

checkRSync :: ErrT IO ()
checkRSync = do
	processRes <- lift $ readProcessWithExitCode "rsync" ["--help"] ""
	case processRes of
		(ExitSuccess, _, _) -> return ()
		(_, _, _) -> throwE $ "rsync not installed!"