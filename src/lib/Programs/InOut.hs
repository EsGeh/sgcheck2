module Programs.InOut where

import Data
import Programs.InOut.Params
import Programs.InOut.Utils
import Utils

import System.Exit

import Control.Monad.Trans.Maybe
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
			let
				cmdParams :: CopyFileParams
				cmdParams =
					outParams settings options $ file
			when (copyFlags_printCommand flags) $
				lift2 $ putStrLn $ "executing: " ++ copyParams_fullCommand cmdParams
			lift $ checkParams settings file
			-- lift $ checkRSync
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
			--lift $ checkParams settings file
			-- lift $ checkRSync
			--cmdParams :: CopyFileParams
			cmdParams <- lift $
				fmap (inParams settings options . pathFromEntry) $
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
	lift listEntries >>= \entries ->
		do
			outputEntries <- lift $
				forM entries $
					catch . infoFromEntry settings listParams
			forM_ outputEntries $ \entry ->
				when (entry /= "") $ lift $ lift $ putStrLn entry
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
			inParams settings flags $
			file
		checkOutParams =
			outParams settings flags file
	in
		do
			inRes <-
				(uncurry execCmd $ copyParams_cmd checkInParams)
			outRes <-
				(uncurry execCmd $ copyParams_cmd checkOutParams)
			return $
				P.concat $
				map (renderOutput entry checkInParams inRes outRes) $ listParams
			--return $ calcOutput listParams entry checkInParams inRes outRes

renderOutput :: Entry -> CopyFileParams -> String -> String -> Output -> String
renderOutput entry cpyInParams inRes outRes x =
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
					ThisPath -> path_toStr $ copyParams_src cpyInParams
					ServerPath -> path_toStr $ copyParams_dest cpyInParams
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
