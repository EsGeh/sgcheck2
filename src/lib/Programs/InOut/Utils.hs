module Programs.InOut.Utils(
	CopyFileParams(..),
	checkParams,
	outParams,
	inParams,
	--copyParams,
	execCmd,
	pathFromEntry,
	-- TESTING
	outOptionsFromFileName,
	inOptionsFromFileName,
) where

import Data
import Utils

--import Filesystem.Path.CurrentOS
import qualified Utils.Path as Path
import qualified System.Directory as D

import System.Process
import System.Exit

import Prelude as P hiding( FilePath )

type Path = Path.Path
(</>) = (Path.</>)
(<.>) = (Path.<.>)


outParams :: Settings -> [String] -> Path -> CopyFileParams
outParams settings options =
	uncurry (copyParams options) .
	(outOptionsFromFileName settings)

inParams :: Settings -> [String] -> Path -> CopyFileParams
inParams settings options =
	uncurry (copyParams options) .
	(inOptionsFromFileName settings)

-- filename relative to 'serverPath settings'
outOptionsFromFileName :: Settings -> Path -> (Path, Path)
outOptionsFromFileName settings fileName =
	(src,dest)
	where
		src = serverPath settings </> fileName
		dest = thisPath settings </> Path.filename fileName

-- filename relative to 'thisPath settings'
inOptionsFromFileName :: Settings -> Path -> (Path, Path)
inOptionsFromFileName settings fileName =
	(src, dest)
	where
		src = thisPath settings </> fileName
		dest = serverPath settings </> fileName -- entry_path entry

pathFromEntry :: Entry -> Path
pathFromEntry =
	Path.filename . entry_path -- TODO: appears to be wrong!!
{-
 	-- strip path to the server:
	Path.concat .
	drop 1 .
	Path.splitDirectories .
	entry_path
-}

data CopyFileParams
	= CopyFileParams {
		copyParams_cmd :: (String, [String]),
		copyParams_fullCommand :: String,
		copyParams_src :: Path,
		copyParams_dest :: Path
		--copyParams_options :: [String]
	}
	deriving( Show, Eq, Ord )

{-|
	'copyParams opt "a/x" "b/x"' creates b/x with the same content as a/x
	a/x can be a file or a directory.

	! WARNING: 'copyFile opt "a/x" "b/y"' creates b/x with the same content as a/x
	! WARNING: src and dest must not end with "/"
-}
copyParams :: [String] -> Path -> Path -> CopyFileParams
copyParams options src dest =
	let
		rsyncDest = Path.directory $ dest
		rsyncArgs =
			(options ++ [Path.path_toStr src, Path.path_toStr rsyncDest])
		rsyncCmd =
			("rsync", rsyncArgs)
	in
		CopyFileParams {
			copyParams_cmd = rsyncCmd,
			copyParams_fullCommand = uncurry showCommandForUser rsyncCmd,
			copyParams_src = src,
			copyParams_dest = dest
			--copyParams_options = options
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
		(lift $ D.doesFileExist (Path.path_toStr $ serverPath settings </> file))
		(lift $ D.doesDirectoryExist (Path.path_toStr $ serverPath settings </> file))
	when (not exists) $ throwE $ "file \'" ++ Path.path_toStr file ++ "\' not found!"

{-
checkRSync :: ErrT IO ()
checkRSync = do
	processRes <- lift $ readProcessWithExitCode "rsync" ["--help"] ""
	case processRes of
		(ExitSuccess, _, _) -> return ()
		(_, _, _) -> throwE $ "rsync not installed!"
-}
