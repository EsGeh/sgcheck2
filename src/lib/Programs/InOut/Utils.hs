module Programs.InOut.Utils(
	CopyFileParams(..),
	ioSanityCheckParams,
	thisPathFromEntry,
	outParamsFromEntry,
	outParams,
	inParams,
	--copyParams,
	execCmd,
	srcFromEntry,
	-- TESTING
	outOptionsFromFileName,
	inOptionsFromFileName,
) where

import Data
import Utils

--import Filesystem.Path.CurrentOS
import qualified System.Directory as D

import System.Process
import System.Exit

import Prelude as P hiding( FilePath )

import System.FilePath as Path( (</>) {-, (<.>) -} )
import qualified System.FilePath as Path
import Data.List

type Path = Path.FilePath

thisPathFromEntry =
	Path.takeFileName .
	srcFromEntry

outParamsFromEntry settings options entry =
	fmap (outParams settings options . Path.dropDrive) $
	stripPrefix (serverPath settings) $
	srcFromEntry entry

outParams :: Settings -> [String] -> Path -> CopyFileParams
outParams settings options =
	uncurry (copyParams options) .
	(outOptionsFromFileName settings)

inParams :: Settings -> [String] -> Path -> CopyFileParams
inParams settings options =
	uncurry (copyParams options) .
	(inOptionsFromFileName settings)

outOptionsFromFileName :: Settings -> Path -> (Path, Path)
outOptionsFromFileName settings pathRelToOrigin =
	(src,dest)
	where
		src = serverPath settings </> pathRelToOrigin
		dest = thisPath_fromOrigin settings pathRelToOrigin

inOptionsFromFileName :: Settings -> Path -> (Path, Path)
inOptionsFromFileName settings pathAtOrigin =
	(src, dest)
	where
		src =
			thisPath_fromOrigin settings pathAtOrigin
		dest =
			pathAtOrigin

thisPath_fromOrigin settings path =
	thisPath settings </> Path.takeFileName path

srcFromEntry :: Entry -> Path
srcFromEntry =
	entry_path

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
		rsyncDest = Path.takeDirectory $ dest
		rsyncArgs =
			(options ++ [src, rsyncDest])
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

ioSanityCheckParams :: Settings -> Path -> ErrT IO ()
ioSanityCheckParams settings file = do
	exists <- liftM2 (||)
		(lift $ D.doesFileExist (serverPath settings </> file))
		(lift $ D.doesDirectoryExist (serverPath settings </> file))
	when (not exists) $ throwE $ "file \'" ++ file ++ "\' not found!"
