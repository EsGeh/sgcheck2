module Programs.InOut where

import Data
import Global
--import Data
--import qualified Config

import Filesystem.Path
import Filesystem.Path.CurrentOS
import qualified System.Directory as D

import System.Process
import System.Exit

import Control.Monad.Trans.Maybe

import Text.Parsec


import Prelude as P hiding( FilePath )

outOptions :: [String]
outOptions = ["-avz"]
inOptions :: [String]
inOptions = ["-avz"]


checkOut :: Path -> Settings -> MemorizeFile -> MaybeT (ErrT IO) Settings
checkOut file settings memorizeFile = do
	lift $ checkParams settings file
	lift $ checkRSync
	let (src,dest) = outOptionsFromFileName settings $ file
	lift $ uncurry (copyFile outOptions) $ (src,dest)
	lift $ uncurry (memorizeFile settings) $ (src,dest)
	return settings

checkIn :: Path -> Settings -> LookupFile -> MaybeT (ErrT IO) Settings
checkIn file settings lookupFile = do
	lift $ checkParams settings file
	lift $ checkRSync
	(src,dest) <- lift $ inOptionsFromFileName settings file lookupFile
	lift2 $ putStrLn $ show (src,dest)
	lift $ uncurry (copyFile inOptions) $ (src,dest)
	return settings


outOptionsFromFileName :: Settings -> Path -> (Path, Path)
outOptionsFromFileName settings fileName = (src,dest)
	where
		src = serverPath settings </> fileName
		dest = thisPath settings </> filename fileName

inOptionsFromFileName :: Settings -> FilePath -> LookupFile -> ErrT IO (FilePath, FilePath)
inOptionsFromFileName settings fileName lookupFile = do
	entry <- lookupFile fileName
	return (thisPath settings </> fileName, entry_path entry)

parseSettingsTransform :: Parsec [(String,String)] () (Maybe String -> Maybe String)
parseSettingsTransform = do
	(k,v) <- anyToken
	return $ case k of
		"ORIGIN" -> \_ -> Just v
		_ -> fail "unknown key!"

{-|
	'copyFile opt "a/x" "b/x"' creates b/x with the same content as a/x
	a/x can be a file or a directory.

	! WARNING: 'copyFile opt "a/x" "b/y"' creates b/x with the same content as a/x
	! WARNING: src and dest must not end with "/"
-}
copyFile :: [String] -> FilePath -> FilePath -> ErrT IO ()
copyFile options src dest = do
	let rsyncDest = directory $ dest
	lift $ putStrLn $ "executing \'" ++ "rsync " ++ P.unwords (options ++ [encodeString src, encodeString rsyncDest]) ++ "\'"
	processRes <- lift $ readProcessWithExitCode "rsync" (options ++ [encodeString src, encodeString rsyncDest]) ""
	--processRes <- (return (ExitSuccess,undefined,undefined))
	case processRes of
		(ExitSuccess, _, _) -> return ()
		(_, _, _) -> throwE $ "rsync failed!"
	
{-
copyFile :: [String] -> Settings -> Parameters -> ErrT IO ()
copyFile options settings params = do
	lift $ putStrLn $ "executing \'" ++ "rsync" ++ P.unwords (options ++ [srcFile, destFile]) ++ "\'"
	processRes <- lift $ readProcessWithExitCode "rsync" (options ++ [srcFile, destFile]) ""
	case processRes of
		(ExitSuccess, _, _) -> return ()
		(_, _, _) -> throwE $ "rsync failed!"
	where
		srcFile = encodeString $ server </> fileName
		destFile = encodeString $ this

		server = serverPath settings :: FilePath
		this = thisPath settings :: FilePath
		fileName = file params :: FilePath
-}


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


-- concatPath l r = l ++ "/" ++ r
