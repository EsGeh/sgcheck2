module Programs.InOut where

import Global
import Config

import Filesystem.Path
import Filesystem.Path.CurrentOS
import qualified System.Directory as D

import System.Process
import System.Exit

import Data.Text as T
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Text.Parsec


import Prelude as P hiding( FilePath )

outOptions = ["-avz"]
inOptions = ["-avz"]


checkOut params settings = do
	lift $ checkParams settings params
	lift $ checkRSync
	let (src,dest) = outOptionsFromFileName settings $ file params
	lift $ uncurry (copyFile outOptions) $ (src,dest)
	lift $ uncurry (writeHiddenFile settings) $ (src,dest)
	return settings

checkIn :: Parameters -> Settings -> MaybeT (ErrT IO) Settings
checkIn params settings = do
	lift $ checkParams settings params
	lift $ checkRSync
	(src,dest) <- lift $ inOptionsFromFileName settings $ file params
	lift2 $ putStrLn $ show (src,dest)
	lift $ uncurry (copyFile inOptions) $ (src,dest)
	return settings


outOptionsFromFileName :: Settings -> FilePath -> (FilePath, FilePath)
outOptionsFromFileName settings fileName = (src,dest)
	where
		src = serverPath settings </> fileName
		dest = thisPath settings </> filename fileName

inOptionsFromFileName :: Settings -> FilePath -> ErrT IO (FilePath, FilePath)
inOptionsFromFileName settings fileName = do
	origin <- loadHiddenFile fileName
	return (thisPath settings </> fileName, decodeString origin)

{-
calcDestPath settings filePath = do
	str <- loadHiddenFile
	return filePath
-}

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


checkParams :: Settings -> Parameters -> ErrT IO ()
checkParams settings params = do
	exists <- liftM2 (||)
		(lift $ D.doesFileExist (encodeString $ serverPath settings </> file params))
		(lift $ D.doesDirectoryExist (encodeString $ serverPath settings </> file params))
	when (not exists) $ throwE $ "file \'" ++ encodeString (file params) ++ "\' not found!"

checkRSync :: ErrT IO ()
checkRSync = do
	processRes <- lift $ readProcessWithExitCode "rsync" ["--help"] ""
	case processRes of
		(ExitSuccess, _, _) -> return ()
		(_, _, _) -> throwE $ "rsync not installed!"


concatPath l r = l ++ "/" ++ r
