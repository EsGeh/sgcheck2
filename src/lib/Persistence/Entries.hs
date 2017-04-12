{-# LANGUAGE RecordWildCards #-}
module Persistence.Entries(
	writeHiddenFile, loadHiddenFile,
	list
) where

--import Data.Settings
import Persistence.Global
import Utils

import System.Directory
import System.IO.Error

import System.FilePath as Path( (</>), (<.>) )
import qualified System.FilePath as Path

import Data.Yaml as Yaml

type Path = Path.FilePath


-- returns the origin:
loadHiddenFile :: Path -> Path -> ErrT IO Entry
loadHiddenFile configDir name =
	errT $
	fmap (mapLeft show) $
	Yaml.decodeFileEither $ configDir </> name <.> hiddenFileEnding

writeHiddenFile :: Path -> Entry -> ErrT IO ()
writeHiddenFile configDir entry@Entry{..} =
	lift $
	encodeFile (configDir </> (entry_pathOnThis entry) <.> hiddenFileEnding)
	entry

list :: Path -> ErrT IO [Entry]
list configDir =
	do
		allFiles <-
			ExceptT $
			(liftM Right $ getDirectoryContents $  configDir)
				`catchIOError` (\e -> return $ Left $ "error listing entries: " ++ show e)
		filtered <- mapM (loadHiddenFile configDir) $
			map Path.dropExtension $
			filter ((==("." ++ hiddenFileEnding)) . Path.takeExtension) $
			allFiles
		--liftIO $ putStrLn $ "Persistence.list: " ++ show filtered
		return filtered
	--return $ []

{-
catchIO msg x =
	ExceptT $
	liftIO $
	catchIOError x (\e -> return $ Left $ msg ++ show e)
-}
