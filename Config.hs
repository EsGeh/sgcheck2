{-# LANGUAGE FlexibleContexts #-}
module Config(
	createConfig, -- this will create the config directory
	loadConfig, storeConfig,
	writeHiddenFile, loadHiddenFile,
) where

import Prelude hiding( FilePath )

import Global
import Data

import System.IO.Error
import System.Environment
import System.Directory
import qualified Data.Text as T
import Data.List as L


configPath = "config"
hiddenFileEnding = "sgcheck2"


loadConfig :: Path -> ErrT IO Settings
loadConfig configDir = do
	loadSettings $ configDir </> path_fromStr configPath

createConfig :: Path -> ErrT IO ()
createConfig configDir = do
	ExceptT $
		liftM Right (createDirectoryIfMissing True $ path_toStr configDir)
		`catchIOError` (\e -> return $ Left $ L.concat $ ["error creating config dir at \"", path_toStr configDir, "\": ", show e])

{- |lookup config dir, store settings
-}
storeConfig :: Path -> Settings -> ErrT IO ()
storeConfig configDir settings = do
	storeSettings (configDir </> path_fromStr configPath) settings
	where
		storeSettings :: Path -> Settings -> ErrT IO ()
		storeSettings path settings = do
			ExceptT $ liftM Right (writeFile (path_toStr path) $ settingsToString settings)
				`catchIOError` (\e -> return $ Left $ L.concat $ ["error writing config to \"", path_toStr path, "\": ", show e])

-- returns the origin:
loadHiddenFile :: Path -> Path -> ErrT IO Path
loadHiddenFile configDir name = do
	content <- lift $ readFile $ path_toStr $ configDir </> name <.> T.pack hiddenFileEnding
	ExceptT $ return $
		maybeToEither "couldn't read hidden file!" $
		liftM path_fromStr $
		L.stripPrefix "ORIGIN=" $
		L.takeWhile (/='\n') $
		content

writeHiddenFile :: Path -> Settings -> Path -> Path -> ErrT IO ()
writeHiddenFile configDir settings src dest = do
	lift $
		writeFile (path_toStr $ configDir </> filename src <.> T.pack hiddenFileEnding) $
			hiddenFileContent settings src


hiddenFileContent settings src =
	"ORIGIN=" ++ path_toStr src ++ "\n"

loadSettings :: Path -> ErrT IO Settings
loadSettings path = do
	file <- ExceptT $ liftM Right (readFile $ path_toStr path) `catchIOError` (\e -> return $ Left "error while loading: config file not found!")
	ExceptT $ return $ settingsFromString file
