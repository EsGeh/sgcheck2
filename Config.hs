{-# LANGUAGE FlexibleContexts #-}
module Config(
	loadConfig, storeConfig, lookupConfigDir,
	writeHiddenFile,
) where

import Prelude hiding( FilePath )

import Global

import System.IO.Error
import System.Directory
import System.Environment
import Data.Text as T


envVarConfigDir = "SGCHECK2_CONFIGPATH"
defConfigDir = liftM2 (++) getHomeDirectory (return "/.sgcheck2") :: IO String

configPath = "config"
hiddenFileEnding = "sgcheck2"



{- |lookup config dir, store settings
-}
storeConfig :: Settings -> ErrT IO ()
storeConfig settings = do
	configDir <- lookupConfigDir
	storeSettings (configDir </> decodeString configPath) settings
	where
		storeSettings :: Path -> Settings -> ErrT IO ()
		storeSettings path settings = do
			ExceptT $ liftM Right (writeFile (encodeString path) $ settingsToString settings)
				`catchIOError` (\e -> return $ Left "config file not found!")


{- |lookup the config dir by trying the following strategies:

  1. read the environment variable "envVarConfigDir"
	1. use ~/.sgcheck2

-}
lookupConfigDir:: ErrT IO FilePath
lookupConfigDir = do
	let lookup = (do
		fromEnv <- lookupEnv envVarConfigDir
		def <- defConfigDir >>= return . decodeString
		return $ fmap decodeString fromEnv `mplus` Just def) :: IO (Maybe FilePath)
	ExceptT $ liftM (maybeToEither "not installed correctly") lookup :: ErrT IO FilePath

{-
loadHiddenFile :: FilePath -> ErrT IO String
loadHiddenFile name = do
	configDir <- lookupConfigDir
	readFile $ configDir </> name
-}

writeHiddenFile settings src dest = do
	configDir <- lookupConfigDir
	lift $
		writeFile (encodeString $ configDir </> filename src <.> T.pack hiddenFileEnding) $
			hiddenFileContent settings src


hiddenFileContent settings src =
	"ORIGIN=" ++ encodeString src ++ "\n"

loadConfig :: ErrT IO Settings
loadConfig = do
	configDir <- lookupConfigDir
	loadSettings $ configDir </> decodeString configPath


loadSettings :: FilePath -> ErrT IO Settings
loadSettings path = do
	file <- ExceptT $ liftM Right (readFile $ encodeString path) `catchIOError` (\e -> return $ Left "config file not found!")
	ExceptT $ return $ settingsFromString file
