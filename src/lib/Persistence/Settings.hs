{-# LANGUAGE FlexibleContexts #-}
module Persistence.Settings(
	createConfig, -- this will create the config directory
	loadSettings, storeSettings,
) where

import Data.Settings
import Persistence.Global
import Utils

import System.IO.Error
import System.Directory
import Data.List as L
--
import System.FilePath as Path( (</>) {-, (<.>) -} )
import qualified System.FilePath as Path

type Path = Path.FilePath


loadSettings :: Path -> ErrT IO Settings
loadSettings configDir = do
	loadSettingsFromPath $ configDir </> configPath

createConfig :: Path -> ErrT IO ()
createConfig configDir = do
	ExceptT $
		liftM Right (createDirectoryIfMissing True $ configDir)
		`catchIOError` (\e -> return $ Left $ L.concat $ ["error creating config dir at \"", configDir, "\": ", show e])
	storeSettings configDir defSettings

{- |lookup config dir, store settings
-}
storeSettings :: Path -> Settings -> ErrT IO ()
storeSettings configDir settings = do
	storeSettings' (configDir </> configPath)
	where
		storeSettings' :: Path -> ErrT IO ()
		storeSettings' path = do
			ExceptT $ liftM Right (writeFile path $ settings_toStr settings)
				`catchIOError` (\e -> return $ Left $ L.concat $ ["error writing config to \"", path, "\": ", show e])

loadSettingsFromPath :: Path -> ErrT IO Settings
loadSettingsFromPath path = do
	file <- ExceptT $ liftM Right (readFile $ path) `catchIOError` (\e -> return $ Left $ "error while loading config: " ++ show e)
	ExceptT $ return $ settings_fromStr file
