{-# LANGUAGE FlexibleContexts #-}
module Persistence.Settings(
	createConfig, -- this will create the config directory
	loadSettings, storeSettings,
) where

import Data.Settings
import Persistence.Global
import Utils
import qualified Utils.Path as Path

import System.IO.Error
import System.Directory
import Data.List as L

type Path = Path.Path
(</>) = (Path.</>)
(<.>) = (Path.<.>)


loadSettings :: Path -> ErrT IO Settings
loadSettings configDir = do
	loadSettingsFromPath $ configDir </> Path.path_fromStr configPath

createConfig :: Path -> ErrT IO ()
createConfig configDir = do
	ExceptT $
		liftM Right (createDirectoryIfMissing True $ Path.path_toStr configDir)
		`catchIOError` (\e -> return $ Left $ L.concat $ ["error creating config dir at \"", Path.path_toStr configDir, "\": ", show e])
	storeSettings configDir defSettings

{- |lookup config dir, store settings
-}
storeSettings :: Path -> Settings -> ErrT IO ()
storeSettings configDir settings = do
	storeSettings' (configDir </> Path.path_fromStr configPath)
	where
		storeSettings' :: Path -> ErrT IO ()
		storeSettings' path = do
			ExceptT $ liftM Right (writeFile (Path.path_toStr path) $ settings_toStr settings)
				`catchIOError` (\e -> return $ Left $ L.concat $ ["error writing config to \"", Path.path_toStr path, "\": ", show e])

loadSettingsFromPath :: Path -> ErrT IO Settings
loadSettingsFromPath path = do
	file <- ExceptT $ liftM Right (readFile $ Path.path_toStr path) `catchIOError` (\e -> return $ Left $ "error while loading config: " ++ show e)
	ExceptT $ return $ settings_fromStr file
