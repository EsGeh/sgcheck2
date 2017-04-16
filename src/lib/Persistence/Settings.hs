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
loadSettings configDir =
	let
		path = configDir </> configPath
	in
	do
		settings <- (ExceptT . return . settings_fromStr =<<) $
			catchExceptions_IO (L.concat $ ["error loading config from \"", path, "\"" ]) $
				readFile $ path
		when (not $ settings_isValid settings) $
			throwE $ concat ["Invalid settings file \"", path ,"\". Please fix manually!"]
		return settings

{- |lookup config dir, store settings
-}
storeSettings :: Path -> Settings -> ErrT IO ()
storeSettings configDir settings =
	let
		path = configDir </> configPath
	in
		catchExceptions_IO (L.concat $ ["error writing config to \"", path, "\"" ]) $
			writeFile path $ settings_toStr settings

createConfig :: Path -> ErrT IO ()
createConfig configDir =
	do
		catchExceptions_IO "error creating config dir" $
			do
				createDirectoryIfMissing True $ configDir
				createDirectoryIfMissing True $
					configDir </> logDir
		storeSettings configDir defSettings
		liftIO $ putStrLn $
			concat $
			[ "Created config at \"", configDir, "\"", ". "
			, "Please enter the necessary information to the config file manually!"
			]
