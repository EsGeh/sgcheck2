{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Persistence.Entries(
	writeHiddenFile, loadHiddenFile,
	writeLogFile,
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
import qualified Data.Time as Time

--import qualified Control.Exception.Base as Exc
import Data.List

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

writeLogFile :: Path -> Entry -> String -> String -> ErrT IO ()
writeLogFile configDir entry command logStr =
	do
		catchExceptions_IO "error creating log dir" $
			createDirectoryIfMissing True $
				configDir </> logDir </> entry_pathOnThis entry
		timeStr <- getTimeString
		catchExceptions_IO "error writing file" $
			writeFile (
				configDir </> logDir </> entry_pathOnThis entry </> concat [entry_pathOnThis entry, "-", timeStr] <.> "log"
			) $
				intercalate "\n" $
				[ command
				, "--------------------------------------"
				, logStr
				]

list :: Path -> ErrT IO [Entry]
list configDir =
	do
		allFiles <-
			catchExceptions_IO "error listing entries" $
			getDirectoryContents configDir
		filtered <- mapM (loadHiddenFile configDir) $
			map Path.dropExtension $
			filter ((==("." ++ hiddenFileEnding)) . Path.takeExtension) $
			allFiles
		--liftIO $ putStrLn $ "Persistence.list: " ++ show filtered
		return filtered
	--return $ []

getTimeString :: ErrT IO String
getTimeString =
	catchExceptions "error calculating time" $
	do
		zone <- Time.getCurrentTimeZone
		time <- Time.utcToLocalTime zone <$> Time.getCurrentTime
		return $
			Time.formatTime Time.defaultTimeLocale
				"%F_%T" $
			time
