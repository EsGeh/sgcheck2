module Persistence(
	FileSys(..),
	Settings.createConfig,
	withSettings,
	withFileSys
) where

import Persistence.Global
import qualified Persistence.Settings as Settings
import qualified Persistence.Entries as Entries
import Data.Settings
import Utils
import qualified Utils.Path as Path

type Path = Path.Path


withSettings :: Path -> (Settings -> ErrT IO (Maybe Settings)) -> ErrT IO ()
withSettings configDir f =
	do
		settings <- Settings.loadSettings configDir
		mNewSettings <- f settings
		maybe
			(return ())
			(Settings.storeSettings configDir)
			mNewSettings

withFileSys :: Path -> (FileSys -> ErrT IO a) -> ErrT IO a
withFileSys configDir f =
	f $ FileSys {
		fs_memorizeFile =
			Entries.writeHiddenFile configDir,
		fs_lookupFile =
			Entries.loadHiddenFile configDir,
		fs_list =
			Entries.list configDir
	}
