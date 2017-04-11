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
--
--import System.FilePath as Path( (</>), (<.>) )
import qualified System.FilePath as Path

type Path = Path.FilePath


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
