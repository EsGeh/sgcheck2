module Persistence(
	FileSys(..),
	Settings.createConfig,
	withSettings,
	withFileSys
) where

--import Persistence.Global
import qualified Persistence.Settings as Settings
import qualified Persistence.Entries as Entries
import Data
import Utils
--
--import System.FilePath as Path( (</>), (<.>) )
import qualified System.FilePath as Path

type Path = Path.FilePath

data FileSys
	= FileSys {
		fs_memorizeFile ::
			-- -> Path -- configDir
			Entry
			-> ErrT IO (),
		fs_lookupFile ::
			Path ->
			ErrT IO Entry,
		fs_writeLogFile ::
			Entry
			-> String -- command
			-> String -- content
			-> ErrT IO (),
		fs_list ::
			ErrT IO [Entry]
	}


withSettings :: Path -> (Settings -> ErrT IO (Maybe Settings)) -> ErrT IO ()
withSettings configDir f =
	do
		settings <- Settings.loadSettings configDir
		mNewSettings <- f settings
		maybe
			(return ())
			(Settings.storeSettings configDir)
			mNewSettings

withFileSys :: Settings -> Path -> (FileSys -> a) -> a
withFileSys settings configDir f =
	f $ FileSys {
		fs_memorizeFile =
			Entries.writeHiddenFile configDir,
		fs_lookupFile =
			Entries.loadHiddenFile settings configDir,
		fs_writeLogFile =
			Entries.writeLogFile configDir,
		fs_list =
			Entries.list settings configDir
	}
