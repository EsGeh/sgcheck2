module Persistence(
	FileSys(..),
	Settings.createConfig,
	calcConfigDir,
	withSettings,
	withFileSys
) where

import Persistence.Global
import qualified Persistence.Settings as Settings
import qualified Persistence.Entries as Entries
import Data.Settings
import Global

import System.Environment

calcConfigDir :: Maybe Path -> ErrT IO Path
calcConfigDir mPathFromOptions =
	firstThatWorks $
		[ maybe (throwE $ "config dir not specified!") return $ mPathFromOptions
		, lookupConfigDirFromEnv
		, lift $ defConfigDir
		]
	where
		firstThatWorks :: [ErrT IO a] -> ErrT IO a
		firstThatWorks =
			foldl conc (throwE "no strategy to find config dir")
			where
				conc a b =
					do
						a' <-
							lift $ runExceptT a
						case a' of
							Left _ -> b
							Right x -> return x

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


lookupConfigDirFromEnv :: ErrT IO Path
lookupConfigDirFromEnv =
	ExceptT $
	liftM (
		(maybeToEither "not installed correctly")
		.
		fmap path_fromStr
		) $
	lookupEnv envVarConfigDir
{-
	let lookup = (do
		fromEnv <- lookupEnv envVarConfigDir
		def <- defConfigDir
		return $ fmap path_fromStr fromEnv `mplus` Just def) :: IO (Maybe Path)
	ExceptT $ liftM (maybeToEither "not installed correctly") lookup :: ErrT IO Path
-}
