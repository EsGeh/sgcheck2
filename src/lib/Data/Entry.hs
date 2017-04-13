{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Entry(
	Entry(..),
	entry_pathOnThis,

	ValidEntry(..),
	entry_isValid,
) where

import qualified System.FilePath as Path
import Data.Yaml as Yaml
import Data.Aeson as Yaml
import Data.Aeson.Types as Yaml
import GHC.Generics

import Test.QuickCheck

type Path = Path.FilePath


{-
 full paths:
 	entry_serverPath </> entry_pathOnServer
 	entry_thisPath </> entry_pathOnThis
-}

data Entry
	= Entry {
		entry_pathOnServer :: Path,

		-- mirror the settings for sanity checking:
		entry_serverPath :: Path,
		entry_thisPath :: Path
	}
	deriving( Show, Eq, Ord, Read, Generic )

entry_pathOnThis :: Entry -> Path
entry_pathOnThis =
	Path.takeFileName . entry_pathOnServer

{-
entry_toText :: Entry -> String
entry_toText = entry_path
-}


instance Yaml.FromJSON Entry where
	parseJSON = Yaml.genericParseJSON encodingOptions

instance Yaml.ToJSON Entry where
	toJSON = Yaml.genericToJSON encodingOptions
	--toEncoding = Yaml.genericToEncoding encodingOptions

encodingOptions :: Yaml.Options
encodingOptions = Yaml.defaultOptions{
	fieldLabelModifier = drop 1 . dropWhile (/='_')
}

-- for testing:

entry_isValid Entry{..} =
	and $
	[ Path.isRelative entry_pathOnServer
	, not $ Path.hasTrailingPathSeparator entry_pathOnServer
	, not $ Path.hasTrailingPathSeparator entry_serverPath
	, not $ Path.hasTrailingPathSeparator entry_thisPath
	, Path.isValid entry_pathOnServer
	, Path.isValid entry_thisPath
	, Path.isValid entry_serverPath
	]

newtype ValidEntry = ValidEntry{ getValidEntry :: Entry }
	deriving( Show, Eq, Read, Generic )

instance Arbitrary ValidEntry where
	arbitrary =
		ValidEntry <$>
		arbitrary `suchThat` entry_isValid

instance Arbitrary Entry where
	arbitrary =
		do
			entry_serverPath <- arbitrary
			entry_thisPath <- arbitrary
			entry_pathOnServer <- arbitrary
			return Entry{..}
