{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Entry(
	Entry(..),
	entry_pathOnThis,

	EntrySavedInfo(..),

	entry_toSavedInfo, entry_fromSavedInfo,

	ValidEntry(..),
	entry_isValid,

	ValidEntrySavedInfo(..),
	entrySavedInfo_isValid,
) where

import Data.Settings

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

data EntrySavedInfo
	= EntrySavedInfo {
		entrySavedInfo_pathOnServer :: Path,
		entrySavedInfo_excludePattern :: [Path],
		entrySavedInfo_execBeforeOut :: Maybe Path,
		entrySavedInfo_execBeforeIn :: Maybe Path
	}
	deriving( Show, Eq, Ord, Read, Generic )

data Entry
	= Entry {
		entry_pathOnServer :: Path,

		-- mirror the settings for sanity checking:
		entry_serverPath :: Path,
		entry_thisPath :: Path,
		entry_excludePattern :: [Path],
		entry_execBeforeOut :: Maybe Path,
		entry_execBeforeIn :: Maybe Path
	}
	deriving( Show, Eq, Ord, Read, Generic )

entry_toSavedInfo e =
	EntrySavedInfo{
		entrySavedInfo_pathOnServer = entry_pathOnServer e,
		entrySavedInfo_excludePattern = entry_excludePattern e,
		entrySavedInfo_execBeforeOut = entry_execBeforeOut e,
		entrySavedInfo_execBeforeIn = entry_execBeforeIn e
	}

entry_fromSavedInfo settings EntrySavedInfo{..} =
	Entry{
		entry_pathOnServer = entrySavedInfo_pathOnServer,
		entry_serverPath = serverPath settings,
		entry_thisPath = thisPath settings,
		entry_excludePattern = entrySavedInfo_excludePattern,
		entry_execBeforeOut = entrySavedInfo_execBeforeOut,
		entry_execBeforeIn = entrySavedInfo_execBeforeIn
	}

entry_pathOnThis :: Entry -> Path
entry_pathOnThis =
	Path.takeFileName . entry_pathOnServer

{-
entry_toText :: Entry -> String
entry_toText = entry_path
-}


instance Yaml.FromJSON EntrySavedInfo where
	parseJSON = Yaml.genericParseJSON encodingOptions
			--Yaml.omitNothingFields = True

instance Yaml.ToJSON EntrySavedInfo where
	toJSON = Yaml.genericToJSON encodingOptions
	--toEncoding = Yaml.genericToEncoding encodingOptions

encodingOptions :: Yaml.Options
encodingOptions = Yaml.defaultOptions{
	fieldLabelModifier = drop 1 . dropWhile (/='_')
}

-- for testing:

entrySavedInfo_isValid EntrySavedInfo{..} =
	and $
	[ Path.isRelative entrySavedInfo_pathOnServer
	, not $ Path.hasTrailingPathSeparator entrySavedInfo_pathOnServer
	, Path.isValid entrySavedInfo_pathOnServer
	]

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

newtype ValidEntrySavedInfo = ValidEntrySavedInfo{ getValidEntrySavedInfo :: EntrySavedInfo }
	deriving( Show, Eq, Read, Generic )

instance Arbitrary ValidEntrySavedInfo where
	arbitrary =
		ValidEntrySavedInfo <$>
		arbitrary `suchThat` entrySavedInfo_isValid

instance Arbitrary EntrySavedInfo where
	arbitrary =
		EntrySavedInfo <$> arbitrary <*> pure [] <*> pure Nothing <*> pure Nothing

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
			let
				entry_excludePattern = []
				entry_execBeforeOut = Nothing
				entry_execBeforeIn = Nothing
			return Entry{..}
