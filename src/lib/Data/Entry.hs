{-# LANGUAGE DeriveGeneric #-}
module Data.Entry(
	Entry(..),
	entry_pathOnThis,
) where

import qualified System.FilePath as Path
import Data.Yaml as Yaml
import Data.Aeson as Yaml
import Data.Aeson.Types as Yaml
import GHC.Generics

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
	deriving( Show, Read, Generic )

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
