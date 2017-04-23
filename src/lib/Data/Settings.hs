{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Settings(
	Settings(..), defSettings,
	settings_toStr, settings_fromStr,

	IP(), ip_fromStr, ip_toStr,

	ValidSettings(..),
	settings_isValid,
	genValidIPString
) where

import Utils()
--import System.FilePath as Path( (</>), (<.>) )
import qualified System.FilePath as Path

import Data.Yaml as Yaml
import Data.Aeson.Types as Yaml
import GHC.Generics
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.List

import Test.QuickCheck

type Path = Path.FilePath


-- settings
data Settings = Settings {
	serverIP :: Maybe IP,
	thisIP :: Maybe IP,
	serverPath :: Path,
	thisPath :: Path
}
	deriving( Show, Eq, Ord, Generic )

-- TODO: stronger guaranties:
newtype IP = IP { fromIP :: String }
	deriving( Show, Eq, Ord )

ip_fromStr :: String -> Maybe IP
ip_fromStr x =
	if x == "" then Nothing else Just $ IP x

ip_toStr :: IP -> String
ip_toStr = fromIP

defSettings :: Settings
defSettings = Settings {
	serverIP = Nothing,
	thisIP = Nothing,
	serverPath =  "",
	thisPath =  ""
}

settings_toStr :: Settings -> String
settings_toStr =
	BS.unpack . Yaml.encode

settings_fromStr :: String -> Either String Settings
settings_fromStr =
	Yaml.decodeEither . BS.pack

-- for testing:

settings_isValid :: Settings -> Bool
settings_isValid Settings{..} =
	and $
	[ not $ Path.hasTrailingPathSeparator serverPath
	, not $ Path.hasTrailingPathSeparator thisPath
	, Path.isValid serverPath
	, Path.isValid thisPath
	]

newtype ValidSettings = ValidSettings{ getValidSettings :: Settings } deriving( Show, Eq, Ord )

instance Arbitrary ValidSettings where
	arbitrary =
		fmap ValidSettings $
		arbitrary `suchThat` settings_isValid
		{-
		Settings <$>
			arbitrary <*>
			arbitrary <*>
			(getValidPath <$> arbitrary) <*>
			(getValidPath <$> arbitrary)
		-}

instance Arbitrary Settings where
	arbitrary =
		Settings
			<$> arbitrary
			<*> arbitrary
			<*> arbitrary
			<*> arbitrary

instance Arbitrary IP where
	arbitrary =
		fromMaybe (error "arbitrary ip generation error") . ip_fromStr <$>
		genValidIPString

genValidIPString :: Gen String
genValidIPString =
	do
		numbers <- (map getNonNegative) <$> vectorOf 4 arbitrary :: Gen [Int]
		return $
			intercalate "." $
			map show $
			numbers

instance Yaml.FromJSON Settings where
	parseJSON = Yaml.genericParseJSON encodingOptions

instance Yaml.ToJSON Settings where
	toJSON = Yaml.genericToJSON encodingOptions

instance Yaml.FromJSON IP where
	parseJSON x =
		parseJSON x >>= \str ->
			case ip_fromStr str of
				Nothing -> fail "invalid ip format"
				Just ip -> return ip
		--Yaml.genericParseJSON encodingOptions

instance Yaml.ToJSON IP where
	toJSON =
		toJSON . ip_toStr
		--Yaml.genericToJSON encodingOptions

encodingOptions :: Yaml.Options
encodingOptions = Yaml.defaultOptions
{-
	 fieldLabelModifier = drop 1 . dropWhile (/='_')
-}
