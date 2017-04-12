{-# LANGUAGE DeriveGeneric #-}
module Data.Settings(
	Settings(..), defSettings,
	settings_toStr, settings_fromStr,
	IP(), ip_fromStr, ip_toStr,
) where

import Utils
--import System.FilePath as Path( (</>), (<.>) )
import qualified System.FilePath as Path

import Data.Yaml as Yaml
--import Data.Aeson as Yaml
import Data.Aeson.Types as Yaml
import GHC.Generics
import qualified Data.ByteString.Char8 as BS

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
