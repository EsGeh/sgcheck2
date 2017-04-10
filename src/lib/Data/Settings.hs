module Data.Settings(
	Settings(..), defSettings,
	settings_toStr, settings_fromStr,
	IP(), ip_fromStr, ip_toStr,
) where

import Utils
import qualified Utils.Path as Path

import Text.Parsec

type Path = Path.Path


-- settings
data Settings = Settings {
	serverIP :: Maybe IP,
	thisIP :: Maybe IP,
	serverPath :: Path,
	thisPath :: Path
}
	deriving( Show, Eq, Ord )

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
	serverPath = Path.path_fromStr "",
	thisPath = Path.path_fromStr ""
}

settings_toStr :: Settings -> String
settings_toStr settings =
	"serverIP=" ++ (nothingToEmpty . fmap ip_toStr . serverIP) settings ++ "\n" ++
	"thisIP=" ++ (nothingToEmpty . fmap ip_toStr . thisIP) settings ++ "\n" ++
	"serverPath=" ++ (Path.path_toStr $ serverPath settings) ++ "\n" ++ 
	"thisPath=" ++ (Path.path_toStr $ thisPath settings) ++ "\n"

settings_fromStr :: String -> Either String Settings
settings_fromStr str = settingsChangeFromString str <*> return defSettings 


settingsChangeFromString :: String -> Either String (Settings -> Settings)
settingsChangeFromString str =
	mapLeft show $
		parse parseKeyValue "" str
		>>=
		parse (parseTransformations parseSettingsTransform) ""

parseTransformations :: (Show k, Show v) => Parsec [(k,v)] () (a -> a)  -> Parsec [(k,v)] () (a -> a)
parseTransformations parseSingleTransform = chainl parseSingleTransform (return (.)) id

parseSettingsTransform :: Parsec [(String,String)] () (Settings -> Settings)
parseSettingsTransform = do
	(k,v) <- anyToken
	return $ case k of
		"serverIP" -> \def -> def{ serverIP = ip_fromStr v }
		"thisIP" -> \def -> def{ thisIP = ip_fromStr v }
		"serverPath" -> \def -> def{ serverPath = Path.path_fromStr v }
		"thisPath" -> \def -> def{ thisPath = Path.path_fromStr v }
		_ -> fail "unknown key!"


parseKeyValue :: Parsec String () [(String,String)]
parseKeyValue = do
	kvList <- parseKV `sepEndBy` newline
	--many newline
	eof
	return kvList
	--`endBy` many (char '\n' <|> char ' ')

parseKV :: Parsec String () (String,String)
parseKV = do
	spaces
	key <- many $ noneOf "="
	_ <- char '='
	value <- many $ noneOf "\n"
	return (key,value)
