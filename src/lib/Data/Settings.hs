module Data.Settings(
	Settings(..), defSettings,
	settings_toStr, settings_fromStr,
) where

import Utils

import Text.Parsec


-- settings
data Settings = Settings {
	serverIP :: Maybe String,
	thisIP :: Maybe String,
	serverPath :: Path,
	thisPath :: Path
}
	deriving( Show )

defSettings :: Settings
defSettings = Settings {
	serverIP = Nothing,
	thisIP = Nothing,
	serverPath = path_fromStr "",
	thisPath = path_fromStr ""
}

settings_toStr :: Settings -> String
settings_toStr settings =
	"serverIP=" ++ nothingToEmpty (serverIP settings) ++ "\n" ++
	"thisIP=" ++ nothingToEmpty (thisIP settings) ++ "\n" ++
	"serverPath=" ++ (path_toStr $ serverPath settings) ++ "\n" ++ 
	"thisPath=" ++ (path_toStr $ thisPath settings) ++ "\n"

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
		"serverIP" -> \def -> def{ serverIP = emptyStrToNothing v }
		"thisIP" -> \def -> def{ thisIP = emptyStrToNothing v }
		"serverPath" -> \def -> def{ serverPath = path_fromStr v }
		"thisPath" -> \def -> def{ thisPath = path_fromStr v }
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
