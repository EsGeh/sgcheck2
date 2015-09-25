module Data(
	Data(..),
	CommandArgs(..), Command(..), CommandType(..), Settings(..),
	HasCommmandType(..),
	cmdType_fromStr, cmdType_toStr,
	cmdType_listAll,
	settingsToString, settingsFromString,
	defSettings
) where

import Global
import Text.Parsec
import Data.Tuple( swap )


data Data =
	Data {
		data_programInput :: CommandArgs,
		data_storeSettings :: Settings -> ErrT IO ()
	}

instance Show Data where
	show d =
		Prelude.concat $
		[ "Data{"
		, " data_programInput=" ++ show (data_programInput d)
		, " }"
		]

data CommandArgs =
	CommandArgs {
		cmdArgs_cmd :: Command,
		--cc_params :: Maybe Parameters
		cmdArgs_settings :: Settings,
		cmdArgs_memorizeFile ::
			Settings
			-> Path -- src
			-> Path -- dest
			-> ErrT IO (),
		cmdArgs_lookupFile ::
			Path -> ErrT IO Path 
	}

instance Show CommandArgs where
	show c =
		Prelude.concat $
		[ "CommandArgs{"
		, " cmdArgs_cmd=" ++ show (cmdArgs_cmd c)
		, " cmdArgs_settings=" ++ show (cmdArgs_settings c)
		, " }"
		]

data Command
	= CmdOut Path
	| CmdIn Path
	| CmdListFiles
	| CmdShowConfig
	| CmdWriteConfig
	deriving( Show )

data CommandType
	= Out
	| In
	| ListFiles
	| ShowConfig
	| WriteConfig
	deriving( Eq, Ord, Enum, Bounded, Show )

{-
data CmdDescr
	= CmdDescr {
	}
-}

cmdType_listAll = [(minBound :: CommandType) .. ]

class HasCommmandType a where
	cmd_type :: a -> CommandType

instance HasCommmandType Command where
	cmd_type cmd =
		case cmd of
			CmdOut _ -> Out
			CmdIn _ -> In
			CmdListFiles -> ListFiles
			CmdShowConfig -> ShowConfig
			CmdWriteConfig -> WriteConfig

cmdType_fromStr str =
	lookup str cmdList

cmdType_toStr c =
	maybe (error "cmdType_fromStr error!") id $
	lookup c $ map swap cmdList

--cmdType_listInfo =

cmdList =
	[ ("out", Out)
	, ("in", In)
	, ("list", ListFiles)
	, ("showConfig", ShowConfig)
	, ("writeConfig", WriteConfig)
	]

-- settings
data Settings = Settings {
	serverIP :: Maybe String,
	thisIP :: Maybe String,
	serverPath :: Path,
	thisPath :: Path
}
	deriving( Show )

defSettings = Settings {
	serverIP = Nothing,
	thisIP = Nothing,
	serverPath = path_fromStr "",
	thisPath = path_fromStr ""
}

settingsToString settings =
	"serverIP=" ++ nothingToEmpty (serverIP settings) ++ "\n" ++
	"thisIP=" ++ nothingToEmpty (thisIP settings) ++ "\n" ++
	"serverPath=" ++ (path_toStr $ serverPath settings) ++ "\n" ++ 
	"thisPath=" ++ (path_toStr $ thisPath settings) ++ "\n"

settingsFromString str = settingsChangeFromString str <*> return defSettings 


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
	char '='
	value <- many $ noneOf "\n"
	return (key,value)
