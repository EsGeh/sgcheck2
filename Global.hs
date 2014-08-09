module Global(
	Parameters(..), Settings(..), defSettings, 
	ErrT, Path,
	lift2,
	settingsToString, settingsFromString,
	settingsChangeFromString,
	parseTransformations,
	parseKeyValue,

	-- * utilities:
	maybeToEither,
	module Control.Monad,
	module Control.Monad.Trans,
	module Control.Monad.Trans.Except,
	module Filesystem.Path,
	module Filesystem.Path.CurrentOS,
) where

import Prelude hiding( FilePath )

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Text.Parsec
import Control.Applicative hiding(many, (<|>))
import Filesystem.Path
import Filesystem.Path.CurrentOS


data Parameters = Parameters {
  file :: FilePath
	{-
	src :: String,
	dest :: String
	-}
}
	deriving( Show )

data Settings = Settings {
	serverIP :: Maybe String,
	thisIP :: Maybe String,
	serverPath :: FilePath,
	thisPath :: FilePath
	--configDir :: String
}
	deriving( Show )

defSettings = Settings {
	serverIP = Nothing,
	thisIP = Nothing,
	serverPath = decodeString "",
	thisPath = decodeString ""
	--configDir = defConfigDir
}


type ErrT t = ExceptT String t

type Path = FilePath

maybeToEither :: l -> Maybe a -> Either l a
maybeToEither l maybe = case maybe of
	Nothing -> Left l
	Just x -> Right x

settingsToString settings =
	"serverIP=" ++ nothingToEmpty (serverIP settings) ++ "\n" ++
	"thisIP=" ++ nothingToEmpty (thisIP settings) ++ "\n" ++
	"serverPath=" ++ (encodeString $ serverPath settings) ++ "\n" ++ 
	"thisPath=" ++ (encodeString $ thisPath settings) ++ "\n"

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
		"serverPath" -> \def -> def{ serverPath = decodeString v }
		"thisPath" -> \def -> def{ thisPath = decodeString v }
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

lift2 :: (MonadTrans t', MonadTrans t, Monad (t m), Monad m) => m a -> t' (t m) a
lift2 = lift . lift

mapLeft :: (l -> l') -> Either l r -> Either l' r
mapLeft f x = case x of { Left l -> Left $ f l; Right r -> Right r }

emptyStrToNothing str = case str of
	"" -> Nothing
	_ -> Just str

nothingToEmpty maybeStr = case maybeStr of
	Nothing -> ""
	Just str -> str
