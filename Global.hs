module Global(
	prgName, envVarConfigDir, defConfigDir,
	ErrT, ErrM, Path,
	lift2,

	-- * utilities:
	maybeToEither,
	mapLeft, mapToFstM,
	emptyStrToNothing,
	nothingToEmpty,
	module Control.Monad,
	module Control.Monad.Trans,
	module Control.Monad.Trans.Except,
	path_toStr, path_fromStr,
	Path.root,
	Path.directory,
	Path.parent,
	Path.filename,
	Path.dirname,
	Path.basename,
	Path.absolute,
	Path.relative,
	Path.extension,
	Path.dropExtension,
	(Path.</>), (Path.<.>),
	{-
	module Path
	module Filesystem.Path.CurrentOS,
	-}
) where

import Prelude hiding( FilePath )

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except

import System.Directory( getHomeDirectory )
import qualified Filesystem.Path as Path
import qualified Filesystem.Path.CurrentOS as Path
--import Control.Applicative hiding(many, (<|>))


prgName :: String
prgName = "sgcheck2"
envVarConfigDir :: String
envVarConfigDir = "SGCHECK2_CONFIGPATH"
defConfigDir :: IO Path
defConfigDir =
	liftM path_fromStr $
	liftM2 (++)
		getHomeDirectory
		(return "/.sgcheck2")

type ErrM a = Except String a
type ErrT t = ExceptT String t

type Path = Path.FilePath

path_toStr :: Path -> String
path_toStr = Path.encodeString
path_fromStr :: String -> Path
path_fromStr = Path.decodeString

{-
instance Monoid Path where
	mempty = Path.mempty
	mappend = Path.append
-}

maybeToEither :: l -> Maybe a -> Either l a
maybeToEither l mayb = case mayb of
	Nothing -> Left l
	Just x -> Right x

lift2 :: (MonadTrans t', MonadTrans t, Monad (t m), Monad m) => m a -> t' (t m) a
lift2 = lift . lift

mapToFstM :: Monad m => (a -> m c) -> (a,b) -> m (c,b)
mapToFstM f (a,b) =
	f a >>= \c -> return (c,b)

mapLeft :: (l -> l') -> Either l r -> Either l' r
mapLeft f x = case x of { Left l -> Left $ f l; Right r -> Right r }

emptyStrToNothing :: String -> Maybe String
emptyStrToNothing str = case str of
	"" -> Nothing
	_ -> Just str

nothingToEmpty :: Maybe String -> String
nothingToEmpty maybeStr = case maybeStr of
	Nothing -> ""
	Just str -> str
