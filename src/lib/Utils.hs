{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Utils(
	ErrT, ErrM,

	lift2,
	maybeToEither,
	mapLeft,
	mapToFstM, mapToSndM,
	mapToFst, mapToSnd,
	emptyStrToNothing,
	nothingToEmpty,
	module Control.Monad,
	module Control.Monad.Trans,
	module Control.Monad.Trans.Except,

	--envVarConfigDir,
	--defConfigDir,
	Path,
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
) where

import Prelude hiding( FilePath )

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Identity

import qualified Filesystem.Path as Path
import qualified Filesystem.Path.CurrentOS as Path


type ErrM a = Except String a
type ErrT t = ExceptT String t

type Path = Path.FilePath

path_toStr :: Path -> String
path_toStr = Path.encodeString
path_fromStr :: String -> Path
path_fromStr = Path.decodeString

maybeToEither :: l -> Maybe a -> Either l a
maybeToEither l mayb = case mayb of
	Nothing -> Left l
	Just x -> Right x

lift2 :: (MonadTrans t', MonadTrans t, Monad (t m), Monad m) => m a -> t' (t m) a
lift2 = lift . lift

mapToFst f = runIdentity . mapToFstM (return . f)
mapToSnd f = runIdentity . mapToSndM (return . f)

mapToFstM f (a,b) =
	f a >>= \c -> return (c,b)

mapToSndM f (a,b) =
	f b >>= \c -> return (a,c)

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
