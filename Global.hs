module Global(
	prgName,
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

import Control.Applicative hiding(many, (<|>))
import qualified Filesystem.Path as Path
import qualified Filesystem.Path.CurrentOS as Path


prgName = "sgcheck2"

type ErrM a = Except String a
type ErrT t = ExceptT String t

type Path = Path.FilePath
path_toStr = Path.encodeString
path_fromStr = Path.decodeString

{-
instance Monoid Path where
	mempty = Path.mempty
	mappend = Path.append
-}

maybeToEither :: l -> Maybe a -> Either l a
maybeToEither l maybe = case maybe of
	Nothing -> Left l
	Just x -> Right x

lift2 :: (MonadTrans t', MonadTrans t, Monad (t m), Monad m) => m a -> t' (t m) a
lift2 = lift . lift

mapToFstM f (a,b) =
	f a >>= \c -> return (c,b)

mapLeft :: (l -> l') -> Either l r -> Either l' r
mapLeft f x = case x of { Left l -> Left $ f l; Right r -> Right r }

emptyStrToNothing str = case str of
	"" -> Nothing
	_ -> Just str

nothingToEmpty maybeStr = case maybeStr of
	Nothing -> ""
	Just str -> str
