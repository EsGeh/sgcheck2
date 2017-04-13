{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Utils(
	ErrT, ErrM,
	errT,

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

	ValidPath(..),
	path_isValid
) where

import Prelude hiding( FilePath )

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Identity
import Data.Char
import qualified System.FilePath as Path

import Test.QuickCheck

type Path = Path.FilePath


type ErrM a = Except String a
type ErrT t = ExceptT String t

errT = ExceptT

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


newtype ValidPath = ValidPath{ getValidPath :: Path }
	deriving( Show, Eq, Ord )

path_isValid path =
	(not $ null path)
	&&
	Path.isValid path
	-- be more restrictive:
	&&
	all (/='\0') path
	&&
	all isAscii path
	&&
	all isAlphaNum path
	&&
	all (/='\n') path

instance Arbitrary ValidPath where
	arbitrary =
		ValidPath <$>
		arbitrary `suchThat` path_isValid `suchThat` ((<30) . length)
		{-
		ValidPath <$>
		(listOf $ arbitrary `suchThat` isAlphaNum `suchThat` (/='\n'))
		`suchThat` (not . null)
		--arbitrary `suchThat` Path.isValid
		-}
