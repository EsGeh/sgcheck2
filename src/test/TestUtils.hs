{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TestUtils(
	withTempDir,
	--NonEmptyPath(..),
	ValidPath(..),
	catchErrorsInTest,
) where

import Data.Settings
import Data.Entry
import Utils
import qualified TestUtils.Dir as Dir

import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic

import System.IO.Temp
import Data.Tuple.Curry( uncurryN )
import Data.Maybe
import Data.List
import Data.Char
import System.Directory( getTemporaryDirectory, removeDirectoryRecursive )

import System.FilePath as Path( (</>), (<.>) )
import qualified System.FilePath as Path

import Control.Exception( catch, bracket )
import Control.Concurrent

tempDirTemplate = "sgcheck2_temp"

type Path = Path.FilePath

withTempDir :: MonadIO m => (Path -> m a) -> m a
withTempDir f =
	(liftIO getTemporaryDirectory) >>= \tempDir ->
		do
			path <- liftIO (createTempDirectory tempDir tempDirTemplate)
			--liftIO $ threadDelay 100000
			ret <- f path
			--liftIO $ threadDelay 100000
			liftIO $ removeDirectoryRecursive path
			return $ ret
	{-
	bracket
		(createTempDirectory tempDir tempDirTemplate)
    (ignoringIOErrors . removeDirectoryRecursive)
		f
	-}
	where
		ignoringIOErrors x = catch x $ \(e :: IOError) ->
			return ()
			--const (return ()) (e :: IOError)

catchErrorsInTest ::
	Monad m =>
	ErrT m a -> m a
catchErrorsInTest m =
	runExceptT m >>= \case
		Left err -> fail err
		Right x -> return x
