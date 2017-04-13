{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module TestUtils(
	withTempDir,
	--NonEmptyPath(..),
	ValidPath(..),
	catchExceptions,
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

tempDirTemplate = "sgcheck2_configDir"

type Path = Path.FilePath

withTempDir :: MonadIO m => (Path -> m a) -> m a
withTempDir f =
	(liftIO getTemporaryDirectory) >>= \tempDir ->
		do
			path <- liftIO (createTempDirectory tempDir tempDirTemplate)
			ret <- f path
			liftIO $ ignoringIOErrors $ removeDirectoryRecursive path
			return $ ret
	{-
	bracket
		(createTempDirectory tempDir tempDirTemplate)
    (ignoringIOErrors . removeDirectoryRecursive)
		f
	-}
	where
		ignoringIOErrors = (`catch` (\e -> const (return ()) (e :: IOError)))

catchExceptions m =
	runExceptT m >>= \case
		Left err -> error err
		Right x -> return x


{-
newtype NonEmptyPath = NonEmptyPath{ getNonEmptyPath :: Path } deriving( Show, Eq, Ord )

instance Arbitrary NonEmptyPath where
	arbitrary =
		NonEmptyPath <$> arbitrary `suchThat` (not . null)
-}

{-
genValidPathString =
	(listOf $ arbitrary `suchThat` isAlphaNum)
-}
