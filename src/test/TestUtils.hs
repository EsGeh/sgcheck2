{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
module TestUtils(
	withTempDir,
	ValidSettings(..),
	--NonEmptyPath(..),
	ValidPath(..),
	catchExceptions,
	genValidIPString,
) where

import Data.Settings
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

newtype ValidPath = ValidPath{ getValidPath :: Path }
	deriving( Show, Eq, Ord )


instance Arbitrary ValidPath where
	arbitrary = ValidPath <$>
		(listOf $ arbitrary `suchThat` isAlphaNum `suchThat` (/='\n'))
		`suchThat` (not . null)
		--arbitrary `suchThat` Path.isValid

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


newtype ValidSettings = ValidSettings{ getValidSettings :: Settings } deriving( Show, Eq, Ord )

instance Arbitrary ValidSettings where
	arbitrary =
		fmap ValidSettings $
		Settings <$>
			arbitrary <*>
			arbitrary <*>
			(getValidPath <$> arbitrary) <*>
			(getValidPath <$> arbitrary)

{-
instance Arbitrary Settings where
	arbitrary =
		(uncurryN Settings) <$>
		(arbitrary :: Gen (Maybe IP, Maybe IP, Path, Path))
-}

{-
newtype NonEmptyPath = NonEmptyPath{ getNonEmptyPath :: Path } deriving( Show, Eq, Ord )

instance Arbitrary NonEmptyPath where
	arbitrary =
		NonEmptyPath <$> arbitrary `suchThat` (not . null)
-}

instance Arbitrary IP where
	arbitrary =
		fromMaybe (error "arbitrary ip generation error") . ip_fromStr <$>
		genValidIPString

---------------------------------
-- generators:
---------------------------------

genValidIPString :: Gen String
genValidIPString =
	do
		numbers <- (map getNonNegative) <$> vectorOf 4 arbitrary :: Gen [Int]
		return $
			intercalate "." $
			map show $
			numbers

{-
genValidPathString =
	(listOf $ arbitrary `suchThat` isAlphaNum)
-}
