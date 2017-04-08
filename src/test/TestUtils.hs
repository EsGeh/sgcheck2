{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
module TestUtils where

import Data.Settings
import Utils

import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic

import System.IO.Temp
import Data.Tuple.Curry( uncurryN )
import Data.Maybe
import Data.List
import Data.Char
import System.Directory( getTemporaryDirectory, removeDirectoryRecursive )

import Control.Exception( catch, bracket )

tempDirTemplate = "sgcheck2_configDir"


{-
-- withTempDir :: Int
withTempDir f =
	withSystemTempDirectory tempDirTemplate $ \configDir -> f $ path_fromStr configDir
-}

--withTempDir :: MonadIO m => (m a -> IO a) -> (Path -> m a) -> m a
-- withTempDir runMonad f =

withTempDir :: MonadIO m => (Path -> m a) -> m a
--withTempDir :: (Path -> IO a) -> IO a
withTempDir f =
	(liftIO getTemporaryDirectory) >>= \tempDir ->
		do
			path <- liftIO (createTempDirectory tempDir tempDirTemplate)
			ret <- f (path_fromStr path)
			liftIO $ ignoringIOErrors $ removeDirectoryRecursive path
			return $ ret
	{-
	bracket
		(createTempDirectory tempDir tempDirTemplate)
    (ignoringIOErrors . removeDirectoryRecursive)
		(f . path_fromStr)
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
			(getNonEmptyPath <$> arbitrary) <*>
			(getNonEmptyPath <$> arbitrary)

instance Arbitrary Settings where
	arbitrary =
		(uncurryN Settings) <$>
		(arbitrary :: Gen (Maybe IP, Maybe IP, Path, Path))

newtype NonEmptyPath = NonEmptyPath{ getNonEmptyPath :: Path } deriving( Show, Eq, Ord )

instance Arbitrary NonEmptyPath where
	arbitrary =
		NonEmptyPath <$> arbitrary `suchThat` (not . path_isEmpty)

instance Arbitrary Path where
	arbitrary =
		path_fromStr <$>
		genValidPathString
		-- path_fromStr <$> arbitrary

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

genValidPathString =
	(listOf $ arbitrary `suchThat` isAlphaNum)

{-
path_noNewlines =
	takeWhile (/='\n') <$> arbitrary
-}
