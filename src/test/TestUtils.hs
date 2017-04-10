{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
module TestUtils(
	module TestUtils,
	--Path, (</>), (<.>),
	--module Dir,
) where

import Data.Settings
import Utils
import Utils.Path as Path( Path, (</>), (<.>) )
import qualified Utils.Path as Path
import qualified TestUtils.Dir as Dir

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


withTempDir :: MonadIO m => (Path -> m a) -> m a
withTempDir f =
	(liftIO getTemporaryDirectory) >>= \tempDir ->
		do
			path <- liftIO (createTempDirectory tempDir tempDirTemplate)
			ret <- f (Path.path_fromStr path)
			liftIO $ ignoringIOErrors $ removeDirectoryRecursive path
			return $ ret
	{-
	bracket
		(createTempDirectory tempDir tempDirTemplate)
    (ignoringIOErrors . removeDirectoryRecursive)
		(f . Path.path_fromStr)
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
		NonEmptyPath <$> arbitrary `suchThat` (not . Path.path_isEmpty)

instance Arbitrary Path where
	arbitrary =
		Path.path_fromStr <$>
		genValidPathString
		-- Path.path_fromStr <$> arbitrary

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
Path.path_noNewlines =
	takeWhile (/='\n') <$> arbitrary
-}
