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

tempDirTemplate = "sgcheck2_configDir"


withTempDir f =
	withSystemTempDirectory tempDirTemplate $ \configDir -> f $ path_fromStr configDir

catchExceptions m =
	runExceptT m >>= \case
		Left err -> error err
		Right x -> return x


instance Arbitrary Settings where
	arbitrary =
		(uncurryN Settings) <$>
		(arbitrary :: Gen (Maybe IP, Maybe IP, Path, Path))

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
