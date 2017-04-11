module Data_Test where

import Data.Settings
import Utils
import TestUtils

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic

import System.Directory( createDirectoryIfMissing )

import System.FilePath as Path( (</>), (<.>) )
import qualified System.FilePath as Path

import Data.Maybe
import Control.Applicative

type Path = Path.FilePath


---------------------------------
-- Path:
---------------------------------

prop_assertPathsAreValid str =
	monadicIO $
	do
		pre (Path.isValid str)
		let path = str
		pre $ Path.isRelative $ str

		-- make shure we can create a file in this dir:
		liftIO $ putStrLn $ "trying to create dir: " ++ str
		run $ withTempDir $ \dir ->
			createDirectoryIfMissing True $
				dir </> path

---------------------------------
-- IP:
---------------------------------

prop_ipSerialisation =
	forAll genValidIPString $ \str ->
	Just str === (ip_toStr <$> ip_fromStr str)

prop_ipNonEmpty =
	ip_fromStr "" == Nothing

---------------------------------
-- Settings:
---------------------------------

prop_settingsSerialisation =
	forAll (getValidSettings <$> arbitrary) $ \x ->
	Right x == (settings_fromStr . settings_toStr) x
