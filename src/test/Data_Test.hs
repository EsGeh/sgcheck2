module Data_Test where

import Data.Settings
import Utils
import TestUtils

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic

import Control.Applicative
import System.Directory( createDirectoryIfMissing )
import Data.Maybe


---------------------------------
-- Path:
---------------------------------

prop_assertPathsAreValid str =
	monadicIO $
	do
		pre (path_isValid (path_fromStr str))
		let path = path_fromStr str
		pre $ relative $ path_fromStr str

		-- make shure we can create a file in this dir:
		run $ withTempDir $ \dir ->
			createDirectoryIfMissing True $ path_toStr $
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

prop_settingsSerialisation x =
	Right x == (settings_fromStr . settings_toStr) x
