module Data_Test where

import Data.Settings
import Utils
import Utils.Path as Path( Path, (</>), (<.>) )
import qualified Utils.Path as Path
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
		pre (Path.path_isValid (Path.path_fromStr str))
		let path = Path.path_fromStr str
		pre $ Path.relative $ Path.path_fromStr str

		-- make shure we can create a file in this dir:
		run $ withTempDir $ \dir ->
			createDirectoryIfMissing True $ Path.path_toStr $
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
