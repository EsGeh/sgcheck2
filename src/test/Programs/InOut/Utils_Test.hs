{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Programs.InOut.Utils_Test where

import Programs.InOut.Utils
import Utils
import Data.Settings
import TestUtils

import Test.QuickCheck
import Test.Hspec

import Data.Tuple.Curry( uncurryN )
import Data.List( intercalate )


prop_outParams options path =
	forAll (getValidSettings <$> arbitrary) $ \settings ->
	(not $ path_isEmpty path) ==>
		let
			CopyFileParams{..} = outParams settings options path
			dest = 
				thisPath settings </> filename path
		in
			copyParams_cmd ===
				("rsync"
				, options ++ [
					path_toStr $ serverPath settings </> path,
					path_toStr $
						directory $ dest
				])
			{-
			.&&.
			-- too difficult to test:
			copyParams_fullCommand ===
			-}
			.&&.
			copyParams_src === serverPath settings </> path
			.&&.
			copyParams_dest === dest

prop_inParams options path =
	forAll (getValidSettings <$> arbitrary) $ \settings ->
	(not $ path_isEmpty path) ==>
		let
			CopyFileParams{..} = inParams settings options path
			dest = 
				serverPath settings </> filename path
		in
			copyParams_cmd ===
				("rsync"
				, options ++ [
					path_toStr $ thisPath settings </> path,
					path_toStr $
						directory $ dest
				])
			{-
			.&&.
			-- too difficult to test:
			copyParams_fullCommand ===
			-}
			.&&.
			copyParams_src === thisPath settings </> path
			.&&.
			copyParams_dest === dest

{-
-- prop_dirAndFilename =
	forAll (getNonEmptyPath <$> arbitrary) $ \dir ->
	forAll (getNonEmptyPath <$> arbitrary) $ \path ->
	(directory $ dir </> filename path) === dir </> path_fromStr ""
-}
