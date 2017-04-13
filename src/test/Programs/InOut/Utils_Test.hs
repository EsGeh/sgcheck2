{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Programs.InOut.Utils_Test where

import Programs.InOut.Utils
import Utils
import Data.Settings
import Data.Entry
import TestUtils

import Test.QuickCheck
import Test.Hspec

import Data.Tuple.Curry( uncurryN )
import Data.List( intercalate )

import System.FilePath as Path( (</>), (<.>) )
import qualified System.FilePath as Path

type Path = Path.FilePath


prop_entryFromPathOnServer =
	forAll (getValidSettings <$> arbitrary) $ \settings ->
	forAll (getValidPath <$> arbitrary) $ \path ->
	forAll arbitrary $ \(options :: [String]) ->
		let
			entry = entryFromPathOnServer settings path
		in
			(Path.isRelative $ entry_pathOnThis entry)
			.&&.
			(not $ Path.hasTrailingPathSeparator $ entry_pathOnThis entry)

{-
-- prop_outParams options path =
	forAll (getValidSettings <$> arbitrary) $ \settings ->
	(not $ null path) ==>
		let
			CopyFileParams{..} = outParams settings options path
			dest =
				thisPath settings </> Path.takeFileName path
		in
			copyParams_cmd ===
				("rsync"
				, options ++ [
					serverPath settings </> path,
						Path.takeDirectory $ dest
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
-}

{-
-- prop_dirAndFilename =
	forAll (getNonEmptyPath <$> arbitrary) $ \dir ->
	forAll (getNonEmptyPath <$> arbitrary) $ \path ->
	(directory $ dir </> filename path) === dir </> ""
-}
