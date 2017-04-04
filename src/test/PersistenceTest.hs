module PersistenceTest where

import Persistence.Entries
import Data.Entry
import Utils
import TestUtils

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Data.Char


prop_entryPersistence settings path =
	monadicIO $
	(run $ withTempDir $ catchExceptions . writeAndLoad) >>= \loadedEntry ->
	assert $ entry_path loadedEntry == path
		where
			writeAndLoad :: Path -> ErrT IO Entry
			writeAndLoad configDir =
				do
					-- lift $ putStrLn $ show configDir
					writeHiddenFile configDir settings path undefined
					loadHiddenFile configDir path
