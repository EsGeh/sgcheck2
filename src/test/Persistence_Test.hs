module Persistence_Test where

import Persistence.Entries
import Persistence.Settings
import Data.Entry
import Data.Settings
import Utils
import Utils.Path as Path( Path, (</>), (<.>) )
import qualified Utils.Path as Path
import TestUtils

import Test.QuickCheck
import Test.QuickCheck.Monadic

import System.Directory( getTemporaryDirectory, removeDirectoryRecursive, listDirectory )

import Data.Char


---------------------------------
-- Entry:
---------------------------------

prop_testStoreEntry settings path =
	monadicIO $
	withTempDir $ \configDir ->
		do
			run $ catchExceptions $ writeHiddenFile configDir settings path undefined
			dirContent <- run $ listDirectory (Path.path_toStr configDir)
			assert $ dirContent == [ Path.path_toStr path ++ ".sgcheck2" ]

prop_testEntryPersistence settings path =
	monadicIO $
	withTempDir $ \configDir ->
		do
			run $ catchExceptions $ writeHiddenFile configDir settings path undefined
			loadedEntry <- run $ catchExceptions $ loadHiddenFile configDir path
			assert $ entry_path loadedEntry == path


---------------------------------
-- Settings:
---------------------------------

prop_testStoreSettings settings =
	monadicIO $
	withTempDir $ \configDir ->
		do
			run $ catchExceptions $ storeSettings configDir settings
			dirContent <- run $ listDirectory (Path.path_toStr configDir)
			assert $ dirContent == [ "config" ]

prop_testSettingsPersist settings =
	monadicIO $
	withTempDir $ \configDir ->
		do
			run $ catchExceptions $
				storeSettings configDir settings
			loadedSettings <- run $ catchExceptions $
				loadSettings configDir
			assert $ loadedSettings == settings

---------------------------------
-- createConfig:
---------------------------------

prop_createConfig =
	monadicIO $
	withTempDir $ \configDir ->
		do
			run $ catchExceptions $ createConfig configDir

			-- correct directory structure?
			dirContent <- run $ listDirectory (Path.path_toStr configDir)
			assert $ dirContent == [ "config" ]

			-- empty settings created?
			loadedSettings <- run $ catchExceptions $
				loadSettings configDir
			assert $ loadedSettings == defSettings
