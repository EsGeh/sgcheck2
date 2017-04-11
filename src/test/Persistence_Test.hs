module Persistence_Test where

import Persistence.Entries
import Persistence.Settings
import Data.Entry
import Data.Settings
import Utils
import TestUtils

import Test.QuickCheck
import Test.QuickCheck.Monadic

import System.Directory( getTemporaryDirectory, removeDirectoryRecursive, listDirectory )

import System.FilePath as Path( (</>), (<.>) )
import qualified System.FilePath as Path

import Data.Char

type Path = Path.FilePath


---------------------------------
-- Entry:
---------------------------------

prop_testStoreEntry =
	forAll (getValidSettings <$> arbitrary) $ \settings ->
	forAll (getValidPath <$> arbitrary) $ \path ->
	monadicIO $
	withTempDir $ \configDir ->
		do
			run $ catchExceptions $ writeHiddenFile configDir settings path undefined
			dirContent <- run $ listDirectory configDir
			assert $ dirContent == [ path ++ ".sgcheck2" ]

prop_testEntryPersistence =
	forAll (getValidSettings <$> arbitrary) $ \settings ->
	forAll (getValidPath <$> arbitrary) $ \path ->
	monadicIO $
	withTempDir $ \configDir ->
		do
			run $ catchExceptions $ writeHiddenFile configDir settings path undefined
			loadedEntry <- run $ catchExceptions $ loadHiddenFile configDir path
			assert $ entry_path loadedEntry == path


---------------------------------
-- Settings:
---------------------------------

prop_testStoreSettings =
	forAll (getValidSettings <$> arbitrary) $ \settings ->
	monadicIO $
	withTempDir $ \configDir ->
		do
			run $ catchExceptions $ storeSettings configDir settings
			dirContent <- run $ listDirectory configDir
			assert $ dirContent == [ "config" ]

prop_testSettingsPersist =
	forAll (getValidSettings <$> arbitrary) $ \settings ->
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
			dirContent <- run $ listDirectory configDir
			assert $ dirContent == [ "config" ]

			-- empty settings created?
			loadedSettings <- run $ catchExceptions $
				loadSettings configDir
			assert $ loadedSettings == defSettings
