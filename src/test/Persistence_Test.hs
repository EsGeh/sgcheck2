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
	--forAll (getValidSettings <$> arbitrary) $ \settings ->
	--forAll (getValidPath <$> arbitrary) $ \path ->
	forAll (getValidEntry <$> arbitrary) $ \entry ->
	monadicIO $
	withTempDir $ \configDir ->
		do
			run $ catchExceptions $ writeHiddenFile configDir entry
			dirContent <- run $ listDirectory configDir
			assert $ dirContent == [ entry_pathOnThis entry ++ ".sgcheck2" ]

prop_testEntryPersistence =
	forAll (getValidSettings <$> arbitrary) $ \settings ->
	forAll (getValidEntry <$> arbitrary) $ \entry ->
	monadicIO $
	withTempDir $ \configDir ->
		do
			run $ catchExceptions $ writeHiddenFile configDir entry
			loadedEntry <- run $ catchExceptions $ loadHiddenFile configDir (entry_pathOnThis entry)
			assert $ loadedEntry == entry


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
