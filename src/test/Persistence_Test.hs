module Persistence_Test where

import Persistence.Entries
import Persistence.Settings
import Data.Entry
import Data.Settings
import Utils
import TestUtils
import qualified TestUtils.Dir as Dir

import Test.QuickCheck
import Test.QuickCheck.Monadic

import System.Directory( getTemporaryDirectory, removeDirectoryRecursive, listDirectory )

import System.FilePath as Path( (</>), (<.>) )
import qualified System.FilePath as Path

import Data.Char
import Data.List

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
			run $ catchErrorsInTest $ writeHiddenFile configDir entry
			dirContent <- run $ listDirectory configDir
			assert $ dirContent == [ entry_pathOnThis entry ++ ".sgcheck2" ]

prop_testEntryPersistence =
	forAll (getValidSettings <$> arbitrary) $ \settings ->
	forAll (getValidEntrySavedInfo <$> arbitrary) $ \entrySaved ->
	let entry = entry_fromSavedInfo settings entrySaved
	in
	monadicIO $
	withTempDir $ \configDir ->
		do
			run $ catchErrorsInTest $ writeHiddenFile configDir $ entry
			loadedEntry <- run $ catchErrorsInTest $ loadHiddenFile settings configDir (entry_pathOnThis entry)
			--liftIO $ putStrLn $ "entry: " ++ show entry
			--liftIO $ putStrLn $ "loadedEntry: " ++ show loadedEntry
			assert $ loadedEntry == entry


---------------------------------
-- Settings:
---------------------------------

prop_testStoreSettings =
	forAll (getValidSettings <$> arbitrary) $ \settings ->
	monadicIO $
	withTempDir $ \configDir ->
		do
			run $ catchErrorsInTest $ storeSettings configDir settings
			dirContent <- run $ listDirectory configDir
			assert $ dirContent == [ "config" ]

prop_testSettingsPersist =
	forAll (getValidSettings <$> arbitrary) $ \settings ->
	monadicIO $
	withTempDir $ \configDir ->
		do
			run $ catchErrorsInTest $
				storeSettings configDir settings
			loadedSettings <- run $ catchErrorsInTest $
				loadSettings configDir
			assert $ loadedSettings == settings

---------------------------------
-- createConfig:
---------------------------------

prop_createConfig =
	monadicIO $
	withTempDir $ \configDir ->
		do
			run $ catchErrorsInTest $ createConfig configDir

			-- correct directory structure?
			-- TODO: find better test:
			dirContent <- run $ listDirectory configDir
			assert $ dirContent `elem` permutations [ "config", "logs" ]

			-- empty settings created?
			{-
			loadedSettings <- run $ catchErrorsInTest $
				loadSettings configDir
			assert $ loadedSettings == defSettings
			-}
