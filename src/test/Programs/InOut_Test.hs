{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Programs.InOut_Test where

import Programs.InOut
import Programs.InOut.Params
import Data.Settings
import Programs.TestUtils

import qualified Persistence
import qualified Persistence.Entries as Persistence
import qualified Persistence.Settings as Persistence

import Utils
import TestUtils
import qualified TestUtils.Dir as Dir

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Hspec

import Control.Monad.Trans.Maybe
import qualified Control.Exception as SysExc
import Data.Char
import Data.Either
import Data.Maybe
import Data.Tuple.Curry( uncurryN )

import System.FilePath as Path( (</>), (<.>) )
import qualified System.FilePath as Path


prop_checkOut_copiesFilesCorrectly =
	forAll (arbitrary) $ \scenario@TestScenario{..} ->
	forAll (elements $ Dir.allSubPositionsRec origin) $ \fileToCopy ->
		monadicIO $
		handleIOErrs $
		withTestScenario scenario $ \tempDir settings ->
		do
			let fileToCopy_filename =
				Dir.pos_getFilename fileToCopy
			--run $ putStrLn $ "scenario : " ++ show scenario
			--run $ putStrLn $ "file : " ++ show fileToCopy
			(originalDir :: Dir.DirDescr) <-
				(either fail return =<<) $
				runExceptT $ Dir.findPosInDir (Dir.pos_up (Dir.dir_name origin) fileToCopy) origin
			--run $ putStrLn $ "originalDir: " ++ show originalDir

			-- path does not exist locally:
			pre $
				not $
					(Dir.dir_name this `Dir.pos_up` [fileToCopy_filename])
					`Dir.posIsinDir`
					this

			maybeErr <-
				simulateCheckOut (testCopyParams $ Dir.pos_getFullPath $ fileToCopy) tempDir scenario settings
			run $ maybe (return ()) (putStrLn) $ maybeErr
			assert $ isNothing maybeErr
			copiedDir <- run $
				Dir.readDir (tempDir </> Dir.dir_name this </> fileToCopy_filename)
			--run $ putStrLn $ "copiedDir: " ++ show copiedDir
			assert $ originalDir `Dir.dir_subsetOf` copiedDir

prop_checkOut_simulateDoesntChangeFilesystem =
	forAll (arbitrary) $ \scenario@TestScenario{..} ->
	forAll (elements $ Dir.allSubPositionsRec origin) $ \fileToCopy ->
		monadicIO $
		handleIOErrs $
		withTestScenario scenario $ \tempDir settings ->
		do
			--run $ putStrLn $ "scenario : " ++ show scenario
			--run $ putStrLn $ "file : " ++ show fileToCopy
			(originalDir :: Dir.DirDescr) <- run $ Dir.readDir tempDir
			--run $ putStrLn $ "originalDir: " ++ show originalDir

			-- path does not exist locally:
			pre $
				not $
					(Dir.dir_name this `Dir.pos_up` [Dir.pos_getFilename fileToCopy])
					`Dir.posIsinDir`
					this

			maybeErr <-
				simulateCheckOut
					(testCopyParams $ Dir.pos_getFullPath $ fileToCopy){ copyCmd_flags = testCopyFlags{ copyFlags_simulate = True } }
					tempDir
					scenario
					settings
			run $ maybe (return ()) (putStrLn) $ maybeErr
			assert $ isNothing maybeErr
			(dirAfterCmd :: Dir.DirDescr) <- run $ Dir.readDir tempDir
			--run $ putStrLn $ "dirAfterCmd: " ++ show dirAfterCmd
			assert $ dirAfterCmd == originalDir

prop_checkIn_copiesFilesCorrectly =
	forAll (getValidCheckInScenario <$> arbitrary) $ \scenario@TestScenario{..} ->
	forAll (elements $ configFiles) $ \fileToCopy ->
		monadicIO $
		handleIOErrs $
		withTestScenario scenario $ \tempDir settings ->
		do
			--run $ putStrLn $ "scenario: " ++ show scenario
			--run $ putStrLn $ "file : " ++ show fileToCopy
			(originalDir :: Dir.DirDescr) <-
				--handleIOErrs $
				fmap (either error id) $ runExceptT $
				Dir.findPosInDir (Dir.pos_up (Dir.dir_name this) fileToCopy) this
			--run $ putStrLn $ "originalDir: " ++ show originalDir
			maybeErr <-
				simulateCheckIn (testCopyParams $ Dir.pos_getFullPath $ fileToCopy) tempDir scenario settings
			run $ maybe (return ()) (putStrLn) $ maybeErr
			assert $ isNothing maybeErr
			let fileToCopy_filename = Dir.pos_getFilename fileToCopy
			copiedDir <- run $
				Dir.readDir (tempDir </> Dir.dir_name origin </> fileToCopy_filename)
			--run $ putStrLn $ "copiedDir: " ++ show copiedDir
			assert $ originalDir `Dir.dir_subsetOf` copiedDir

prop_checkIn_simulateDoesntChangeFilesystem =
	forAll (getValidCheckInScenario <$> arbitrary) $ \scenario@TestScenario{..} ->
	forAll (elements $ configFiles) $ \fileToCopy ->
		monadicIO $
		handleIOErrs $
		withTestScenario scenario $ \tempDir settings ->
		do
			--run $ putStrLn $ "scenario : " ++ show scenario
			--run $ putStrLn $ "file : " ++ show fileToCopy
			(originalDir :: Dir.DirDescr) <- run $ Dir.readDir tempDir
			--run $ putStrLn $ "originalDir: " ++ show originalDir
			maybeErr <-
				simulateCheckIn
					(testCopyParams $ Dir.pos_getFullPath $ fileToCopy){ copyCmd_flags = testCopyFlags{ copyFlags_simulate = True} }
					tempDir
					scenario
					settings
			run $ maybe (return ()) (putStrLn) $ maybeErr
			assert $ isNothing maybeErr
			(dirAfterCmd :: Dir.DirDescr) <- run $ Dir.readDir tempDir
			--run $ putStrLn $ "dirAfterCmd: " ++ show dirAfterCmd
			assert $ dirAfterCmd == originalDir

prop_list_doesntChangeFS =
	forAll (arbitrary) $ \scenario@TestScenario{..} ->
		monadicIO $
		ignoreIOErrs $
		withTestScenario scenario $ \tempDir settings ->
		do
			(originalDir :: Dir.DirDescr) <- run $ Dir.readDir tempDir
			--run $ putStrLn $ "originalDir: " ++ show originalDir
			maybeErr <- simulateList tempDir scenario settings
			run $ maybe (return ()) (putStrLn) $ maybeErr
			(dirAfterCmd :: Dir.DirDescr) <- run $ Dir.readDir tempDir
			--run $ putStrLn $ "dirAfterCmd: " ++ show dirAfterCmd
			assert $ dirAfterCmd == originalDir

ignoreIOErrs ::
	ErrT (PropertyM IO) a -> PropertyM IO ()
ignoreIOErrs x =
	runExceptT x >> return ()

handleIOErrs ::
	ErrT (PropertyM IO) a -> PropertyM IO a
	--MonadIO m => ErrT (PropertyM m) () -> PropertyM m ()
handleIOErrs x =
	do
		ioErrOrRes <- runExceptT x
		case ioErrOrRes of
			Left err ->
				do
					run $ putStrLn $ "io error in test: " ++ err
					assert $ isRight ioErrOrRes
					stop $ isRight ioErrOrRes
			Right x -> return x

simulateCheckOut :: CopyCommandParams -> Path -> TestScenario -> Settings -> PropertyM IO (Maybe String)
simulateCheckOut params tempDir TestScenario{..} settings =
	do
		--run $ putStrLn $ "simulateCheckOut. settings: " ++ show settings
		--run $ putStrLn $ "simulateCheckOut. params: " ++ show params
		Persistence.withFileSys settings (tempDir </> configDir) $ \fileSys ->
			do
				errOrRes <- run $ runExceptT $
					runMaybeT $ checkOut params settings fileSys
				case errOrRes of
					Left err -> return $ Just $ err
					Right _ -> return Nothing

simulateCheckIn :: CopyCommandParams -> Path -> TestScenario -> Settings -> PropertyM IO (Maybe String)
simulateCheckIn params tempDir TestScenario{..} settings =
	{-
	run $ putStrLn $ "simulateCheckOut. settings: " ++ show settings
	run $ putStrLn $ "simulateCheckOut. params: " ++ show params
	-}
	Persistence.withFileSys settings (tempDir </> configDir) $ \fileSys ->
	do
		errOrRes <- run $ runExceptT $
			runMaybeT $ checkIn params settings fileSys
		case errOrRes of
			Left err -> return $ Just $ err
			Right _ -> return Nothing

simulateList :: Path -> TestScenario -> Settings -> PropertyM IO (Maybe String)
simulateList tempDir TestScenario{..} settings =
	do
		let params = []
		let listFiles = Persistence.list settings $ tempDir </> configDir
		errOrRes <- run $ runExceptT $
			runMaybeT $ list settings params listFiles
			-- return $ Right Nothing
		case errOrRes of
			Left err -> return $ Just $ err
			Right _ -> return Nothing

testCopyParams fileToCopy =
	CopyCommandParams{
		copyCmd_file = fileToCopy,
		copyCmd_flags = testCopyFlags
	}

testCopyFlags = defCopyFlags {
	copyFlags_printCommand = False,
	copyFlags_printRSyncOut = False
}
