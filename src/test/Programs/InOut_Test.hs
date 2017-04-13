{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Programs.InOut_Test where

import Programs.InOut
import Programs.InOut.Params
import Data.Settings
import Programs.TestUtils

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
import Control.Concurrent

import System.FilePath as Path( (</>), (<.>) )
import qualified System.FilePath as Path


prop_checkOut_copiesFilesCorrectly =
	forAll (arbitrary) $ \scenario@TestScenario{..} ->
	forAll (elements $ Dir.allSubPositionsRec origin) $ \fileToCopy ->
		monadicIO $
		handleIOErrs $
		withTestScenario scenario $ \tempDir ->
		do
			run $ putStrLn $ "scenario : " ++ show scenario
			run $ putStrLn $ "file : " ++ show fileToCopy
			(originalDir :: Dir.DirDescr) <-
				fmap (either error id) $
				runExceptT $ Dir.findPosInDir (Dir.pos_up (Dir.dir_name origin) fileToCopy) origin
			run $ putStrLn $ "originalDir: " ++ show originalDir
			maybeErr <-
				simulateCheckOut tempDir (Dir.pos_getFullPath $ fileToCopy) scenario
			liftIO $ threadDelay 1000
			run $ maybe (return ()) (putStrLn) $ maybeErr
			assert $ isNothing maybeErr
			fileToCopy_filename <- handleIOErrs $
				maybe (throwE "file to copy path is empty") return $ Dir.pos_getFilename fileToCopy
			copiedDir <- run $
				Dir.readDir (tempDir </> Dir.dir_name this </> fileToCopy_filename)
			run $ putStrLn $ "copiedDir: " ++ show copiedDir
			assert $ copiedDir == originalDir

prop_checkIn_copiesFilesCorrectly =
	forAll (getValidCheckInScenario <$> arbitrary) $ \scenario@TestScenario{..} ->
	forAll (elements $ configFiles) $ \fileToCopy ->
		monadicIO $
		handleIOErrs $
		withTestScenario scenario $ \tempDir ->
		do
			run $ putStrLn $ "scenario: " ++ show scenario
			run $ putStrLn $ "file : " ++ show fileToCopy
			(originalDir :: Dir.DirDescr) <-
				--handleIOErrs $
				fmap (either error id) $ runExceptT $
				Dir.findPosInDir (Dir.pos_up (Dir.dir_name this) fileToCopy) this
			run $ putStrLn $ "originalDir: " ++ show originalDir
			maybeErr <-
				simulateCheckIn tempDir (Dir.pos_getFullPath $ fileToCopy) scenario
			liftIO $ threadDelay 1000
			run $ maybe (return ()) (putStrLn) $ maybeErr
			assert $ isNothing maybeErr
			fileToCopy_filename <- handleIOErrs $
				maybe (throwE "file to copy path is empty") return $ Dir.pos_getFilename fileToCopy
			copiedDir <- run $
				Dir.readDir (tempDir </> Dir.dir_name origin </> fileToCopy_filename)
			run $ putStrLn $ "copiedDir: " ++ show copiedDir
			assert $ copiedDir == originalDir

prop_list_doesntChangeFS =
	forAll (arbitrary) $ \scenario@TestScenario{..} ->
		monadicIO $
		ignoreIOErrs $
		withTestScenario scenario $ \tempDir ->
		do
			(originalDir :: Dir.DirDescr) <- run $ Dir.readDir tempDir
			maybeErr <- simulateList tempDir scenario
			run $ maybe (return ()) (putStrLn) $ maybeErr
			(dirAfterCmd :: Dir.DirDescr) <- run $ Dir.readDir tempDir
			--run $ putStrLn $ "originalDir: " ++ show originalDir
			--run $ putStrLn $ "dirAfterCmd: " ++ show dirAfterCmd
			assert $ dirAfterCmd == originalDir
			--return ()

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

simulateCheckOut :: Path -> Path -> TestScenario -> PropertyM IO (Maybe String)
simulateCheckOut tempDir fileToCopy TestScenario{..} =
	do
		let settings =
			Settings{
				serverIP = Nothing,
				thisIP = Nothing,
				serverPath = tempDir </> (Dir.dir_name origin),
				thisPath = tempDir </> (Dir.dir_name this)
			}
		--run $ putStrLn $ "simulateCheckOut. settings: " ++ show settings
		--run $ putStrLn $ "simulateCheckOut. params: " ++ show params
		let memorizeFile = Persistence.writeHiddenFile $ tempDir </> configDir
		errOrRes <- run $ runExceptT $
			runMaybeT $ checkOut (testCopyParams fileToCopy) settings memorizeFile
		case errOrRes of
			Left err -> return $ Just $ err
			Right _ -> return Nothing

simulateCheckIn :: Path -> Path -> TestScenario -> PropertyM IO (Maybe String)
simulateCheckIn tempDir fileToCopy TestScenario{..} =
	do
		let settings =
			Settings{
				serverIP = Nothing,
				thisIP = Nothing,
				serverPath = tempDir </> (Dir.dir_name origin),
				thisPath = tempDir </> (Dir.dir_name this)
			}
		{-
		run $ putStrLn $ "simulateCheckOut. settings: " ++ show settings
		run $ putStrLn $ "simulateCheckOut. params: " ++ show params
		-}
		let lookupFile = Persistence.loadHiddenFile $ tempDir </> configDir
		errOrRes <- run $ runExceptT $
			runMaybeT $ checkIn (testCopyParams fileToCopy) settings lookupFile
		case errOrRes of
			Left err -> return $ Just $ err
			Right _ -> return Nothing

simulateList :: Path -> TestScenario -> PropertyM IO (Maybe String)
simulateList tempDir TestScenario{..} =
	do
		let params = []
		let settings =
			Settings{
				serverIP = Nothing,
				thisIP = Nothing,
				serverPath = tempDir </> Dir.dir_name origin,
				thisPath = tempDir </> Dir.dir_name this
			}
		let listFiles = Persistence.list $ tempDir </> configDir
		errOrRes <- run $ runExceptT $
			runMaybeT $ list settings params listFiles
			-- return $ Right Nothing
		case errOrRes of
			Left err -> return $ Just $ err
			Right _ -> return Nothing

testCopyParams fileToCopy =
	CopyCommandParams{
		copyCmd_file = fileToCopy,
		copyCmd_flags = defCopyFlags { copyFlags_printCommand = True }
	}
