{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Programs.InOut_Test where

import Programs.InOut
import Programs.InOut.Params
import Data.Settings

import qualified Persistence.Entries as Persistence
import qualified Persistence.Settings as Persistence

{-
import Persistence.Entries
import Persistence.Settings
import Data.Entry
import Data.Settings
-}
import Utils
import TestUtils

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Hspec

import System.Directory( getTemporaryDirectory, removeDirectoryRecursive, listDirectory, createDirectory )

import Control.Monad.Trans.Maybe
import qualified Control.Exception as SysExc
import Data.Char
import Data.Either
import Data.Maybe
import Data.Tuple.Curry( uncurryN )


-- a directory structure for testing:
data TestScenario
	= TestScenario {
		configDir :: Path,
		origin :: Path,
		this :: Path,
		configFiles :: [Path],
		originFiles :: [Path],
		thisFiles :: [Path]
	}
	deriving( Show, Eq, Ord )

newtype ValidTestScenario = ValidTestScenario{ getValidTestScenario :: TestScenario }
	deriving( Show, Eq, Ord )

-- a situation for check out script
data CheckOutScenario
	= CheckOutScenario {
		checkOut_fileToCopy :: Path,
		checkOut_scenario :: TestScenario
	}
	deriving( Show, Eq, Ord )

newtype ValidCheckOutScenario = ValidCheckOutScenario{ getValidCheckOutScenario :: CheckOutScenario }
	deriving( Show, Eq, Ord )

-- a situation for check out script
data CheckInScenario
	= CheckInScenario {
		checkIn_fileToCopy :: Path,
		checkIn_scenario :: TestScenario
	}
	deriving( Show, Eq, Ord )

newtype ValidCheckInScenario = ValidCheckInScenario{ getValidCheckInScenario :: CheckInScenario }
	deriving( Show, Eq, Ord )

newtype ValidListScenario = ValidListScenario{ getValidListScenario :: TestScenario }
	deriving( Show, Eq, Ord )

prop_checkOut_mustFailOnInvalidArgs checkoutScenario@CheckOutScenario{ checkOut_scenario=scenario@TestScenario{..}, ..} =
	monadicIO $ -- :: PropertyM IO a -> Property
	do
		ioErrOrRes <- runTest
		let isValidScenario = isValidCheckOutScenario checkoutScenario
		assert $
			case ioErrOrRes of
				Left ioErr -> not $ isValidScenario
				Right (Just err) -> not $ isValidScenario
				Right Nothing -> isValidScenario
	where
		-- Left on filesystem error
		-- if a "Just" is returned: a function failed with an error
		runTest :: PropertyM IO (Either String (Maybe String))
		runTest =
			runExceptT $
			withTestScenario scenario $ \tempDir ->
				simulateCheckOut tempDir checkOut_fileToCopy scenario

prop_checkOut_copiesFilesCorrectly :: ValidCheckOutScenario -> Property
prop_checkOut_copiesFilesCorrectly ValidCheckOutScenario{ getValidCheckOutScenario=CheckOutScenario{ checkOut_scenario=scenario@TestScenario{..}, ..} } =
		monadicIO $
		handleIOErrs $
		withTestScenario scenario $ \tempDir ->
		do
			maybeErr <- simulateCheckOut tempDir checkOut_fileToCopy scenario
			assert $ isNothing maybeErr
			localFiles <- run $ listDirectory (path_toStr $ tempDir </> this)
			assert $ path_toStr checkOut_fileToCopy `elem` localFiles
		where
			handleIOErrs x =
				do
					ioErrOrRes <- runExceptT x
					assert $ isRight ioErrOrRes

prop_checkIn_mustFailOnInvalidArgs checkoutScenario@CheckInScenario{ checkIn_scenario=scenario@TestScenario{..}, ..} =
	monadicIO $ -- :: PropertyM IO a -> Property
	do
		ioErrOrRes <- runTest
		let isValidScenario = isValidCheckInScenario checkoutScenario
		assert $
			case ioErrOrRes of
				Left ioErr -> not $ isValidScenario
				Right (Just err) -> not $ isValidScenario
				Right Nothing -> isValidScenario
	where
		-- Left on filesystem error
		-- if a "Just" is returned: a function failed with an error
		runTest :: PropertyM IO (Either String (Maybe String))
		runTest =
			runExceptT $
			withTestScenario scenario $ \tempDir ->
				simulateCheckIn tempDir checkIn_fileToCopy scenario

prop_checkIn_copiesFilesCorrectly :: ValidCheckInScenario -> Property
prop_checkIn_copiesFilesCorrectly ValidCheckInScenario{ getValidCheckInScenario=CheckInScenario{ checkIn_scenario=scenario@TestScenario{..}, ..} } =
		monadicIO $
		handleIOErrs $
		withTestScenario scenario $ \tempDir ->
		do
			maybeErr <- simulateCheckIn tempDir checkIn_fileToCopy scenario
			assert $ isNothing maybeErr
			foundFiles <- run $ listDirectory (path_toStr $ tempDir </> origin)
			assert $ path_toStr checkIn_fileToCopy `elem` foundFiles

prop_list_doesntChangeFS =
	forAll (getValidListScenario <$> arbitrary) $ \scenario@TestScenario{..} ->
	monadicIO $
	handleIOErrs $
	withTestScenario scenario $ \tempDir ->
	do
			foundFiles_before <- run $ listDirectory (path_toStr $ tempDir </> origin)
			maybeErr <- simulateList tempDir scenario
			--run $ maybe (return ()) (putStrLn) $ maybeErr
			assert $ isNothing maybeErr
			foundFiles_after <- run $ listDirectory (path_toStr $ tempDir </> origin)
			assert $ foundFiles_before == foundFiles_after
			return ()

handleIOErrs x =
	do
		ioErrOrRes <- runExceptT x
		assert $ isRight ioErrOrRes

{- |creates a temporary directory structure
 -  corresponding to a given TestScenario
 -  returns an error if directory structure raises a system exception
-}
withTestScenario ::
	forall a m .
	MonadIO m => TestScenario -> (Path -> m a) -> ErrT m a
withTestScenario scenario f =
	withTempDir $ \tempDir ->
		(handleExceptions $ prepareFiles scenario tempDir)
		>>
		lift (f tempDir)
	where
		prepareFiles :: TestScenario -> Path -> IO ()
		prepareFiles TestScenario{..} tempDir =
			do
				catchExceptions $ Persistence.createConfig $ tempDir </> configDir
				createDirectory $ path_toStr $ tempDir </> origin
				createDirectory $ path_toStr $ tempDir </> this
				forM_ originFiles $ \path ->
					writeRndFile (tempDir </> origin </> path)
				forM_ thisFiles $ \path ->
					writeRndFile (tempDir </> this </> path)
				forM_ configFiles $ writeConfigFile
			where
				writeRndFile path =
					writeFile (path_toStr $ path) $ "a"
				writeConfigFile path =
					writeFile (path_toStr $ tempDir </> configDir </> path) $ "ORIGIN=" ++ path_toStr (basename path)
		handleExceptions :: IO b -> ErrT m b
		handleExceptions =
			ExceptT .
			liftIO .
			(SysExc.tryJust $ \(e :: SysExc.IOException) -> Just (show e))

simulateCheckOut :: Path -> Path -> TestScenario -> PropertyM IO (Maybe String)
simulateCheckOut tempDir checkOut_fileToCopy TestScenario{..} =
	do
		let params =
			CopyCommandParams{
				copyCmd_file = checkOut_fileToCopy,
				copyCmd_flags = defCopyFlags
			}
		let settings =
			Settings{
				serverIP = Nothing,
				thisIP = Nothing,
				serverPath = tempDir </> origin,
				thisPath = tempDir </> this
			}
		let memorizeFile = Persistence.writeHiddenFile $ tempDir </> configDir
		errOrRes <- run $ runExceptT $
			runMaybeT $ checkOut params settings memorizeFile
		case errOrRes of
			Left err -> return $ Just $ err
			Right _ -> return Nothing

simulateCheckIn :: Path -> Path -> TestScenario -> PropertyM IO (Maybe String)
simulateCheckIn tempDir fileToCopy TestScenario{..} =
	do
		let params =
			CopyCommandParams{
				copyCmd_file = fileToCopy,
				copyCmd_flags = defCopyFlags
			}
		let settings =
			Settings{
				serverIP = Nothing,
				thisIP = Nothing,
				serverPath = tempDir </> origin,
				thisPath = tempDir </> this
			}
		let lookupFile = Persistence.loadHiddenFile $ tempDir </> configDir
		errOrRes <- run $ runExceptT $
			runMaybeT $ checkIn params settings lookupFile
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
				serverPath = tempDir </> origin,
				thisPath = tempDir </> this
			}
		let listFiles = Persistence.list $ tempDir </> configDir
		errOrRes <- run $ runExceptT $
			runMaybeT $ list settings params listFiles
			-- return $ Right Nothing
		case errOrRes of
			Left err -> return $ Just $ err
			Right _ -> return Nothing

instance Arbitrary ValidTestScenario where
	arbitrary =
		fmap ValidTestScenario $
		(`suchThat` isValidTestScenario) $ -- just to be shure:
		do
			configDir <- getNonEmptyPath <$> arbitrary
			origin <- getNonEmptyPath <$> arbitrary
			this <- getNonEmptyPath <$> arbitrary

			-- TODO: avoid duplicates in the same dir:
			originFiles <- listOf $ getNonEmptyPath <$> arbitrary
			thisFiles <- listOf $ getNonEmptyPath <$> arbitrary

			configFiles <-
				map (<.> "sgcheck2") <$>
				sublistOf thisFiles
			return $ TestScenario{ .. }

isValidTestScenario TestScenario{..} =
	and $
	[
		all (not . path_isEmpty) $
			[configDir, origin, this]
			++ originFiles
			++ thisFiles
		, distinct [configDir, origin, this]
		, distinct originFiles
		, distinct thisFiles
	]

instance Arbitrary ValidCheckOutScenario where
	arbitrary =
		fmap ValidCheckOutScenario $
		(`suchThat` isValidCheckOutScenario) $ -- just to be shure:
		do
			checkOut_scenario <-
				(getValidTestScenario <$> arbitrary)
				`suchThat`
				\scenario -> length (originFiles scenario) > 0
			checkOut_fileToCopy <- elements $ originFiles checkOut_scenario
			return $
				CheckOutScenario{..}

instance Arbitrary ValidListScenario where
	arbitrary =
		fmap ValidListScenario $
		(`suchThat` isValidTestScenario) $ -- just to be shure:
		do
			configDir <- getNonEmptyPath <$> arbitrary
			origin <- getNonEmptyPath <$> arbitrary
			this <- getNonEmptyPath <$> arbitrary

			synchronizedFiles <- listOf $ getNonEmptyPath <$> arbitrary

			-- TODO: avoid duplicates in the same dir:
			originFiles <-
				fmap (synchronizedFiles ++) $
				listOf $ getNonEmptyPath <$> arbitrary
			thisFiles <-
				fmap (synchronizedFiles ++) $
				listOf $ getNonEmptyPath <$> arbitrary

			let configFiles =
				map (<.> "sgcheck2") $
				synchronizedFiles
			return $ TestScenario{ .. }

instance Arbitrary ValidCheckInScenario where
	arbitrary =
		fmap ValidCheckInScenario $
		(`suchThat` isValidCheckInScenario) $ -- just to be shure:
		do
			scenarioTemp <-
				(getValidTestScenario <$> arbitrary)
				`suchThat`
				\scenario -> length (configFiles scenario) > 0
			checkIn_fileToCopy <-
				basename <$>
				(elements $ configFiles scenarioTemp)
			--checkIn_fileToCopy <- elements $ thisFiles scenarioTemp
			let checkIn_scenario =
				fileAlsoAtOrigin checkIn_fileToCopy scenarioTemp
			return $
				CheckInScenario{..}
		where
			-- make shure that the file exists at the origin too:
			fileAlsoAtOrigin file scenario =
				if not $ file `elem` originFiles scenario
					then 
						scenario{ originFiles= file:(originFiles scenario) }
					else scenario

isValidCheckOutScenario CheckOutScenario{ checkOut_scenario=TestScenario{..}, ..} =
	checkOut_fileToCopy `elem` originFiles

isValidCheckInScenario CheckInScenario{ checkIn_scenario=TestScenario{..}, ..} =
	and $
	[ checkIn_fileToCopy `elem` thisFiles
	, checkIn_fileToCopy `elem` originFiles
	]

distinct [] = True
distinct (x:xs) = ((/=x) `all` xs) && distinct xs

instance Arbitrary TestScenario where
	arbitrary =
		(uncurryN TestScenario) <$>
		(arbitrary :: Gen (Path, Path, Path, [Path], [Path], [Path]))

instance Arbitrary CheckOutScenario where
	arbitrary =
		(uncurryN CheckOutScenario) <$>
		(arbitrary :: Gen (Path, TestScenario))

instance Arbitrary CheckInScenario where
	arbitrary =
		(uncurryN CheckInScenario) <$>
		(arbitrary :: Gen (Path, TestScenario))

instance Arbitrary CopyCommandParams where
	arbitrary =
		(uncurryN CopyCommandParams) <$>
		(arbitrary :: Gen (Path, CopyFlags))

instance Arbitrary CopyFlags where
	arbitrary =
		(uncurryN CopyFlags) <$>
		(arbitrary :: Gen (Bool, Bool, [String]))
