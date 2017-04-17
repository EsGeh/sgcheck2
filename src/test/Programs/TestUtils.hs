{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Programs.TestUtils where

import Programs.InOut
import Programs.InOut.Params
import Data.Settings
import qualified Persistence.Entries as Persistence
import qualified Persistence.Settings as Persistence
import Utils
import TestUtils
import Programs.InOut.Utils
import qualified TestUtils.Dir as Dir

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Control.Monad.Trans.Maybe
import qualified Control.Exception as SysExc
import Data.Char
import Data.Either
import Data.Maybe
import Data.Tuple.Curry( uncurryN )

import Data.Foldable

import System.FilePath as Path( (</>), (<.>) )
import qualified System.FilePath as Path


-- a directory structure for testing:
data TestScenario
	= TestScenario {
		origin :: Dir.DirDescr,
		this :: Dir.DirDescr,
		configDir :: Path,
		configFiles :: [Dir.PosInDir]
	}
	deriving( Show, Eq, Ord )

isValidTestScenario TestScenario{..} =
	let
		origin_name = Dir.dir_name origin
		this_name = Dir.dir_name this
	in
		and $
		[
			all (\p -> Path.isValid p && Path.isRelative p) $ [origin_name, this_name, configDir]
			, distinct [origin_name, this_name, configDir]
			
		]

newtype ValidCheckInScenario = ValidCheckInScenario{ getValidCheckInScenario :: TestScenario }
	deriving( Show, Eq, Ord )

instance Arbitrary ValidCheckInScenario where
	arbitrary =
		fmap ValidCheckInScenario $
		(`suchThat` isValidTestScenario) $ -- just to be shure:
		genScenario False

instance Arbitrary TestScenario where
	arbitrary =
		(`suchThat` isValidTestScenario) $ -- just to be shure:
		genScenario True

genScenario managedFilesCanBeEmpty =
	do
		origin <- arbitrary
		this_nonManagedFiles <- arbitrary

		configDir <-
			(getValidPath <$> arbitrary) --`suchThat` (\path -> path/=(Dir.dir_name origin) && path/=(Dir.dir_name this_nonManagedFiles))

		(managedFileOrigins :: [Dir.PosInDir]) <-
			(if not managedFilesCanBeEmpty then (`suchThat` (not . null)) else id) $
				--fmap (map $ Dir.pos_up $ Dir.dir_name origin) $
				sublistOf $
				Dir.allSubPositions $
				origin
		(managedDirs :: [Dir.DirDescr]) <-
			catch $
			mapM (
				flip Dir.findPosInDir origin .
				(Dir.pos_up $ Dir.dir_name origin)
			) $
			managedFileOrigins

		let configFiles = managedFileOrigins

		let
			this :: Dir.DirDescr
			this =
				($ this_nonManagedFiles) $
				foldl (.) id $
				map `flip` (managedFileOrigins `zip` managedDirs) $ \(origin, dirToInsert) ->
				Dir.insert dirToInsert

		return $ TestScenario{..}
	where
		catch x =
			do
				errOrVal <- runExceptT x
				case errOrVal of
					Left err -> fail err
					Right y -> return y

{- |creates a temporary directory structure
 -  corresponding to a given TestScenario
 -  returns an error if directory structure raises a system exception
-}
withTestScenario ::
	forall a m .
	MonadIO m => TestScenario -> (Path -> Settings -> m a) -> ErrT m a
withTestScenario scenario f =
	withTempDir $ \tempDir ->
		(prepareFiles scenario tempDir)
		>>=
		lift . (f tempDir)
	where
		prepareFiles :: TestScenario -> Path -> ErrT m Settings
		prepareFiles TestScenario{..} tempDir =
			do
				Dir.writeDir tempDir origin
				Dir.writeDir tempDir this
				errT . liftIO . runExceptT $ Persistence.createConfig $ tempDir </> configDir
				let settings =
					Settings{
						thisPath = tempDir </> Dir.dir_name this,
						serverPath = tempDir </> Dir.dir_name origin,
						thisIP = Nothing, serverIP = Nothing
					}
				errT . liftIO . runExceptT $
					forM_ configFiles $ \dest ->
						let
							entry =
								entryFromPathOnServer settings $
								Dir.pos_getFullPath $
								--(Dir.pos_up $ tempDir) $
								--(Dir.pos_up $ Dir.dir_name origin) $
								dest
						in
							do
								--liftIO $ putStrLn $ "testScenario entry: " ++ show entry
								Persistence.writeHiddenFile (tempDir </> configDir) $ entry
				return settings

distinct [] = True
distinct (x:xs) = ((/=x) `all` xs) && distinct xs

instance Arbitrary Dir.DirDescr where
	arbitrary =
		resize 5 $
		sized $ arbTree ((getValidPath <$> arbitrary) `suchThat` Path.isRelative `suchThat` (not . Path.hasTrailingPathSeparator) `suchThat` (not . (Path.pathSeparator `elem`)))

arbTree :: Gen Path -> Int -> Gen Dir.DirDescr
arbTree fileNameGen 0 =
	Dir.dirFile <$>
		fileNameGen <*>
		pure "a"
			--arbitrary
arbTree fileNameGen size =
	do
		(Positive subNodeCount) <- arbitrary
		--let subNodeCount = 5
		let childrenSize = size `div` (subNodeCount + 1)
		f <-
			replicateM subNodeCount (arbTree fileNameGen childrenSize)
			`suchThat` (distinct . map Dir.dir_name)
		name <- fileNameGen
		return $ Dir.dirDir name f

instance Arbitrary CopyCommandParams where
	arbitrary =
		(uncurryN CopyCommandParams) <$>
		(arbitrary :: Gen (Path, CopyFlags))

instance Arbitrary CopyFlags where
	arbitrary =
		(uncurryN CopyFlags) <$>
		(arbitrary :: Gen (Bool, Bool, Bool, [String]))
