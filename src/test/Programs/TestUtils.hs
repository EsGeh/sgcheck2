{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Programs.TestUtils where

import Programs.InOut
import Programs.InOut.Params
import Data.Settings
import Utils
import Utils.Path as Path( Path, (</>), (<.>) )
import qualified Utils.Path as Path
import TestUtils
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
			all (not . Path.path_isEmpty) $ [origin_name, this_name, configDir]
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
			(getNonEmptyPath <$> arbitrary)
			--`suchThat` (\path -> path/=(Dir.dir_name origin) && path/=(Dir.dir_name this_nonManagedFiles))

		(managedFileOrigins :: [Dir.PosInDir]) <-
			(if not managedFilesCanBeEmpty then (`suchThat` (not . null)) else id) $
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
	MonadIO m => TestScenario -> (Path -> m a) -> ErrT m a
withTestScenario scenario f =
	withTempDir $ \tempDir ->
		(prepareFiles scenario tempDir)
		>>
		lift (f tempDir)
	where
		prepareFiles :: TestScenario -> Path -> ErrT m ()
		prepareFiles TestScenario{..} tempDir =
			do
				Dir.writeDir tempDir origin
				Dir.writeDir tempDir this
				let configDirTree =
					Dir.dirDir (Path.path_toStr configDir) $
					mapMaybe maybeCreateConfigFile $
					--map (\dest -> Dir.dirFile (configFilenameFromDest dest) (configContentFromDest dest)) $
					configFiles
				Dir.writeDir tempDir configDirTree
		maybeCreateConfigFile :: Dir.PosInDir -> Maybe Dir.DirDescr
		maybeCreateConfigFile dest =
			do
				dest_name <- (Path.path_toStr <$> Dir.pos_getFilename dest)
				let configFilename = dest_name ++ ".sgcheck2"
				let configContent =
					"ORIGIN=" ++ (Path.path_toStr $ Dir.pos_getFullPath dest)
				return $
					Dir.dirFile configFilename configContent

distinct [] = True
distinct (x:xs) = ((/=x) `all` xs) && distinct xs

instance Arbitrary Dir.DirDescr where
	arbitrary =
		resize 5 $
		sized $ arbTree (getNonEmptyPath <$> arbitrary)

arbTree :: Gen Path -> Int -> Gen Dir.DirDescr
arbTree fileNameGen 0 =
	Dir.dirFile <$>
		(Path.path_toStr <$> fileNameGen) <*>
		pure "a"
			--arbitrary
arbTree fileNameGen size =
	do
		(Positive subNodeCount) <- arbitrary
		--let subNodeCount = 5
		let childrenSize = size `div` (subNodeCount + 1)
		f <- replicateM subNodeCount (arbTree fileNameGen childrenSize)
		name <- fileNameGen
		return $ Dir.dirDir (Path.path_toStr name) f

instance Arbitrary CopyCommandParams where
	arbitrary =
		(uncurryN CopyCommandParams) <$>
		(arbitrary :: Gen (Path, CopyFlags))

instance Arbitrary CopyFlags where
	arbitrary =
		(uncurryN CopyFlags) <$>
		(arbitrary :: Gen (Bool, Bool, [String]))
