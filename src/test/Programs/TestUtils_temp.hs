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

{-
-- a situation for check out script
data CheckOutScenario
	= CheckOutScenario {
		checkOut_fileToCopy :: Dir.PosInDir,
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
-}

{-
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

isValidCheckOutScenario CheckOutScenario{ checkOut_scenario=TestScenario{..}, ..} =
	checkOut_fileToCopy `elem` (Tree.flatten $ annotateWithPositions originFiles)
	--checkOut_fileToCopy `elem` originFiles
-}

{-
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

isValidCheckInScenario CheckInScenario{ checkIn_scenario=TestScenario{..}, ..} =
	and $
	[ checkIn_fileToCopy `elem` thisFiles
	, checkIn_fileToCopy `elem` originFiles
	]
-}

{-
instance Arbitrary CheckOutScenario where
	arbitrary =
		(uncurryN CheckOutScenario) <$>
		(arbitrary :: Gen (Dir.PosInDir, TestScenario))

instance Arbitrary CheckInScenario where
	arbitrary =
		(uncurryN CheckInScenario) <$>
		(arbitrary :: Gen (Path, TestScenario))
-}
