{-# LANGUAGE LambdaCase #-}
module TestUtils.Dir(
	DirDescr,
	dir_name,
	dirFile,
	dirDir,
	findPosInDir,
	insert,
	dir_subsetOf,

	PosInDir,
	pos_down, pos_up,
	pos_getFilename,
	pos_getFullPath,
	allPositions,
	allSubPositions,
	allSubPositionsRec,

	readDir,
	writeDir,
) where

import Utils

--import qualified System.File.Tree as Dir
import qualified System.Directory.Tree as Dir
import qualified Data.Tree as Tree
--import qualified Data.Tree.Zipper as Zipper

import Data.Foldable( asum )
import Data.Maybe

import System.FilePath as Path( (</>), (<.>) )
import qualified System.FilePath as Path

type Path = Path.FilePath


type DirDescr = Dir.DirTree FileContent

type FileContent = String
type PosInDir = [Path]

dir_name = Dir.name
dirFile = Dir.File
dirDir = Dir.Dir

handleDirExceptions ::
	MonadIO m =>
	IO (Dir.AnchoredDirTree ()) -> ErrT m ()
handleDirExceptions x =
		liftIO x >>= \case
			(_ Dir.:/ (Dir.Failed _ err)) -> throwE $ show err
			ret ->
				do
					--liftIO $ putStrLn $ "withTestScenario created: " ++ show ret
					return ()

dir_subsetOf :: DirDescr -> DirDescr -> Bool
dir_subsetOf a b =
	subset' (toTree a) (toTree b)
	where
		subset' ::
			Tree.Tree (Dir.DirTree FileContent)
			-> Tree.Tree (Dir.DirTree FileContent)
			-> Bool
		subset' a b =
			let
				aDir = (Tree.rootLabel a) 
				bDir = (Tree.rootLabel b)
			in
				dir_name aDir == dir_name bDir
			&&
			(
				case (aDir, bDir) of
					(Dir.File _ x, Dir.File _ y) -> x == y
					_ -> True
			)
			&&
			(
				all
					(\x -> any (x `subset'`) $ Tree.subForest b) $
					Tree.subForest a
			)

readDir :: Path -> IO DirDescr
readDir = fmap Dir.dirTree . Dir.readDirectory

writeDir :: MonadIO m => Path -> DirDescr -> ErrT m ()
writeDir location dir =
	handleDirExceptions $
	Dir.writeDirectory $ location Dir.:/ dir

pos_down = drop 1
pos_up = (:)

-- |insert a dir as a direct child of some other
insert :: DirDescr -> DirDescr -> DirDescr
insert dirToInsert parentDir =
	case parentDir of
		Dir.Dir name content -> Dir.Dir name $ dirToInsert : content
		_ -> parentDir

{-
insert :: PosInDir -> DirDescr -> DirDescr -> DirDescr
insert pos dirToInsert =
	fromTree .
	Zipper.toTree .
	Zipper.modifyLabel (mapToSnd insertF) .
	moveToPos .
	Zipper.fromTree .
	toTree 
	where
		moveToPos :: Zipper (Path, Dir.DirTree a) -> Zipper (Path, Dir.DirTree a)
		moveToPos z =
		insertF :: DirDescr -> DirDescr
		insertF = undefined
-}

--type Zipper a = Zipper.TreePos Zipper.Full a

findPosInDir ::
	Monad m =>
	PosInDir -> Dir.DirTree a -> ErrT m (Dir.DirTree a)
findPosInDir pos dirTree =
	walkDownDir pos $
	toTree dirTree

walkDownDir ::
	Monad m =>
	PosInDir -> Tree.Tree (Dir.DirTree a) -> ErrT m (Dir.DirTree a)
walkDownDir pos (Tree.Node dir subNodes) =
	let
		path = dir_name dir
	in
	case pos of
		x:xs ->
			do
				when (x/=path) $ throwE "position not found in dir tree!"
				if xs == []
					then return $ dir
					else
						asum $
						map (walkDownDir xs) subNodes

fromTree :: Tree.Tree (Path, Dir.DirTree a) -> Dir.DirTree a
fromTree = undefined

toTree :: Dir.DirTree a -> Tree.Tree (Dir.DirTree a)
toTree =
	treeFromDir fromFile fromSubTrees
	where
		fromFile name content =
			Tree.Node (Dir.File name content) []
		fromSubTrees name content subTrees =
			Tree.Node
				(Dir.Dir name content)
				subTrees

pos_getFilename = listToMaybe . reverse

pos_getFullPath :: PosInDir -> Path
pos_getFullPath = foldl (</>) ""

allSubPositions =
	map (pos_down . Tree.rootLabel) .
	Tree.subForest .
	annotateWithPositions

allSubPositionsRec =
	filter (/=[]) .
	map pos_down .
	allPositions

allPositions =
	Tree.flatten .
	annotateWithPositions

annotateWithPositions :: Dir.DirTree a -> Tree.Tree PosInDir
annotateWithPositions dir =
	treeFromDir treeFromFile treeFromTrees dir
	where
		treeFromFile name _ = Tree.Node [name] []
		treeFromTrees name entries subNodes = 
			Tree.Node [name] $
				map (fmap $ (name :)) $
				subNodes

treeFromDir treeFromFile treeFromTrees dir =
	case dir of
		Dir.File name content -> treeFromFile name content
		Dir.Dir name entries ->
			treeFromTrees name entries $
				map (treeFromDir treeFromFile treeFromTrees) entries
