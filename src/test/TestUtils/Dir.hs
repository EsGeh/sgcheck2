{-# LANGUAGE LambdaCase #-}
module TestUtils.Dir(
	DirDescr,
	dir_name,
	dirFile,
	dirDir,
	findPosInDir,
	insert,

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
import qualified Utils.Path as Path

--import qualified System.File.Tree as Dir
import qualified System.Directory.Tree as Dir
import qualified Data.Tree as Tree
--import qualified Data.Tree.Zipper as Zipper

import Data.Foldable( asum )
import Data.Maybe

type Path = Path.Path
(</>) = (Path.</>)
(<.>) = (Path.<.>)


type DirDescr = Dir.DirTree FileContent

type FileContent = String
type PosInDir = [Path]

dir_name = Path.path_fromStr . Dir.name
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

readDir :: Path -> IO DirDescr
readDir = fmap Dir.dirTree . Dir.readDirectory . Path.path_toStr

writeDir :: MonadIO m => Path -> DirDescr -> ErrT m ()
writeDir location dir =
	handleDirExceptions $
	Dir.writeDirectory $ (Path.path_toStr location) Dir.:/ dir

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
	PosInDir -> Tree.Tree (Path, Dir.DirTree a) -> ErrT m (Dir.DirTree a)
walkDownDir pos (Tree.Node (path, dir) subNodes) =
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

toTree :: Dir.DirTree a -> Tree.Tree (Path, Dir.DirTree a)
toTree =
	treeFromDir fromFile fromSubTrees
	where
		fromFile name content = Tree.Node (Path.path_fromStr name, Dir.File name content) []
		fromSubTrees name content subTrees =
			Tree.Node
				(Path.path_fromStr $ name, Dir.Dir name content)
				subTrees

pos_getFilename = listToMaybe . reverse

pos_getFullPath :: PosInDir -> Path
pos_getFullPath = foldl (</>) (Path.path_fromStr "")

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
		treeFromFile name _ = Tree.Node [Path.path_fromStr name] []
		treeFromTrees name entries subNodes = 
			Tree.Node [Path.path_fromStr name] $
				map (fmap $ (Path.path_fromStr name :)) $
				subNodes
	{-
	case dir of
		Dir.File name _ -> Tree.Node [Path.path_fromStr name] []
		Dir.Dir name entries ->
			Tree.Node [Path.path_fromStr name] $
				map (fmap $ (Path.path_fromStr name :)) $
				map annotateWithPositions entries
	-}

treeFromDir treeFromFile treeFromTrees dir =
	case dir of
		Dir.File name content -> treeFromFile name content
		Dir.Dir name entries ->
			treeFromTrees name entries $
				map (treeFromDir treeFromFile treeFromTrees) entries
