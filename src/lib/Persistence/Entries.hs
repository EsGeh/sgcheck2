module Persistence.Entries(
	writeHiddenFile, loadHiddenFile,
	list
) where

import Data.Settings
import Persistence.Global
import Utils
import qualified Utils.Path as Path

import System.Directory
import System.IO.Error
import Data.List as L

type Path = Path.Path
(</>) = (Path.</>)
(<.>) = (Path.<.>)


-- returns the origin:
loadHiddenFile :: Path -> Path -> ErrT IO Entry
loadHiddenFile configDir name = do
	content <- lift $ readFile $ Path.path_toStr $ configDir </> name <.> hiddenFileEnding
	ExceptT $ return $
		maybeToEither "couldn't read hidden file!" $
		liftM (Entry . Path.path_fromStr) $
		L.stripPrefix "ORIGIN=" $
		L.takeWhile (/='\n') $
		content

writeHiddenFile :: Path -> Settings -> Path -> Path -> ErrT IO ()
writeHiddenFile configDir settings src _ = do
	lift $
		writeFile (Path.path_toStr $ configDir </> Path.filename src <.> hiddenFileEnding) $
			hiddenFileContent settings src

list :: Path -> ErrT IO [Entry]
list configDir =
	do
		allFiles <-
			ExceptT $
			(liftM Right $ getDirectoryContents $ Path.path_toStr configDir)
				`catchIOError` (\e -> return $ Left $ "error listing entries: " ++ show e)
		mapM (loadHiddenFile configDir) $
			map Path.dropExtension $
			filter (maybe False ((==hiddenFileEnding) ) . Path.extension) $
			map Path.path_fromStr $
			allFiles
	--return $ []

hiddenFileContent :: Settings -> Path -> String
hiddenFileContent _ src =
	"ORIGIN=" ++ Path.path_toStr src ++ "\n"
