module Persistence.Entries(
	writeHiddenFile, loadHiddenFile,
	list
) where

import Data.Settings
import Persistence.Global
import Global

import System.Directory
import System.IO.Error
import Data.List as L
import qualified Data.Text as T

-- returns the origin:
loadHiddenFile :: Path -> Path -> ErrT IO Entry
loadHiddenFile configDir name = do
	content <- lift $ readFile $ path_toStr $ configDir </> name <.> T.pack hiddenFileEnding
	ExceptT $ return $
		maybeToEither "couldn't read hidden file!" $
		liftM (Entry . path_fromStr) $
		L.stripPrefix "ORIGIN=" $
		L.takeWhile (/='\n') $
		content

writeHiddenFile :: Path -> Settings -> Path -> Path -> ErrT IO ()
writeHiddenFile configDir settings src _ = do
	lift $
		writeFile (path_toStr $ configDir </> filename src <.> T.pack hiddenFileEnding) $
			hiddenFileContent settings src

list :: Path -> ErrT IO [Entry]
list configDir =
	do
		allFiles <-
			ExceptT $
			(liftM Right $ getDirectoryContents $ path_toStr configDir)
				`catchIOError` (\e -> return $ Left $ "error listing entries: " ++ show e)
		mapM (loadHiddenFile configDir) $
			map dropExtension $
			filter (maybe False ((==hiddenFileEnding) . T.unpack) . extension) $
			map path_fromStr $
			allFiles
	--return $ []

hiddenFileContent :: Settings -> Path -> String
hiddenFileContent _ src =
	"ORIGIN=" ++ path_toStr src ++ "\n"
