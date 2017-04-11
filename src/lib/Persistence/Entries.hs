module Persistence.Entries(
	writeHiddenFile, loadHiddenFile,
	list
) where

import Data.Settings
import Persistence.Global
import Utils

import System.Directory
import System.IO.Error

import System.FilePath as Path( (</>), (<.>) )
import qualified System.FilePath as Path

type Path = Path.FilePath


-- returns the origin:
loadHiddenFile :: Path -> Path -> ErrT IO Entry
loadHiddenFile configDir name =
	fmap Entry $
	liftIO $ readFile $ configDir </> name <.> hiddenFileEnding
{-
	(content :: String) <- lift $
		--fmap read $
		readFile $ configDir </> name <.> hiddenFileEnding
	ExceptT $ return $
		maybeToEither "couldn't read hidden file!" $
		liftM Entry $
		content
		{-
		L.stripPrefix "ORIGIN=" $
		L.takeWhile (/='\n') $
		content
		-}
	-}

writeHiddenFile :: Path -> Settings -> Path -> Path -> ErrT IO ()
writeHiddenFile configDir _ src _ = do
	lift $
		writeFile (configDir </> Path.takeFileName src <.> hiddenFileEnding) $
		src
		--show entry
			-- hiddenFileContent settings src

list :: Path -> ErrT IO [Entry]
list configDir =
	do
		allFiles <-
			ExceptT $
			(liftM Right $ getDirectoryContents $  configDir)
				`catchIOError` (\e -> return $ Left $ "error listing entries: " ++ show e)
		mapM (loadHiddenFile configDir) $
			map Path.dropExtension $
			filter ((==hiddenFileEnding) . Path.takeExtension) $
			--filter (maybe False ((==hiddenFileEnding) ) . Path.takeExtension) $
			allFiles
	--return $ []

{-
hiddenFileContent :: Settings -> Path -> String
hiddenFileContent _ src =
	"ORIGIN=" ++  src ++ "\n"
-}
