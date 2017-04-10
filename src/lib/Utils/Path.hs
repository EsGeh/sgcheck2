module Utils.Path(
	Path,
	path_toStr, path_fromStr,
	path_isEmpty,
	path_isValid,
	Path.root,
	Path.directory,
	Path.parent,
	Path.filename,
	Path.dirname,
	Path.basename,
	Path.absolute,
	Path.relative,
	Path.stripPrefix,
	Path.splitDirectories,
	Path.concat,
	extension,
	-- Path.extension,
	Path.dropExtension,
	(Path.</>), (<.>),
	--(Path.</>), (Path.<.>),
) where

import qualified Filesystem.Path as Path
import qualified Filesystem.Path.CurrentOS as Path

import qualified Data.Text as T


(<.>) :: Path -> String -> Path
x <.> y = x Path.<.> T.pack y

type Path = Path.FilePath

path_toStr :: Path -> String
path_toStr = Path.encodeString
path_fromStr :: String -> Path
path_fromStr = Path.decodeString

path_isValid :: Path -> Bool
path_isValid path =
	Path.valid path && path_hasNoNewlines path

path_isEmpty :: Path -> Bool
path_isEmpty =
	(=="") . path_toStr

path_hasNoNewlines :: Path -> Bool
path_hasNoNewlines =
	not . ('\n' `elem`) . path_toStr

extension :: Path -> Maybe String
extension = (fmap T.unpack . ) Path.extension
