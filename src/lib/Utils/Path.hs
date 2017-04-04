module Utils.Path(
	Path,
	path_toStr, path_fromStr,
	path_isValid,
	Path.root,
	Path.directory,
	Path.parent,
	Path.filename,
	Path.dirname,
	Path.basename,
	Path.absolute,
	Path.relative,
	Path.extension,
	Path.dropExtension,
	(Path.</>), (Path.<.>),
) where

import qualified Filesystem.Path as Path
import qualified Filesystem.Path.CurrentOS as Path


type Path = Path.FilePath

path_toStr :: Path -> String
path_toStr = Path.encodeString
path_fromStr :: String -> Path
path_fromStr = Path.decodeString

path_isValid path =
	Path.valid path && path_hasNoNewlines path

path_hasNoNewlines =
	not . ('\n' `elem`) . path_toStr
