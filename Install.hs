

import System.Environment
import System.Directory


configPath = ""

main = do
	setEnv "SGCHECK2_CONFIGPATH" configPath
	createDirectoryIfMissing True $ configPath ++ "/.sgcheck"
