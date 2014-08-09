module Args( paramsAndSettingsFromArgs ) where

import Global

import System.Console.GetOpt
import System.Environment
import Data.List


paramsAndSettingsFromArgs :: Settings -> ErrT IO (Settings,Command,Parameters)
paramsAndSettingsFromArgs settings = do
	args <- lift $ getArgs
	do
		(cmd,params) <- paramsFromArgs args
		return (settings,cmd,params)


paramsFromArgs :: Monad m => [String] -> ErrT m (Command, Parameters)
paramsFromArgs args = do
	let (cmds, restArgs) = span (not . isPrefixOf "-") args
	when (length cmds /= 1) $ throwE usageString
	let (cmd, args) = (head cmds, restArgs) :: (String,[String])

	let optReturn = getOpt' RequireOrder (optDescr cmd) args
	params <- case optReturn of
		(options, [], [], []) ->
			return $ foldr (.) id options $ Parameters $ decodeString ""
		--(options, nonOptions, unrecognizedOptions, errMessages) ->
		_ -> throwE $ usageStringCMD cmd
	return (cmd, params)

usageString = "usage: sgcheck2 CMD OPTIONS"
usageStringCMD cmd = usageInfo "usage: sgcheck2 CMD OPTIONS" (optDescr cmd)

optDescr _ = [
	Option ['f'] ["file"] (ReqArg (\str params -> params{ file= decodeString str }) "FILE") "file"
	{-
	Option ['s'] ["src","source"] (ReqArg (\str params -> params{ src= str }) "SRC") "source file",
	Option ['d'] ["dest","destination"] (ReqArg (\str params -> params{ dest= str }) "DEST") "destination file"
	-}
	]

type Command = String
