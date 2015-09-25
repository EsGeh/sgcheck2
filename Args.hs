module Args(
	userInputFromCmdArgs
	) where

import Global
import Data
import Config

import System.Console.GetOpt
import System.Environment
import System.Directory
import Data.List


envVarConfigDir = "SGCHECK2_CONFIGPATH"
defConfigDir :: IO Path
defConfigDir =
	liftM path_fromStr $
	liftM2 (++)
		getHomeDirectory
		(return "/.sgcheck2")

{-
1. try from options
2. try from env var
3. try default config
-}

userInputFromCmdArgs :: [String] -> ErrT IO Data
userInputFromCmdArgs args =
	do
		inputData <-
			either
				(\e -> throwE e)
				return
				(runExcept $ parseCommandArgs args)
		configPath <-
			firstThatWorks $
				[ maybe (throwE "") return $ id_configFile inputData
				, lookupConfigDirFromEnv
				, lift $ defConfigDir
				]
		lift $ putStrLn $ "config path: " ++ path_toStr configPath
		settings <-
			if cmd_type (id_cmd inputData) /= WriteConfig
				then loadConfig configPath
				else return $ defSettings
		return $
			Data {
				data_programInput =
					CommandArgs {
						cmdArgs_cmd = id_cmd inputData,
						cmdArgs_settings = settings,
						cmdArgs_memorizeFile =
							Config.writeHiddenFile configPath,
						cmdArgs_lookupFile =
							Config.loadHiddenFile configPath
					},
				data_storeSettings =
					storeConfig configPath
			}
	where
		firstThatWorks :: [ErrT IO a] -> ErrT IO a
		firstThatWorks =
			foldl conc (throwE "no strategy to find config path")
			where
				conc a b =
					do
						a' <-
							lift $ runExceptT a
						case a' of
							Left _ -> b
							Right x -> return x

data InputData
	= InputData {
		id_cmd :: Command,
		id_configFile :: Maybe Path
	}

parseCommandArgs :: [String] -> ErrM InputData
parseCommandArgs args =
	do
		(cmdType, rest) <- parseCmd args
		(options, nonOpts) <- parseOptions cmdType rest
		cmd <- parseParams cmdType nonOpts
		return $ InputData {
			id_cmd = cmd,
			id_configFile = opt_configFile options
		}
	where
		parseParams cmdType params =
				case (cmdType, params) of
					(Out, [param]) ->
						return $ CmdOut $ path_fromStr param
					(In, [param]) ->
						return $ CmdIn $ path_fromStr param
					(ListFiles, []) ->
						return $ CmdListFiles
					(ShowConfig, []) ->
						return $ CmdShowConfig
					(WriteConfig, []) ->
						return $ CmdWriteConfig
					_ ->
						throwE $ Data.List.concat ["wrong parameters for ", cmdType_toStr cmdType]
		parseCmd args =
			case span (not . isPrefixOf "-") args of
				([cmd], restArgs) ->
					maybe (throwE $ "command " ++ cmd ++ " not found!") return $
					(mapToFstM cmdType_fromStr (cmd, restArgs) :: Maybe (CommandType, [String]))
				_ ->
					throwE $ generalHelpStr
		parseOptions cmdType args =
			let optReturn = getOpt RequireOrder (generalOptDescr ++ optDescr cmdType) args
			in
				case optReturn of
					(opts', nonOpts, []) ->
						let mOpts = foldl (>>=) (return defOptions) opts' :: Maybe Options
						in
							case mOpts of
								Nothing ->
									throwE $ showHelp cmdType
								Just opts ->
									return $ (opts, nonOpts)
					(_, _, errMsgs) -> throwE $ unlines [ unlines errMsgs, generalHelpStr ]

data Options =
	Options {
		opt_configFile :: Maybe Path
	}

defOptions = Options $ Nothing

type HelpOrM a = Maybe a
-- type HelpOrM a = Except CommandType a

optDescr _ = []

generalOptDescr :: [OptDescr (Options -> HelpOrM Options)]
generalOptDescr =
	[ Option ['c'] ["config"] (ReqArg (\str o -> return $ o{ opt_configFile = Just $ path_fromStr str }) "CONFIG_FILE") "the location of the config file"
	, Option ['h'] ["help"] (NoArg (const $ Nothing)) "print help"
	]

showHelp cmdType =
	unlines $
	[ syntaxStr cmdType
	, usageInfo "general OPTIONS: " generalOptDescr
	, usageInfo "specific OPTIONS for this command: " (optDescr cmdType)
	]

generalHelpStr =
	unlines $
	[ generalSyntaxStr
	, usageInfo "general OPTIONS: " generalOptDescr
	, concat $ [ "CMD: " ]
	, unlines $ map ( ("\t"++) . cmdType_toStr) $ cmdType_listAll
	, ""
	, "try"
		, concat [ "\t$> ", prgName, " CMD --help"]
	, "to get help for a specific command"
	]

generalSyntaxStr =
	concat $ [ "syntax: ", prgName, " CMD [OPTIONS] [PARAMS]" ]

syntaxStr cmdType =
	concat $
	[ "syntax: ", prgName, " ", cmdType_toStr cmdType, " [OPTIONS] "
	, case cmdType of
			In -> "<file>"
			Out -> "<file>"
			_ -> ""
	]

lookupConfigDirFromEnv :: ErrT IO Path
lookupConfigDirFromEnv = do
	let lookup = (do
		fromEnv <- lookupEnv envVarConfigDir
		def <- defConfigDir
		return $ fmap path_fromStr fromEnv `mplus` Just def) :: IO (Maybe Path)
	ExceptT $ liftM (maybeToEither "not installed correctly") lookup :: ErrT IO Path

{-
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
			return $ foldr (.) id options $ Parameters $ path_fromStr ""
		--(options, nonOptions, unrecognizedOptions, errMessages) ->
		_ -> throwE $ usageStringCMD cmd
	return (cmd, params)

usageStringCMD cmd = usageInfo "usage: sgcheck2 CMD OPTIONS" (optDescr cmd)
-}
