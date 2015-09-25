{-# LANGUAGE TupleSections #-}
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
3. try default config dir
-}

userInputFromCmdArgs :: [String] -> ErrT IO Data
userInputFromCmdArgs args =
	do
		defConfigDir' <- lift defConfigDir
		inputData <-
			either
				(\e -> throwE e)
				return
				(runExcept $ parseCommandArgs defConfigDir' args)
		configDir <-
			firstThatWorks $
				[ maybe (throwE $ "config dir not specified!") return $ id_configDir inputData
				, lookupConfigDirFromEnv
				, lift $ defConfigDir
				]
		--lift $ putStrLn $ "config dir: " ++ path_toStr configDir
		settings <-
			if cmd_type (id_cmd inputData) /= WriteConfig
				then loadConfig configDir
				else
					do
						createConfig configDir
						return $ defSettings
		return $
			Data {
				data_programInput =
					CommandArgs {
						cmdArgs_cmd = id_cmd inputData,
						cmdArgs_settings = settings,
						cmdArgs_memorizeFile =
							Config.writeHiddenFile configDir,
						cmdArgs_lookupFile =
							Config.loadHiddenFile configDir
					},
				data_storeSettings =
					storeConfig configDir
			}
	where
		firstThatWorks :: [ErrT IO a] -> ErrT IO a
		firstThatWorks =
			foldl conc (throwE "no strategy to find config dir")
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
		id_configDir :: Maybe Path
	}

parseCommandArgs :: Path -> [String] -> ErrM InputData
parseCommandArgs defConfigDir args =
	do
		(cmdType, rest) <-
			case parseCmd args of
				Nothing ->
					throwE $ generalHelpStr defConfigDir
				Just x ->
					x `catchE` \msg -> throwE $ unlines [ msg, generalHelpInfo ]
		(options, nonOpts) <-
			case parseOptions cmdType rest of
				Nothing -> 
					throwE $ showHelp defConfigDir cmdType
				Just x ->
					x `catchE` \msg -> throwE $ unlines [ msg, helpInfo cmdType ]
		cmd <-
			parseParams cmdType nonOpts
				`catchE` \msg ->  throwE $ unlines [ msg, helpInfo cmdType ]
		{-
		when (cmd_type cmd == PrintHelp) $
			throwE $ generalHelpStr defConfigDir
		-}
		return $ InputData {
			id_cmd = cmd,
			id_configDir = opt_configDir options
		}
	where
		parseCmd args =
			case args of
				(cmd:rest)
					| cmd `elem` ["-h", "--help"] -> Nothing
				(cmd:rest) -> 
					Just $
						maybe (throwE $ "command " ++ cmd ++ " not found!") (return . (,rest)) $
						cmdType_fromStr cmd
				_ ->
					Just $ throwE $ "command expected"
		parseOptions cmdType args =
			let optReturn = getOpt RequireOrder (generalOptDescr ++ optDescr cmdType) args
			in
				case optReturn of
					(opts', nonOpts, []) ->
						let mOpts = foldl (>>=) (return defOptions) opts' :: Maybe Options
						in
							case mOpts of
								Nothing ->
									Nothing
								Just opts ->
									Just $ return $ (opts, nonOpts)
					(_, _, errMsgs) ->
						Just $ throwE $ unlines errMsgs
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


helpInfo cmdType =
	unlines $
	[ "use "
	, "\t" ++ cmdLineInput (concat $ [prgName, " ", cmdType_toStr cmdType, " --help"])
	, " to get more info"
	]

generalHelpInfo =
	unlines $
	[ "use "
	, "\t" ++ cmdLineInput (concat $ [prgName, " --help"])
	, " to get more info"
	]

data Options =
	Options {
		opt_configDir :: Maybe Path
	}

defOptions = Options $ Nothing

type HelpOrM a = Maybe a

optDescr _ = []

generalOptDescr :: [OptDescr (Options -> HelpOrM Options)]
generalOptDescr =
	[ Option ['c'] ["config"] (ReqArg (\str o -> return $ o{ opt_configDir = Just $ path_fromStr str }) "CONFIG_DIR") "the location of the config dir"
	, Option ['h'] ["help"] (NoArg (const $ Nothing)) "print help"
	]

showHelp defConfigDir cmdType =
	unlines $
	[ syntaxStr cmdType
	, ""
	, usageInfo "general OPTIONS: " generalOptDescr
	, usageInfo "specific OPTIONS for this command: " (optDescr cmdType)
	, ""
	, configHelp $ path_toStr defConfigDir
	]

generalHelpStr defConfigDir =
	unlines $
	[ generalSyntaxStr
	, ""
	, usageInfo "general OPTIONS: " generalOptDescr
	, concat $ [ "CMD: " ]
	, unlines $ map ( ("\t"++) . cmdType_toStr) $ cmdType_listAll
	, ""
	, "try"
		, "\t" ++ (cmdLineInput $ concat [ prgName, " CMD --help"])
	, "to get help for a specific command"
	, ""
	, configHelp $ path_toStr defConfigDir
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

configHelp defConfigDir =
	unlines $
	[ "config dir:"
	, "the path of the config dir is determined by trying the following"
	, "  * use the parameter of the command line option -c|--config-dir, if existent"
	, "  * use the path of the environment variable $" ++ envVarConfigDir
	, "  * use the default path \"" ++ defConfigDir ++ "\""
	, ""
	, "if the directory doesn't exist, you will get an error. to create the config dir, and write some default config file, use"
	, "\t" ++  cmdLineInput (concat $ [prgName, " ", cmdType_toStr WriteConfig] )
	]

cmdLineInput = ("$> "++)

lookupConfigDirFromEnv :: ErrT IO Path
lookupConfigDirFromEnv = do
	let lookup = (do
		fromEnv <- lookupEnv envVarConfigDir
		def <- defConfigDir
		return $ fmap path_fromStr fromEnv `mplus` Just def) :: IO (Maybe Path)
	ExceptT $ liftM (maybeToEither "not installed correctly") lookup :: ErrT IO Path
