{-# LANGUAGE TupleSections #-}
module UserInput(
	UserInput(..),
	userInputFromCmdArgs,
	calcConfigDir
	) where

import Programs.InOut.Params
import UserInput.Types
import Utils

import System.Console.GetOpt
import qualified Data.List as List

import System.Environment( lookupEnv )
import System.Directory( getHomeDirectory )


prgName :: String
prgName = "sgcheck2"

envVarConfigDir :: String
envVarConfigDir = "SGCHECK2_CONFIGPATH"

calcConfigDir :: Maybe Path -> ErrT IO Path
calcConfigDir mPathFromOptions =
	firstThatWorks $
		[ maybe (throwE $ "config dir not specified!") return $ mPathFromOptions
		, lookupConfigDirFromEnv
		, lift $ defConfigDir
		]
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

lookupConfigDirFromEnv :: ErrT IO Path
lookupConfigDirFromEnv =
	ExceptT $
	liftM (
		(maybeToEither "not installed correctly")
		.
		fmap path_fromStr
		) $
	lookupEnv envVarConfigDir

defConfigDir :: IO Path
defConfigDir =
	liftM path_fromStr $
	liftM2 (++)
		getHomeDirectory
		(return "/.sgcheck2")


type HelpOrM a = Maybe a


userInputFromCmdArgs :: [String] -> ErrT IO UserInput
userInputFromCmdArgs args =
	do
		cd <- lift defConfigDir
		(cmdType, rest) <-
			case parseCmd args of
				Nothing ->
					throwE $ generalHelpStr cd generalOptDescr
				Just x ->
					x `catchE` \msg -> throwE $ unlines [ msg, generalHelpInfo ]
		case cmdType of
			x | x `elem` [In, Out] ->
				flip (parseToInput cmdType optDescr (defCopyFlags, defGeneralOptions) (parseFile cmdType)) rest $
					\(copyFlags, generalOpts) file ->
						UserInput {
							ui_cmd =
								(case cmdType of { Out -> CmdOut; In -> CmdIn; _ -> error "" })
									CopyCommandParams {
										copyCmd_file = file,
										copyCmd_flags = copyFlags
									},
							ui_configDir = genOpts_configDir $ generalOpts
						}
					where
						optDescr =
							map (fmap $ mapToFstM) copyOptDescr
							++
							map (fmap $ mapToSndM) generalOptDescr
			ListFiles ->
				flip (parseToInput cmdType optDescr (defListParamsMarkChanged,defGeneralOptions) (parseNoArgs cmdType)) rest $
					\opts _ ->
						UserInput {
							ui_cmd =
								CmdListFiles $
									fst opts,
							ui_configDir = genOpts_configDir $ snd opts
						}
					where
						optDescr =
							map (fmap $ mapToFstM) listFilesOptDescr
							++
							map (fmap $ mapToSndM) generalOptDescr
			_ -> 
				flip (parseToInput cmdType optDescr defGeneralOptions (parseNoArgs cmdType)) rest $
				\opts _ ->
					UserInput {
						ui_cmd =
							(case cmdType of { ShowConfig -> CmdShowConfig; WriteConfig -> CmdWriteConfig; _ -> error "" }),
						ui_configDir = genOpts_configDir $ opts
					}
				where
					optDescr =
						generalOptDescr

parseToInput ::
	CommandType
	-> [OptDescr (options -> HelpOrM options)]
	-> options
	-> ([String] -> ExceptT String IO t)
	-> (options -> t -> r)
	-> [String]
	-> ExceptT String IO r
parseToInput cmdType optDescr defOpts parseArgs calcInput args =
	do
		cd <- lift defConfigDir
		(opts, nonOpts) <-
			case parseOptions defOpts optDescr args of
				Nothing -> 
					throwE $ showHelp cd cmdType optDescr
				Just copyOpts' ->
					copyOpts' `catchE` \msg -> throwE $ unlines [ msg, helpInfo cmdType ]
		params <- parseArgs nonOpts
		return $
			calcInput opts params

listFilesOptDescr :: [OptDescr (ListParams -> HelpOrM ListParams)]
listFilesOptDescr =
	[ Option ['m'] ["mark-changed"] (ReqArg (\str _ -> return $ uncurry defListParamsMarkChangedWithMarker $ markInfoFromStr str) "locally,onServer") "mark files which have changed locally/on the server"
	, Option ['r'] ["output-rsync"] (NoArg (\_ -> return $ defListParamsRSyncOut)) "append rsync output"
	, Option ['o'] ["output"] (NoArg (\_ -> return $ [])) "output for each entry will be defined by the following options"
	, Option [] ["output-string"] (ReqArg (\str -> return . (++ [SimpleOutput $ Str str])) "any string") "add a string to each entry"
	, Option [] ["output-path"] (NoArg (return . (++ [SimpleOutput $ Path]))) "add the path to each entry"
	, Option [] ["output-thispath"] (NoArg (return . (++ [SimpleOutput $ ThisPath]))) "add the local path to each entry"
	, Option [] ["output-serverpath"] (NoArg (return . (++ [SimpleOutput $ ServerPath]))) "add the path on the server to each entry"
	]

markInfoFromStr :: String -> (String, String)
markInfoFromStr = mapToSnd (drop 1) . span (/=',')

{-
	[ Option [] ["mark-local"] (ReqArg (\str o -> return $ o{ simpleListDescr_markChangedLocally = str}) "MARKER") "do not execute"
	]
-}

copyOptDescr :: [OptDescr (CopyFlags -> HelpOrM CopyFlags)]
copyOptDescr =
	[ Option ['s'] ["simulate"] (NoArg (\o -> return $ o{ copyFlags_simulate = True})) "do not execute"
	, Option ['p'] ["print-command"] (NoArg (\o -> return $ o{ copyFlags_printCommand = True})) "do not execute"
	, Option [] ["rsync-opts"] (ReqArg (\str o -> return $ copyFlags_mapToRSyncOpts (++[str]) o) "COPY_OPTIONS") "additional options to the copy command"
	]

generalOptDescr :: [OptDescr (GeneralOptions -> HelpOrM GeneralOptions)]
generalOptDescr =
	[ Option ['c'] ["config"] (ReqArg (\str o -> return $ o{ genOpts_configDir = Just $ path_fromStr str }) "CONFIG_DIR") "the location of the config dir"
	, Option ['h'] ["help"] (NoArg (const $ Nothing)) "print help"
	]

{-
packCopyFlags :: (CopyFlags -> HelpOrM CopyFlags)
	-> (CopyOptions -> HelpOrM CopyOptions)
packCopyFlags g opts =
	copyOpts_mapToCopyFlagsM g $ opts

packGeneralOptions :: (GeneralOptions -> HelpOrM GeneralOptions)
	-> (CopyOptions -> HelpOrM CopyOptions)
packGeneralOptions g opts =
	copyOpts_mapToGeneralM g $ opts
{-
	case opts of
		Copyfmap g opts
-}
-}

parseCmd :: Monad m => [String] -> Maybe (ErrT m (CommandType, [String]))
parseCmd args =
	case args of
		(cmd:_)
			| cmd `elem` ["-h", "--help"] -> Nothing
		(cmd:rest) -> 
			Just $
				maybe (throwE $ "command " ++ cmd ++ " not found!") (return . (,rest)) $
				cmdType_fromStr cmd
		_ ->
			Just $ throwE $ "command expected"

parseOptions :: Monad m => options -> [OptDescr (options -> HelpOrM options)] -> [String] -> Maybe (ErrT m (options, [String]))
parseOptions defOptions optDescr args =
	let optReturn = getOpt RequireOrder optDescr args
	in
		case optReturn of
			(opts', nonOpts, []) ->
				let mOpts = foldl (>>=) (return defOptions) opts' -- :: Maybe Options
				in
					case mOpts of
						Nothing ->
							Nothing
						Just opts ->
							Just $ return $ (opts, nonOpts)
			(_, _, errMsgs) ->
				Just $ throwE $ unlines errMsgs

parseNoArgs :: Monad m => CommandType -> [String] -> ErrT m ()
parseNoArgs cmdType args =
	case args of
		[] -> return $ ()
		_ -> 
			throwE $ List.concat ["wrong parameters for ", cmdType_toStr cmdType]

parseFile :: Monad m => CommandType -> [String] -> ErrT m Path
parseFile cmdType args =
	case args of
		[file] -> return $ path_fromStr file
		_ -> 
			throwE $ List.concat ["wrong parameters for ", cmdType_toStr cmdType]

helpInfo :: CommandType -> String
helpInfo cmdType =
	unlines $
	[ "use "
	, "\t" ++ cmdLineInput (concat $ [prgName, " ", cmdType_toStr cmdType, " --help"])
	, " to get more info"
	]

generalHelpInfo :: String
generalHelpInfo =
	unlines $
	[ "use "
	, "\t" ++ cmdLineInput (concat $ [prgName, " --help"])
	, " to get more info"
	]

generalHelpStr :: Path -> [OptDescr a] -> String
generalHelpStr configDir optDescr =
	unlines $
	[ generalSyntaxStr
	, ""
	, usageInfo "general OPTIONS: " optDescr
	, concat $ [ "CMD: " ]
	, unlines $ map ( ("\t"++) . cmdType_toStr) $ cmdType_listAll
	, ""
	, "try"
		, "\t" ++ (cmdLineInput $ concat [ prgName, " CMD --help"])
	, "to get help for a specific command"
	, ""
	, configHelp configDir
	]

showHelp :: Path -> CommandType -> [OptDescr b] -> String
showHelp configDir cmdType optDescr =
	unlines $
	[ syntaxStr cmdType
	, ""
	, usageInfo "OPTIONS for this command: " optDescr
	, ""
	, configHelp configDir
	]

generalSyntaxStr :: String
generalSyntaxStr =
	concat $ [ "syntax: ", prgName, " CMD [OPTIONS] [PARAMS]" ]

syntaxStr :: CommandType -> String
syntaxStr cmdType =
	concat $
	[ "syntax: ", prgName, " ", cmdType_toStr cmdType, " [OPTIONS] "
	, case cmdType of
			In -> "<file>"
			Out -> "<file>"
			_ -> ""
	]

configHelp :: Path -> String
configHelp configDir =
	unlines $
	[ "files:"
	, "the path of the config dir is determined by trying the following"
	, "  * use the parameter of the command line option -c|--config-dir, if existent"
	, "  * use the path of the environment variable $" ++ envVarConfigDir
	, "  * use the default path \"" ++ path_toStr configDir ++ "\""
	, ""
	, "if the directory doesn't exist, you will get an error. to create the config dir, and write some default config file, use"
	, "\t" ++  cmdLineInput (concat $ [prgName, " ", cmdType_toStr WriteConfig] )
	]

cmdLineInput :: String -> String
cmdLineInput = ("$> "++)
