{-# LANGUAGE TupleSections #-}
module UserInput(
	UserInput(..),
	Command(..), CommandType(..), HasCommmandType(..),
	userInputFromCmdArgs
	) where

import Data
import UserInput.Types
import Global

import System.Console.GetOpt
import qualified Data.List as List


type HelpOrM a = Maybe a

{-
optDescr :: CommandType -> [OptDescr (CopyOptions -> HelpOrM CopyOptions)]
optDescr cmdType =
	case cmdType of
		c | c `elem` [Out, In] ->
			map (fmap $ packCopyFlags) copyOptDescr
			++
			map (fmap $ packGeneralOptions) generalOptDescr
		_ ->
			map (fmap $ packGeneralOptions) generalOptDescr
-}

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
				do
					(copyOpts, nonOpts) <-
						case parseOptions defCopyOptions optDescr rest of
							Nothing -> 
								throwE $ showHelp cd cmdType generalOptDescr optDescr
							Just copyOpts' ->
								copyOpts' `catchE` \msg -> throwE $ unlines [ msg, helpInfo cmdType ]
					file <- parseFile cmdType nonOpts
					return $
						UserInput {
							ui_cmd =
								(case cmdType of { Out -> CmdOut; In -> CmdIn; _ -> error "" })
									CopyCommandParams {
										copyCmd_file = file,
										copyCmd_flags = opts_copyFlags copyOpts
									},
							ui_configDir = genOpts_configDir $ opts_general copyOpts
						}
					where
						optDescr =
							map (fmap $ packCopyFlags) copyOptDescr
							++
							map (fmap $ packGeneralOptions) generalOptDescr
			_ -> 
				do
					(copyOpts, nonOpts) <-
						case parseOptions defGeneralOptions optDescr rest of
							Nothing -> 
								throwE $ showHelp cd cmdType generalOptDescr []
							Just copyOpts' ->
								copyOpts' `catchE` \msg -> throwE $ unlines [ msg, helpInfo cmdType ]
					when (nonOpts /= []) $ throwE $ unlines $ [List.concat ["unexpected argument for ", cmdType_toStr cmdType], helpInfo cmdType]
					return $
						UserInput {
							ui_cmd =
								(case cmdType of { ListFiles -> CmdListFiles; ShowConfig -> CmdShowConfig; WriteConfig -> CmdWriteConfig; _ -> error "" }),
							ui_configDir = genOpts_configDir $ copyOpts
						}
					where
						optDescr =
							generalOptDescr

copyOptDescr :: [OptDescr (CopyFlags -> HelpOrM CopyFlags)]
copyOptDescr =
	[ Option ['s'] ["simulate"] (NoArg (\o -> return $ o{ copyFlags_simulate = True})) "do not execute"
	, Option ['p'] ["print-command"] (NoArg (\o -> return $ o{ copyFlags_printCommand = True})) "do not execute"
	]

generalOptDescr :: [OptDescr (GeneralOptions -> HelpOrM GeneralOptions)]
generalOptDescr =
	[ Option ['c'] ["config"] (ReqArg (\str o -> return $ o{ genOpts_configDir = Just $ path_fromStr str }) "CONFIG_DIR") "the location of the config dir"
	, Option ['h'] ["help"] (NoArg (const $ Nothing)) "print help"
	]

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

parseFile :: Monad m => CommandType -> [String] -> ErrT m Path
parseFile cmdType nonOpts =
	case nonOpts of
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
generalHelpStr defConfigDir generalOptDescr =
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
	, configHelp defConfigDir
	]

showHelp :: Path -> CommandType -> [OptDescr a] -> [OptDescr b] -> String
showHelp defConfigDir cmdType generalOptDescr optDescr =
	unlines $
	[ syntaxStr cmdType
	, ""
	, usageInfo "specific OPTIONS for this command: " optDescr
	, usageInfo "general OPTIONS: " generalOptDescr
	, ""
	, configHelp defConfigDir
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
configHelp defConfigDir =
	unlines $
	[ "files:"
	, "the path of the config dir is determined by trying the following"
	, "  * use the parameter of the command line option -c|--config-dir, if existent"
	, "  * use the path of the environment variable $" ++ envVarConfigDir
	, "  * use the default path \"" ++ path_toStr defConfigDir ++ "\""
	, ""
	, "if the directory doesn't exist, you will get an error. to create the config dir, and write some default config file, use"
	, "\t" ++  cmdLineInput (concat $ [prgName, " ", cmdType_toStr WriteConfig] )
	]

cmdLineInput :: String -> String
cmdLineInput = ("$> "++)

{-
class CmdDescr cmd opts args where
	cmdDescr_cmd :: cmd
	cmd_
-}

{-
{-
1. try from options
2. try from env var
3. try default config dir
-}

data UserInput
	= UserInput {
		ui_cmd :: Command,
		ui_configDir :: Maybe Path
	}
	deriving( Show )

data Command
	= CmdOut Path
	| CmdIn Path
	| CmdListFiles
	| CmdShowConfig
	| CmdWriteConfig
	deriving( Show )

{-
-}

data CommandType
	= Out
	| In
	| ListFiles
	| ShowConfig
	| WriteConfig
	deriving( Eq, Ord, Enum, Bounded, Show )

cmdType_listAll :: [CommandType]
cmdType_listAll = [(minBound :: CommandType) .. ]

class HasCommmandType a where
	cmd_type :: a -> CommandType

instance HasCommmandType Command where
	cmd_type cmd =
		case cmd of
			CmdOut _ -> Out
			CmdIn _ -> In
			CmdListFiles -> ListFiles
			CmdShowConfig -> ShowConfig
			CmdWriteConfig -> WriteConfig

cmdType_fromStr :: String -> Maybe CommandType
cmdType_fromStr str =
	lookup str cmdList

cmdType_toStr :: CommandType -> String
cmdType_toStr c =
	maybe (error "cmdType_fromStr error!") id $
	lookup c $ map swap cmdList


userInputFromCmdArgs :: [String] -> ErrT IO UserInput
userInputFromCmdArgs args =
	do
		cd <- lift defConfigDir
		(cmdType, rest) <-
			case parseCmd args of
				Nothing ->
					throwE $ generalHelpStr cd
				Just x ->
					x `catchE` \msg -> throwE $ unlines [ msg, generalHelpInfo ]
		(options, nonOpts) <-
			case parseOptions cmdType rest of
				Nothing -> 
					throwE $ showHelp cd cmdType
				Just x ->
					x `catchE` \msg -> throwE $ unlines [ msg, helpInfo cmdType ]
		cmd <-
			parseParams cmdType nonOpts
				`catchE` \msg ->  throwE $ unlines [ msg, helpInfo cmdType ]
		{-
		when (cmd_type cmd == PrintHelp) $
			throwE $ generalHelpStr defConfigDir
		-}
		return $ UserInput {
			ui_cmd = cmd,
			ui_configDir = opt_configDir options
		}

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

parseOptions :: Monad m => CommandType -> [String] -> Maybe (ErrT m (Options, [String]))
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

parseParams :: Monad m => CommandType -> [String] -> ErrT m Command
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

data Options =
	Options {
		opt_configDir :: Maybe Path
	}

defOptions :: Options
defOptions = Options $ Nothing

optDescr :: CommandType -> [OptDescr (Options -> HelpOrM Options)]
optDescr _ = []

generalOptDescr :: [OptDescr (Options -> HelpOrM Options)]
generalOptDescr =
	[ Option ['c'] ["config"] (ReqArg (\str o -> return $ o{ opt_configDir = Just $ path_fromStr str }) "CONFIG_DIR") "the location of the config dir"
	, Option ['h'] ["help"] (NoArg (const $ Nothing)) "print help"
	]
-}
