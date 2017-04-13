{-# LANGUAGE TupleSections #-}
module UserInput(
	UserInput(..),
	userInputFromCmdArgs,
	calcConfigDir
	) where

import Programs.InOut.Params
import UserInput.Types
import Utils

import Options.Applicative as Opt

import System.Environment( lookupEnv )
import System.Directory( getHomeDirectory )
import Data.Foldable( asum )
import Data.Monoid
import Data.List

--import qualified Paths_sgcheck2 as SGCHECK

import qualified Text.PrettyPrint.ANSI.Leijen as Doc


prgName :: String
prgName = "sgcheck2"

envVarConfigDir :: String
envVarConfigDir = "SGCHECK2_CONFIGPATH"

configDir_filename :: Path
configDir_filename = 
	".sgcheck2"

defConfigDir :: IO Path
defConfigDir =
	liftM2 (++)
		getHomeDirectory
		(return configDir_filename)


calcConfigDir :: Maybe Path -> ErrT IO Path
calcConfigDir mPathFromOptions =
	asum $ -- choose first strategy that works:
	[ maybe (throwE $ "config dir not specified!") return $ mPathFromOptions
	, lookupConfigDirFromEnv
	, lift $ defConfigDir
	]

lookupConfigDirFromEnv :: ErrT IO Path
lookupConfigDirFromEnv =
	ExceptT $
	fmap (maybeToEither "not installed correctly") $
	lookupEnv envVarConfigDir


userInputFromCmdArgs :: IO UserInput
userInputFromCmdArgs =
	Opt.execParser $ Opt.info (inputParser <**> Opt.helper) mainInformation

inputParser :: Opt.Parser UserInput
inputParser =
	UserInput
		<$> commandParser
		<*> option (Just <$> str) (long "config" <> short 'c' <> value Nothing <> metavar "CONFIG_DIR" <> help "the location of the config dir")

commandParser :: Opt.Parser Command
commandParser =
	Opt.hsubparser $
		(command "out" $ Opt.info ((CmdOut <$> outParamsParser)) out_info)
		<> (command "in" $ Opt.info ((CmdIn <$> inParamsParser)) in_info)
		<> (command "list" $ Opt.info ((CmdListFiles <$> listParamsParser)) list_info)
		<> (command "showConfig" $ Opt.info (pure CmdShowConfig) showConfig_info)
		<> (command "writeConfig" $ Opt.info (pure CmdWriteConfig) writeConfig_info)

inParamsParser :: Opt.Parser CopyCommandParams
inParamsParser =
	copyParamsParser
		(metavar "FILE" <> help "the file to check in (relative to the \"thisPath\" location" )

outParamsParser :: Opt.Parser CopyCommandParams
outParamsParser =
	copyParamsParser
		(metavar "FILE_AT_ORIGIN" <> help "the file to check out (relative to the \"serverPath\" location)" )

copyParamsParser :: Opt.Mod ArgumentFields String -> Opt.Parser CopyCommandParams
copyParamsParser fileInfo=
	CopyCommandParams
		<$> Opt.argument Opt.str fileInfo
		<*> (
			CopyFlags
				<$> switch (long "simulate" <> short 's' <> help "do not execute on filesystem")
				<*> switch (long "print-command" <> short 'p' <> help "print rsync command being run")
				<*> switch (long "print-rsync-output" <> short 'r' <> help "print rsync output")
				<*> option auto (long "rsync-opts" <> value [] <> metavar "RSYNC_OPTS" <> help "additional options to be passed to the copy command (rsync)")
		)

listParamsParser :: Opt.Parser ListParams
listParamsParser =
	fmap (\l -> if null l then defListParamsMarkChanged else l) $
	many $
		(SimpleOutput <$> simpleOutputInfoParser)

	{-
	[ Option ['m'] ["mark-changed"] (ReqArg (\str _ -> return $ uncurry defListParamsMarkChangedWithMarker $ markInfoFromStr str) "locally,onServer") "mark files which have changed locally/on the server"
	, Option ['r'] ["output-rsync"] (NoArg (\_ -> return $ defListParamsRSyncOut)) "append rsync output"
	-}

-- for each entry: print the entry using the followin options:
simpleOutputInfoParser :: Opt.Parser SimpleOutputInfo
simpleOutputInfoParser =
	Opt.option (Str <$> str) (long "output-string" <> help "add a string (useful as a seperator)")
	<|> Opt.flag' Path (long "output-path" <> help "add a path as it could be used for \"in\"")
	<|> Opt.flag' ThisPath (long "output-thispath" <> help "add local path" )
	<|> Opt.flag' ServerPath (long "output-serverpath" <> help "add path server" )

mainInformation :: Opt.InfoMod a
mainInformation =
	mconcat $
	[ Opt.fullDesc
	, Opt.headerDoc $ Just $
		Doc.vsep . map Doc.text $
		[ "-----------------------------------------------"
		, prgName
		, "-----------------------------------------------"
		]
	, Opt.progDesc $
		intercalate ". " $
		[ "Synchronize files between two directories (possibly remote) while automatically memorizing their locations"
		, "These two directories are called \"serverPath\" and \"thisPath\" and are specified using the config file"
		, "Files can be \"checked out\" from the server, which will also memorize their original path"
		, "Files which have been checked out this way can be \"checked in\" to their original location on the server"
		]
	, Opt.footerDoc $ Just $
		configHelp
		Doc.<$>
		(Doc.vsep . map Doc.text)
		[ "try"
			, "\t" ++ (cmdLineInput $ concat [ prgName, " CMD --help"])
		, "to get help for a specific command"
		, ""
		]
	]

out_info :: Opt.InfoMod a
out_info =
	Opt.fullDesc
	<> Opt.progDesc "check out a file (or directory) from server and memorize it"

in_info :: Opt.InfoMod a
in_info =
	Opt.fullDesc
	<> Opt.progDesc "check in a memorized file (or directory)"

list_info :: Opt.InfoMod a
list_info =
	Opt.fullDesc
	<> Opt.progDesc "list memorized files (or directories)"

showConfig_info :: Opt.InfoMod a
showConfig_info =
	Opt.fullDesc
	<> Opt.progDesc "show the location of the config used"

writeConfig_info :: Opt.InfoMod a
writeConfig_info =
	Opt.fullDesc
	<> Opt.progDesc "write a new config directory"

configHelp :: Doc.Doc
configHelp =
	let configDir = "on linux: ~/" ++ configDir_filename in
	Doc.vsep  . map Doc.text $
	[ "files:"
	, "the path of the config dir is determined by trying the following"
	, "  * use the parameter of the command line option -c|--config-dir, if existent"
	, "  * use the path of the environment variable $" ++ envVarConfigDir
	, "  * use the default path \"" ++ configDir ++ "\""
	, ""
	, "if the directory doesn't exist, you will get an error. to create the config dir, and write some default config file, use"
	, "\t" ++  cmdLineInput (concat $ [prgName, " ", cmdType_toStr WriteConfig] )
	]

cmdLineInput :: String -> String
cmdLineInput = ("$> "++)
