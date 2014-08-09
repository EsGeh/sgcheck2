module Main where

import Programs.Settings
import Programs.InOut

import Global
import Args
import Config

import Text.Read(readEither)
import Control.Monad
import Control.Monad.Trans.Maybe


main :: IO ()
main = do
	eitherErr <- runExceptT execProgram
	case eitherErr of
		Left err -> putStrLn $ err
		Right _ -> return ()

execProgram :: ErrT IO ()
execProgram = do
	(settings,cmd,params) <- (loadConfig >>= paramsAndSettingsFromArgs) -- :: IO (Either String Settings)
	let settings' = execFromCmd cmd params settings
	maybeNewSettings <- runMaybeT settings'
	case maybeNewSettings of
		Nothing -> return ()
		Just newSettings -> storeConfig newSettings

cmdList :: [(String, Parameters -> Settings -> MaybeT (ErrT IO) Settings)]
cmdList = [
	("show", showSettings),
	("setup", setup),
	("out", checkOut),
	("in", checkIn)
	]

execFromCmd :: String -> Parameters -> Settings -> MaybeT (ErrT IO) Settings
execFromCmd cmdName params settings = do
	let mFunction = lookup cmdName cmdList
	maybe (lift $ throwE ("unknown command \"" ++ cmdName ++ "\"") >> return settings)
		(\f -> f params settings)
		mFunction


{-
errIO :: ErrT IO x -> IO x
errIO val = do
	eitherVal <- runErrorT val
	case eitherVal of
		Left err -> putStrLn err
-}


