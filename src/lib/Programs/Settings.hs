module Programs.Settings where

import Data.Settings
import Utils
--import Data

import Control.Monad.Trans.Maybe


showSettings :: Settings -> MaybeT (ErrT IO) Settings
showSettings settings = do
	lift2 $ putStrLn $ settings_toStr settings
	MaybeT $ return Nothing

{-
setup params settings = do
	lift2 $ putStrLn $ settingsToString settings
	lift2 $ putStr "> "
	userInput <- lift2 $ getLine
	case userInput of
		"" -> return settings
		_ -> do
			changeSettingsFunc <- lift $ ExceptT $ return $ settingsChangeFromString userInput
			setup params $ changeSettingsFunc settings
-}
