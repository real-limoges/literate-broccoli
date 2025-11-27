#!/usr/bin/env stack
-- stack --resolver lts-22.11 script --package aeson --package process --package bytestring --package text --package lens --package lens-aeson

{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

import           Control.Concurrent         (threadDelay)
import           Control.Lens               ((^.), (^?))
import           Control.Monad              (forever, void)
import           Data.Aeson                 (Value, decode)
import           Data.Aeson.Lens            (_String, key, nth)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text                  as T
import           System.Exit                (ExitCode (..))
import           System.Info                (os)
import           System.IO.Error            (tryIOError)
import           System.Process             (callCommand, readProcess,
                                             readProcessWithExitCode)

data ServiceStatus
    = RunningHealthy
    | RunningUnhealthy
    | Stopped String
    | Missing
    deriving(Show, Eq)

configServiceName :: String
configServiceName = "ml-service"

parseStatus :: BL.ByteString -> ServiceStatus
parseStatus json =
    let state  = json ^? nth 0 . key "State"
        status = state >>= (^? key "Status" . _String)
        health = state >>= (^? key "Health" . key "Status" . _String)

    in case status of
        Just "running" ->
            case health of
                Just "unhealthy" -> RunningUnhealthy
                _                -> RunningHealthy
        Just other -> Stopped (T.unpack other)
        Nothing    -> Missing

getDockerStatus :: String -> IO ServiceStatus
getDockerStatus name = do
    (exitCode, output, _err) <- readProcessWithExitCode "docker" ["inspect", name] ""
    return $ case exitCode of
        ExitSuccess   -> parseStatus (BL.pack output)
        ExitFailure _ -> Missing


notify :: String -> String -> IO ()
notify title msg = do
    _ <- tryIOError $ callCommand $ case System.Info.os of
        "darwin" -> "osascript -e 'display notification \"" ++ msg ++ "\" with title \"" ++ title ++ "\"'"
        "linux"  -> "notify-send \"" ++ title ++ "\" \"" ++ msg ++ "\""
        _        -> "echo 'Alert: " ++ msg ++ "'"
    return ()


main :: IO ()
main = do
    putStrLn $ "Haskell Monad Watching: " ++ configServiceName

    forever $ do
        currentStatus <- getDockerStatus configServiceName

        case currentStatus of
            RunningUnhealthy -> do
                putStrLn $ configServiceName ++ " is unhealthy!"
                notify "ML Service Alert" "Service is Running but Unhealthy"

            Stopped reason -> do
                putStrLn $ configServiceName ++ " is stopped: " ++ reason
                notify "ML Service Alert" $ "Service Stopped: " ++ reason

            Missing -> do
                 putStrLn $ configServiceName ++ " is not found."
                 notify "ML Service Alert" $ "Service Is Not Found"

            RunningHealthy ->
                putStrLn $ configServiceName ++ " is working as expected."

        threadDelay 5_000_000  -- this is in microseconds
