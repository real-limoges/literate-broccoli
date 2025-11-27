#!/usr/bin/env stack
-- stack --resolver lts-20.5 script --package aeson --package process --package bytestring --package text --package lens --package lens-aeson
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

import           Control.Concurrent         (threadDelay)
import           Control.Lens               ((^.), (^?))
import           Control.Monad              (forever, void)
import           Data.Aeson                 (Value, decode)
import           Data.Aeson.Lens            (_String, key, nth)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text                  as T
import           System.IO.Error            (tryIOError)
import           System.Process             (callCommand, readProcess)

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
    let state = json ^? nth 0 . key "State"
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
    result <- tryIOError $ readProcess "docker" ["inspect", name] ""
    return $ case result of
        Right output -> parseStatus (BL.pack output)
        Left _       -> Missing


main :: IO ()
main = do
    putStrLn $ "Haskell Monad Watching: " ++ configServiceName

    forever $ do
        currentStatus <- getDockerStatus configServiceName

        case currentStatus of
            RunningUnhealthy -> do
                putStrLn $ configServiceName ++ " is unhealthy!"
                -- todo Notify

            Stopped reason -> do
                putStrLn $ configServiceName ++ " is stopped: " ++ reason
                -- todo Notify

            Missing -> do
                putStrLn $ configServiceName ++ " is not found."
                -- notify?

            RunningHealthy ->
                putStrLn $ configServiceName ++ " is working as expected."

        threadDelay 5_000_000  -- this is in microseconds
