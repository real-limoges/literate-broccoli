#!/usr/bin/env stack
-- stack --resolver lts-20.5 script --package aeson --package process --package bytestring --package text --package lens --package lens-aeson
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}

import Control.Monad(forever)
import Control.Lens ((^?), (^.))
import Data.Aeson (decode)
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy.Char8 as BL

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
    let state = ...

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
                putStrLn configServiceName ++ " is unhealthy!"
                todo Notify
            
            Stopped reason -> do
                putStrLn configServiceName ++ " is stopped: " ++ reason
                todo Notify
            
            Missing -> do
                putStrLn configServiceName ++ " is not found."
                notify?

            RunningHealthy ->
                putStrLn configServiceName ++ " is working as expected."
    
        threadDelay 5_000_000  -- this is in microseconds