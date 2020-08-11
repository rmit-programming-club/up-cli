{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( runCli
    ) where

import System.Environment (getArgs, lookupEnv)
import qualified Up as Up
import Text.Read (readMaybe)


runCli :: IO ()
runCli = do
  args <- getArgs
  tokenM <- lookupEnv "API_UP_OAUTH"
  case tokenM of
    Just token -> 
      runCommand args token
    Nothing -> putStrLn "Expected to find API_UP_OAUTH"


-- Runs a command with a token
runCommand :: [String] -> String -> IO ()
runCommand ["ping"] token = do
    result <- Up.ping token
    case result of
       Right (Up.Ping (Up.Meta id status)) -> putStrLn status
       Left err -> putStrLn err
runCommand ["accounts"] token = do
    result <- Up.accounts token
    case result of
       Right (Up.AccountResponse accounts) -> putStrLn $ unlines (map showAccount accounts)
       Left err -> putStrLn err
runCommand ["runway", expenseS] token =
    case (readMaybe expenseS :: Maybe Int) of
       Just expensePerWeek -> do
        response <- Up.accounts token
        case response of
          Right (Up.AccountResponse accounts) ->
            let balance = sum $ map (Up.amountValueInBaseUnits . Up.accountBalance) accounts
            in
              putStrLn $ (show (fromRational (toRational balance / toRational expensePerWeek) :: Float) ) ++ " Weeks"
          Left err -> putStrLn err
       Nothing -> putStrLn "Requires expense per week in cents"
runCommand ["transactions"] token = do
    result <- Up.transactions token
    case result of
       Right (Up.TransactionsResponse response) -> putStrLn $ unlines (map (show .Up.amountValueInBaseUnits . Up.transactionAmount) response)
       Left err -> putStrLn err
runCommand command token =
   putStrLn "Invalid command"

showAccount :: Up.Account -> String
showAccount (Up.Account _ name (Up.Amount amount)) = name ++ ": $" ++ show amount
