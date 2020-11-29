{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( runCli
    ) where

import System.Environment (getArgs, lookupEnv)
import qualified Up as Up
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Maybe as Maybe
import Text.Read (readMaybe)
import qualified Data.HashMap.Strict as HashMap


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
       Right (Up.AccountResponse accounts) -> Text.putStrLn $ Text.unlines (map showAccount accounts)
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
    result <- Up.transactions token Nothing
    case result of
       Right (Up.TransactionsResponse response) -> Text.putStrLn $ Text.unlines (map (showTransaction) response)
       Left err -> putStrLn err
runCommand ["charity"] token = do
    result <- Up.transactions token (Just "gifts-and-charity")
    case result of
       Right (Up.TransactionsResponse response) -> 
          let donations = filter ((==) (Just "gifts-and-charity") . Up.transactionCategory) response :: [Up.Transaction]
          in
          putStrLn $ showCharities (foldr (\donation acc -> HashMap.alter (\v -> Just $ Maybe.fromMaybe 0 v - (Up.transactionAmount donation)) (Up.transactionDecsription donation) acc) HashMap.empty donations)
       Left err -> putStrLn err
runCommand command token =
   putStrLn "Invalid command"

showCharities :: HashMap.HashMap Text.Text Up.Amount -> String
showCharities charityMap = 
    unlines ((map (\(charity, amount) -> Text.unpack charity <> ": " <> show amount) (HashMap.toList charityMap))
              ++ ["Total: "<> show (sum (HashMap.elems charityMap))])
    

showAccount :: Up.Account -> Text.Text
showAccount (Up.Account accountId name (Up.Amount amount)) = Text.concat[name, ": $", Text.pack $ show amount, " ",  accountId]

showTransaction :: Up.Transaction -> Text.Text
showTransaction t = 
   Text.intercalate " " ( Maybe.catMaybes [ Just $ Up.transactionDecsription t
                        , Up.transactionMessage t
                        , Just $ Up.transactionId t
                        , Just $ Text.pack . show $ Up.transactionAmount t
                        , Up.transactionCategory t
                        ] )
