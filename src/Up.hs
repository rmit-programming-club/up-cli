{-# LANGUAGE OverloadedStrings #-}
module Up
  ( ping
  , accounts
  , transactions
  , Ping(..)
  , AccountResponse(..)
  , Account(..)
  , Amount(..)
  , Meta(..)
  , TransactionsResponse(..)
  , Transaction(..)
  )
   where
import Control.Lens ((&), (.~), (^.))
import qualified Network.Wreq  as Wreq
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import Text.Read (readMaybe)
import qualified Data.ByteString.Char8 as BS



makeRequest :: Aeson.FromJSON a => String -> String -> IO (Either String a)
makeRequest endpoint token = do
    let 
       options = Wreq.defaults & Wreq.header "Authorization" .~ [BS.pack $ "Bearer " ++ token]
    response <- Wreq.getWith options ("https://api.up.com.au/api/v1/" ++ endpoint)
    let rBody = response ^. Wreq.responseBody
    return $ Aeson.eitherDecode $ rBody

ping :: String -> IO (Either String Ping)
ping = makeRequest "util/ping"

data Meta = Meta { metaId :: String, metaStatus :: String }

instance Aeson.FromJSON Meta where
    parseJSON = Aeson.withObject "Meta" $ \v -> Meta
        <$> v .: "id"
        <*> v .: "statusEmoji"

data Ping = Ping Meta
instance Aeson.FromJSON Ping where
    parseJSON = Aeson.withObject "Ping" $ \v -> Ping
        <$> v .: "meta"

accounts :: String -> IO (Either String AccountResponse)
accounts = makeRequest "accounts"

data AccountResponse = AccountResponse [Account]

instance Aeson.FromJSON AccountResponse where
    parseJSON = Aeson.withObject "AccountResponse" $ \v -> AccountResponse
       <$> v .: "data"

data Account = Account { accountId :: String
                       , accountDisplayName :: String
                       , accountBalance :: Amount 
                       }


instance Aeson.FromJSON Account where
    parseJSON = Aeson.withObject "Account" $ \v -> do
       id <- v .: "id"
       attributes <- v .: "attributes"
       displayName <- attributes .: "displayName"
       balance <- attributes .: "balance"
       return $ Account id displayName balance

data Amount = Amount { amountValueInBaseUnits :: Int } 

instance Aeson.FromJSON Amount where
    parseJSON = Aeson.withObject "Balance" $ \v ->
       Amount <$> v  .: "valueInBaseUnits"


transactions :: String -> IO (Either String TransactionsResponse)
transactions = makeRequest "transactions"

data TransactionsResponse = TransactionsResponse [Transaction]

instance Aeson.FromJSON TransactionsResponse where
    parseJSON = Aeson.withObject "TransactionsResponse" $ \v ->
        TransactionsResponse <$> v .: "data"

data Transaction = Transaction { transactionId :: String, transactionAmount :: Amount, transactionDecsription :: String }

instance Aeson.FromJSON Transaction where
    parseJSON = Aeson.withObject "Transaction" $ \v -> do
       id <- v .: "id"
       attributes <- v .: "attributes"
       amount <- attributes .: "amount"
       description <- attributes .: "description"
       return $ Transaction id amount description
