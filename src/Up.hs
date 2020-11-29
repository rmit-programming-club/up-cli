{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as Text
import Data.Aeson ((.:))
import Text.Read (readMaybe)
import qualified Data.ByteString.Char8 as BS
import qualified Text.Printf as Printf
import qualified Data.Maybe as Maybe



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

data Account = Account { accountId :: Text.Text
                       , accountDisplayName :: Text.Text
                       , accountBalance :: Amount 
                       }


instance Aeson.FromJSON Account where
    parseJSON = Aeson.withObject "Account" $ \v -> do
       id <- v .: "id"
       attributes <- v .: "attributes"
       displayName <- attributes .: "displayName"
       balance <- attributes .: "balance"
       return $ Account id displayName balance

newtype Amount = Amount { amountValueInBaseUnits :: Int } 
  deriving(Num)

instance Aeson.FromJSON Amount where
    parseJSON = Aeson.withObject "Balance" $ \v ->
       Amount <$> v  .: "valueInBaseUnits"

instance Show Amount where
  show (Amount baseUnits) = 
     let dollars = baseUnits `quot` 100
         cents = baseUnits `rem` 100
      in
        concat ["$", show dollars, ".", Printf.printf "%02d" cents]

transactions :: String -> Maybe String -> IO (Either String TransactionsResponse)
transactions token category = makeRequest ("transactions" <> (Maybe.fromMaybe "" (fmap ("?filter%5Bcategory%5D="<>) category))) token

data TransactionsResponse = TransactionsResponse [Transaction]

instance Aeson.FromJSON TransactionsResponse where
    parseJSON = Aeson.withObject "TransactionsResponse" $ \v ->
        TransactionsResponse <$> v .: "data"

data Transaction = Transaction 
    { transactionId :: Text.Text
    , transactionAmount :: Amount
    , transactionDecsription :: Text.Text 
    , transactionMessage :: Maybe Text.Text 
    , transactionCategory :: Maybe Text.Text
    }

instance Aeson.FromJSON Transaction where
    parseJSON = Aeson.withObject "Transaction" $ \v -> do
       id <- v .: "id"
       attributes <- v .: "attributes"
       amount <- attributes .: "amount"
       description <- attributes .: "description"
       message <- attributes .: "message"
       relationships <- v .: "relationships"
       category <- relationships .: "category"
       category_data <- category .: "data" :: Aeson.Parser (Maybe Aeson.Object)
       catId <-
             case category_data of
               Nothing -> return Nothing
               Just cat_data ->
                 cat_data .: "id"
       return $ Transaction id amount description message catId
