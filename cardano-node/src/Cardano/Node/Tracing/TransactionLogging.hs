{-# LANGUAGE OverloadedStrings #-}

-- | Opt-in transaction payload policy. This module deliberately has no ledger
-- or dispatcher dependency: configuration and the lazy payload boundary can be
-- tested without constructing a node.
module Cardano.Node.Tracing.TransactionLogging
  ( TransactionLogOptions (..)
  , defaultTransactionLogOptions
  , parseTransactionLogOptions
  , transactionObject
  , fullTxIdsField
  , transactionPayloadField
  , LeiosReferenceLogOptions (..)
  , defaultLeiosReferenceLogOptions
  , parseLeiosReferenceLogOptions
  ) where

import           Control.Monad (unless)
import           Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (..), object,
                   withObject, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Aeson.Types (Parser)
import           Data.List ((\\))
import           Data.Text (Text)
import qualified Data.Text as Text

data TransactionLogOptions = TransactionLogOptions
  { fullTxIds :: Bool
  , suppressTxBodies :: Bool
  } deriving (Eq, Show)

-- | Existing configurations must retain their existing output.
defaultTransactionLogOptions :: TransactionLogOptions
defaultTransactionLogOptions = TransactionLogOptions False False

instance FromJSON TransactionLogOptions where
  parseJSON = withObject "TraceOptionTransactions" $ \o -> do
    let unknown = KeyMap.keys o \\ ["fullTxIds", "suppressTxBodies"]
    unless (null unknown) $
      fail $ "Unknown TraceOptionTransactions fields: " <> show unknown
    TransactionLogOptions <$> option o "fullTxIds" <*> option o "suppressTxBodies"
    where
      -- Explicit nulls and misspelled fields are errors, not silent opt-outs.
      option o key = maybe (pure False) parseJSON (KeyMap.lookup key o)

instance ToJSON TransactionLogOptions where
  toJSON opts = object
    [ "fullTxIds" .= fullTxIds opts
    , "suppressTxBodies" .= suppressTxBodies opts
    ]

-- | Read the optional top-level field from the node configuration. Other node
-- settings are intentionally left to their existing parsers.
parseTransactionLogOptions :: Value -> Parser TransactionLogOptions
parseTransactionLogOptions = withObject "node configuration" $ \o ->
  maybe (pure defaultTransactionLogOptions) parseJSON
    (KeyMap.lookup "TraceOptionTransactions" o)

-- | Separate policy preserves the existing two-field transaction API. Hashing
-- serialized replies has a cost, so references are explicitly opt-in.
newtype LeiosReferenceLogOptions = LeiosReferenceLogOptions
  { includeTxReferences :: Bool
  } deriving (Eq, Show)

defaultLeiosReferenceLogOptions :: LeiosReferenceLogOptions
defaultLeiosReferenceLogOptions = LeiosReferenceLogOptions False

instance FromJSON LeiosReferenceLogOptions where
  parseJSON = withObject "TraceOptionLeios" $ \o -> do
    let unknown = KeyMap.keys o \\ ["includeTxReferences"]
    unless (null unknown) $
      fail $ "Unknown TraceOptionLeios fields: " <> show unknown
    LeiosReferenceLogOptions <$> maybe (pure False) parseJSON
      (KeyMap.lookup "includeTxReferences" o)

instance ToJSON LeiosReferenceLogOptions where
  toJSON opts = object ["includeTxReferences" .= includeTxReferences opts]

parseLeiosReferenceLogOptions :: Value -> Parser LeiosReferenceLogOptions
parseLeiosReferenceLogOptions = withObject "node configuration" $ \o ->
  maybe (pure defaultLeiosReferenceLogOptions) parseJSON
    (KeyMap.lookup "TraceOptionLeios" o)

-- | Keep legacy output untouched unless explicitly configured. In suppressed
-- mode the legacy object is not evaluated: no transaction dump is constructed
-- and discarded. The caller supplies the canonical ledger transaction ID.
transactionObject :: TransactionLogOptions -> Text -> Object -> Object
transactionObject opts txId legacy = base <> fullId
  where
    base
      | suppressTxBodies opts = KeyMap.fromList
          [ ("txid", String $ Text.take 8 txId)
          , ("txBodyOmitted", Bool True)
          ]
      | otherwise = legacy
    fullId
      | fullTxIds opts = KeyMap.singleton "txIdFull" (String txId)
      | otherwise = mempty

-- | Additive list fields retain order and multiplicity. The name distinguishes
-- the identifier domain and its role, e.g. txsRemovedFull vs txIdsFull.
fullTxIdsField :: TransactionLogOptions -> Key.Key -> [Text] -> Object
fullTxIdsField opts key ids
  | fullTxIds opts = KeyMap.singleton key (toJSON ids)
  | otherwise = mempty

-- | Payloads in peer traces are also lazy. Do not evaluate 'show txs' when
-- suppression is selected. Counts and identity fields are supplied separately.
transactionPayloadField :: TransactionLogOptions -> Key.Key -> Text -> Object
transactionPayloadField opts key payload
  | suppressTxBodies opts = KeyMap.singleton "txBodyOmitted" (Bool True)
  | otherwise = KeyMap.singleton key (String payload)
