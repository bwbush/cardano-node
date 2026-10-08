{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Tracing.NewTracing.TransactionLogging (tests) where

import           Cardano.Logging hiding (detail)
import           Cardano.Node.Tracing.Configured
import           Cardano.Node.Tracing.TransactionLogging
import           Cardano.Node.Tracing.Tracers.TransactionSubmission
import           Control.Monad (forM_)
import           Data.Aeson (Value (..), encode, toJSON)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as BSL
import           Data.Text (Text)
import qualified Data.Text as Text
import           Hedgehog
import           Network.TypedProtocol.Codec (AnyMessage (AnyMessageAndAgency))
import           Ouroboros.Network.Driver.Simple (TraceSendRecv (..))
import qualified Ouroboros.Network.Protocol.TxSubmission2.Type as STX
import           Ouroboros.Network.TxSubmission.Inbound.V2 (TraceTxSubmissionInbound (..))
import           Ouroboros.Network.TxSubmission.Outbound (TraceTxSubmissionOutbound (..))

data FixtureTx = FixtureTx { fixtureId :: Text, fixtureBody :: String }
  deriving Show

hashA, hashB :: Text
hashA = "deadbeef" <> Text.replicate 56 "0"
hashB = "deadbeef" <> Text.replicate 56 "1"

compact :: TransactionLogOptions
compact = TransactionLogOptions True True

reply :: [FixtureTx] -> AnyMessage (STX.TxSubmission2 Text FixtureTx)
reply = AnyMessageAndAgency STX.SingTxs . STX.MsgReplyTxs

tests :: IO Bool
tests = checkParallel $ Group "transaction logging integration"
  [ ("legacy peer output at every detail", withTests 1 $ property $
      forM_ [DMinimal, DNormal, DDetailed, DMaximum] $ \detail -> do
        let event = reply [FixtureTx hashA "body"]
        formatTxSubmissionWith defaultTransactionLogOptions id fixtureId detail event ===
          forMachine detail event)
  , ("peer reply does not evaluate omitted bodies", withTests 1 $ property $ do
      let event = reply [FixtureTx hashA (error "body evaluated"), FixtureTx hashB "body"]
          formatted = formatTxSubmissionWith compact id fixtureId DDetailed event
      assert $ BSL.length (encode formatted) > 0
      KeyMap.lookup "txs" formatted === Nothing
      KeyMap.lookup "txIdsFull" formatted === Just (toJSON [hashA, hashB])
      KeyMap.lookup "numTxs" formatted === Just (Number 2))
  , ("full IDs alone preserve the legacy peer payload", withTests 1 $ property $ do
      let event = reply [FixtureTx hashA "body"]
          formatted = formatTxSubmissionWith (TransactionLogOptions True False) id fixtureId DNormal event
      KeyMap.delete "txIdsFull" (KeyMap.delete "numTxs" formatted) === forMachine DNormal event
      KeyMap.lookup "txIdsFull" formatted === Just (toJSON [hashA]))
  , ("suppression alone does not evaluate the added IDs", withTests 1 $ property $ do
      let event = reply [FixtureTx hashA (error "body evaluated")]
          formatted = formatTxSubmissionWith (TransactionLogOptions False True)
            (error "ID renderer evaluated") fixtureId DNormal event
      assert $ BSL.length (encode formatted) > 0
      KeyMap.lookup "txIdsFull" formatted === Nothing
      KeyMap.lookup "txBodyOmitted" formatted === Just (Bool True))
  , ("outbound reply does not evaluate omitted bodies", withTests 1 $ property $ do
      let event = TraceTxSubmissionOutboundSendMsgReplyTxs [FixtureTx hashA (error "body evaluated")]
          formatted = formatTxOutboundWith compact id fixtureId DDetailed event
      assert $ BSL.length (encode formatted) > 0
      KeyMap.lookup "txs" formatted === Nothing
      KeyMap.lookup "txIdsFull" formatted === Just (toJSON [hashA]))
  , ("inbound IDs retain order and multiplicity", withTests 1 $ property $ do
      let event = TraceTxSubmissionCollected [hashB, hashA, hashB] :: TraceTxSubmissionInbound Text FixtureTx
          formatted = formatTxInboundWith compact id DMinimal event
      KeyMap.lookup "count" formatted === Just (Number 3)
      KeyMap.lookup "txIdsFull" formatted === Just (toJSON [hashB, hashA, hashB]))
  , ("configuration adapter preserves metadata and metrics", withTests 1 $ property $ do
      let event = TraceTxSubmissionCollected [hashA] :: TraceTxSubmissionInbound Text FixtureTx
          wrapped = ConfiguredTrace True (formatTxInboundWith compact id) event
      nsGetTuple (namespaceFor wrapped) === nsGetTuple (namespaceFor event)
      severityFor (namespaceFor wrapped) (Just wrapped) === severityFor (namespaceFor event) (Just event)
      show (asMetrics wrapped) === show (asMetrics event)
      forHuman wrapped === "")
  , ("send envelope agrees with legacy formatting", withTests 1 $ property $ do
      let event = TraceSendMsg $ reply [FixtureTx hashA "body"]
      formatSendRecvWith forMachine DDetailed event === forMachine DDetailed event)
  , ("receive envelope agrees with legacy formatting", withTests 1 $ property $ do
      let event = TraceRecvMsg $ reply [FixtureTx hashA "body"]
      formatSendRecvWith forMachine DDetailed event === forMachine DDetailed event)
  ]
