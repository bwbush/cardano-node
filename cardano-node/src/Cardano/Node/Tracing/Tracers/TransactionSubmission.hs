{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Configured representations at the node boundary. The network package's
-- legacy instances, event types, namespaces, and protocols are unchanged.
module Cardano.Node.Tracing.Tracers.TransactionSubmission
  ( formatPeerWith
  , formatSendRecvWith
  , formatTxSubmissionWith
  , formatTxOutboundWith
  , formatTxInboundWith
  , formatLocalSubmissionWith
  ) where

import           Cardano.Logging hiding (detail)
import           Cardano.Node.Tracing.TransactionLogging
import           Cardano.Node.Tracing.Tracers.NodeToClient ()
import           Cardano.Node.Tracing.Tracers.NodeToNode ()
import           Data.Aeson (Object, Value (String), (.=))
import           Data.Foldable (toList)
import           Data.Text (Text, pack)
import           Network.Mux.Trace (TraceLabelPeer (..))
import           Network.TypedProtocol.Codec (AnyMessage (AnyMessageAndAgency))
import           Ouroboros.Network.Driver.Simple (TraceSendRecv (..))
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LTS
import qualified Ouroboros.Network.Protocol.TxSubmission2.Type as STX
import           Ouroboros.Network.Tracing ()
import           Ouroboros.Network.TxSubmission.Inbound.V2 (TraceTxSubmissionInbound (..))
import           Ouroboros.Network.TxSubmission.Outbound (TraceTxSubmissionOutbound (..))

-- These two envelopes mirror the existing network instances without first
-- rendering their payload. In particular, never overwrite an already-rendered
-- msg field: doing so would still pay for the suppressed transaction dump.
formatPeerWith
  :: LogFormatting peer
  => (DetailLevel -> a -> Object) -> DetailLevel -> TraceLabelPeer peer a -> Object
formatPeerWith format detail (TraceLabelPeer peer event) =
  ("peer" .= forMachine detail peer) <> format detail event

formatSendRecvWith
  :: (DetailLevel -> AnyMessage ps -> Object) -> DetailLevel -> TraceSendRecv ps -> Object
formatSendRecvWith format detail event = case event of
  TraceSendMsg msg -> ("kind" .= String "Send") <> ("msg" .= format detail msg)
  TraceRecvMsg msg -> ("kind" .= String "Recv") <> ("msg" .= format detail msg)

formatTxSubmissionWith
  :: (Show txid, Show tx)
  => TransactionLogOptions -> (txid -> Text) -> (tx -> txid)
  -> DetailLevel -> AnyMessage (STX.TxSubmission2 txid tx) -> Object
formatTxSubmissionWith opts renderId getId detail event
  | opts == defaultTransactionLogOptions = forMachine detail event
  | otherwise = case event of
      AnyMessageAndAgency _ (STX.MsgReplyTxIds ids) ->
        forMachine detail event <>
          fullTxIdsField opts "txIdsFull" (map (renderId . fst) $ toList ids)
      AnyMessageAndAgency _ (STX.MsgRequestTxs ids) ->
        forMachine detail event <> fullTxIdsField opts "txIdsFull" (map renderId ids)
      AnyMessageAndAgency agency (STX.MsgReplyTxs txs) ->
        ("kind" .= String "MsgReplyTxs") <>
        ("agency" .= String (pack $ show agency)) <>
        ("numTxs" .= length txs) <>
        transactionPayloadField opts "txs" (pack $ show txs) <>
        fullTxIdsField opts "txIdsFull" (map (renderId . getId) txs)
      _ -> forMachine detail event

formatTxOutboundWith
  :: (Show txid, Show tx)
  => TransactionLogOptions -> (txid -> Text) -> (tx -> txid)
  -> DetailLevel -> TraceTxSubmissionOutbound txid tx -> Object
formatTxOutboundWith opts renderId getId detail event
  | opts == defaultTransactionLogOptions = forMachine detail event
  | otherwise = case event of
      TraceTxSubmissionOutboundRecvMsgRequestTxs ids ->
        forMachine detail event <> fullTxIdsField opts "txIdsFull" (map renderId ids)
      TraceTxSubmissionOutboundSendMsgReplyTxs txs ->
        ("kind" .= String "TraceTxSubmissionOutboundSendMsgReplyTxs") <>
        ("numTxs" .= length txs) <>
        (if suppressTxBodies opts || detail == DDetailed
          then transactionPayloadField opts "txs" (pack $ show txs)
          else mempty) <>
        fullTxIdsField opts "txIdsFull" (map (renderId . getId) txs)
      _ -> forMachine detail event

formatLocalSubmissionWith
  :: TransactionLogOptions -> (tx -> Text)
  -> DetailLevel -> AnyMessage (LTS.LocalTxSubmission tx err) -> Object
formatLocalSubmissionWith opts renderId detail event =
  forMachine detail event <> case event of
    AnyMessageAndAgency _ (LTS.MsgSubmitTx tx)
      | fullTxIds opts -> "txIdFull" .= renderId tx
    _ -> mempty

formatTxInboundWith
  :: (Show txid, Show tx)
  => TransactionLogOptions -> (txid -> Text)
  -> DetailLevel -> TraceTxSubmissionInbound txid tx -> Object
formatTxInboundWith opts renderId detail event =
  forMachine detail event <> case event of
    TraceTxSubmissionCollected ids -> idsField ids
    TraceTxInboundAddedToMempool ids _ -> idsField ids
    TraceTxInboundRejectedFromMempool ids _ -> idsField ids
    TraceTxInboundRequestTxs ids -> idsField ids
    _ -> mempty
  where
    idsField = fullTxIdsField opts "txIdsFull" . map renderId
