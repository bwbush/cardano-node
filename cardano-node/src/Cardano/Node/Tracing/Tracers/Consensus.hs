{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
-- Configured wrappers only; upstream consensus owns the default instances.
module Cardano.Node.Tracing.Tracers.Consensus
  (formatMempoolWith, formatMempoolWithIdentities, formatLocalTxSubmissionWith) where
import Cardano.Logging hiding (detail)
import Cardano.Node.Tracing.TransactionLogging
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Mempool (TraceEventMempool(..), jsonMempoolRejectionDetails)
import Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server (TraceLocalTxSubmissionServerEvent(..))
import Ouroboros.Consensus.Storage.LedgerDB.Forker (ResolveLeiosBlock(leiosTxBytesOfGenTx))
import Ouroboros.Consensus.Tracing
import Data.Aeson (Value(String), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import qualified Data.Text as Text
import qualified LeiosDemoTypes as Leios

-- | The legacy instance below and the configured tracer share one formatter.
formatMempoolWith
  :: ( LogFormatting (ApplyTxErr blk)
  , LogFormatting (GenTx blk)
  , HasTxId (GenTx blk)
  , Show (GenTxId blk)
  , ConvertTxId blk
  , LedgerSupportsMempool blk
  , ConvertRawHash blk
  ) => TransactionLogOptions -> DetailLevel -> TraceEventMempool blk -> Aeson.Object
formatMempoolWith opts
  | opts == defaultTransactionLogOptions = forMachine
  | otherwise = formatMempoolUsing
  (\dtal tx -> transactionObject opts (renderTxId $ txId tx) (forMachine dtal tx))
  (fullTxIdsField opts "txsRemovedFull" . map renderTxId)

-- | Pair the ledger ID with exactly the encoding used by the node's Leios
-- serializer. This adds no decoding, signature checking, or script execution.
-- A wire encoding with different witnesses is a separate serialized variant.
formatMempoolWithIdentities
  :: ( LogFormatting (ApplyTxErr blk), LogFormatting (GenTx blk)
     , HasTxId (GenTx blk), Show (GenTxId blk), ConvertTxId blk
     , LedgerSupportsMempool blk, ConvertRawHash blk, ResolveLeiosBlock blk
     )
  => TransactionIdentityLogOptions -> TransactionLogOptions
  -> DetailLevel -> TraceEventMempool blk -> Aeson.Object
formatMempoolWithIdentities identityOpts opts
  | identityOpts == defaultTransactionIdentityLogOptions && opts == defaultTransactionLogOptions = forMachine
  | otherwise = formatMempoolUsing
  (\dtal tx -> transactionObject opts (renderTxId $ txId tx) (forMachine dtal tx) <>
    transactionIdentityField identityOpts (renderTxId $ txId tx)
      (fmap (\bs -> (Text.pack $ Leios.prettyTxHash $ Leios.hashLeiosTx $ Leios.MkLeiosTx bs, BS.length bs))
        (leiosTxBytesOfGenTx tx)))
  (fullTxIdsField opts "txsRemovedFull" . map renderTxId)

-- Keep the legacy instance's constraints unchanged: only the configured path
-- requires extracting an ID from a transaction.
formatMempoolUsing
  :: ( LogFormatting (ApplyTxErr blk)
     , LogFormatting (GenTx blk)
     , Show (GenTxId blk)
     , ConvertTxId blk
     , LedgerSupportsMempool blk
     , ConvertRawHash blk
     )
  => (DetailLevel -> GenTx blk -> Aeson.Object)
  -> ([GenTxId blk] -> Aeson.Object)
  -> DetailLevel -> TraceEventMempool blk -> Aeson.Object
formatMempoolUsing renderTx fullIds dtal event = case event of
  TraceMempoolAddedTx tx _mpSzBefore mpSzAfter ->
    mconcat
      [ "kind" .= String "TraceMempoolAddedTx"
      , "tx" .= formatTx (txForgetValidated tx)
      , "mempoolSize" .= forMachine dtal mpSzAfter
      ]
  TraceMempoolRejectedTx tx txApplyErr details mpSz ->
    mconcat $
      [ "kind" .= String "TraceMempoolRejectedTx"
      , "tx" .= formatTx tx
      , "mempoolSize" .= forMachine dtal mpSz
      , "errdetails" .= jsonMempoolRejectionDetails details
      ] <>
      if dtal < DDetailed then [] else
      [ "err" .= forMachine dtal txApplyErr ]
  TraceMempoolRemoveTxs txs mpSz ->
    mconcat
      [ "kind" .= String "TraceMempoolRemoveTxs"
      , "txs" .= map (\(tx, err) -> Aeson.object $
          [ "tx" .= formatTx tx ] <>
          [ "err" .= forMachine dtal err | dtal >= DDetailed ]) txs
      , "mempoolSize" .= forMachine dtal mpSz
      ]
  TraceMempoolManuallyRemovedTxs txs0 txs1 mpSz ->
    mconcat
      [ "kind" .= String "TraceMempoolManuallyRemovedTxs"
      , "txsRemoved" .= map (String . renderTxIdForDetails dtal) (toList txs0)
      , "txsInvalidated" .= map formatTx txs1
      , "mempoolSize" .= forMachine dtal mpSz
      ] <> fullIds (toList txs0)
  TraceMempoolAttemptingAdd tx ->
    ("kind" .= String "TraceMempoolAttemptingAdd") <> ("tx" .= formatTx tx)
  other -> forMachine dtal other
  where
    formatTx = renderTx dtal

formatLocalTxSubmissionWith
  :: (ConvertTxId blk, HasTxId (GenTx blk))
  => TransactionLogOptions -> DetailLevel -> TraceLocalTxSubmissionServerEvent blk -> Aeson.Object
formatLocalTxSubmissionWith opts detail event@(TraceReceivedTx tx) =
  forMachine detail event <>
    (if fullTxIds opts then "txIdFull" .= renderTxId (txId tx) else mempty)
