{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- needs different instances on ghc8 and on ghc9

module Cardano.Node.Tracing.Tracers
  ( mkDispatchTracers
  , buildNodeTracers
  ) where

import qualified Cardano.Network.Diffusion as Cardano.Diffusion
import           Cardano.Network.NodeToClient (LocalAddress)
import           Cardano.Network.NodeToClient.Version ()
import           Cardano.Network.NodeToNode (RemoteAddress)
import           Cardano.Network.NodeToNode.Version ()
import           Cardano.Network.OrphanInstances ()
import           Cardano.Network.Tracing.PeerSelection ()
import           Cardano.Network.Tracing.PeerSelectionCounters ()
import           Cardano.Node.Configuration.TopologyP2P ()
import           Cardano.Node.TraceConstraints
import           Cardano.Node.Tracing
import           Cardano.Node.Tracing.Formatting ()
import           Cardano.Node.Tracing.NodeInfo ()
import           Cardano.Node.Tracing.NodeStartupInfo ()
import           Cardano.Node.Tracing.Registry
import qualified Cardano.Node.Tracing.StateRep as SR
import           Cardano.Node.Tracing.Tracers.BlockReplayProgress
import           Cardano.Node.Tracing.Tracers.ChainDB
import           Cardano.Node.Tracing.Tracers.Consensus
import           Cardano.Node.Tracing.Tracers.ForgingStats (calcForgeStats)
import           Cardano.Node.Tracing.Tracers.KESInfo
import           Cardano.Node.Tracing.Tracers.LedgerMetrics ()
import           Cardano.Node.Tracing.Tracers.NodeToClient ()
import           Cardano.Node.Tracing.Tracers.NodeToNode ()
import           Cardano.Node.Tracing.Tracers.NodeVersion (getNodeVersion)
import           Cardano.Node.Tracing.Tracers.Rpc ()
import           Cardano.Node.Tracing.Tracers.Shutdown ()
import           Cardano.Node.Tracing.Tracers.Startup ()
import           Ouroboros.Consensus.Ledger.Inspect (LedgerEvent)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client (TraceChainSyncClientEvent)
import qualified Ouroboros.Consensus.Network.NodeToClient as NodeToClient
import qualified Ouroboros.Consensus.Network.NodeToClient as NtC
import qualified Ouroboros.Consensus.Network.NodeToNode as NodeToNode
import qualified Ouroboros.Consensus.Network.NodeToNode as NtN
import           Ouroboros.Consensus.Node.GSM
import qualified Ouroboros.Consensus.Node.Run as Consensus
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import           Ouroboros.Network.Block
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.ConnectionId (ConnectionId)
import qualified Ouroboros.Network.Diffusion as Diffusion
import           Ouroboros.Network.PeerSelection.PublicRootPeers ()
import           Ouroboros.Network.Tracing ()

import           Codec.CBOR.Read (DeserialiseFailure)
import           Control.Monad (unless)
import           Data.Aeson (ToJSON (..))
import           Data.Proxy (Proxy (..))
import           Network.Mux.Trace (TraceLabelPeer (..))
import qualified Network.Mux.Trace as Mux
import           Network.Mux.Tracing ()

import           Hermod.Tracing hiding (mkTracer)
import           Hermod.Tracing.API.Tracer (mkTracer)
import           Hermod.Tracing.HermodTracingMessage (HermodTracingMessage)
import           Hermod.Tracing.Resources.Types ()


-- | Construct and configure the tracers for all system components.
--
mkDispatchTracers
  :: forall blk .
  ( Consensus.RunNode blk
  , TraceConstraints blk
  , LogFormatting (LedgerEvent blk)
  , LogFormatting
    (TraceLabelPeer
      (ConnectionId RemoteAddress) (TraceChainSyncClientEvent blk))
  , LogFormatting (TraceGsmEvent (Tip blk))
  , MetaTrace (TraceGsmEvent (Tip blk))
  , ToJSON (HeaderHash blk)
  )
  => Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> Trace IO DataPoint
  -> TraceConfig
  -> IO (Tracers RemoteAddress LocalAddress blk IO)

mkDispatchTracers trBase trForward mbTrEKG trDataPoint trConfig = do
    registry <- newRegistry ForRuntime Backends
      { bkStdout    = trBase
      , bkForward   = trForward
      , bkEKG       = mbTrEKG
      , bkDataPoint = trDataPoint
      }
    !tracers <- buildNodeTracers registry
    entries <- registered registry

    configReflection <- emptyConfigReflection
    configureAll configReflection trConfig entries

    traceTracerInfo trBase trForward configReflection

    let warnings = checkAll trConfig entries
    unless (null warnings) $
      traceConfigWarnings trBase trForward warnings

    traceEffectiveConfiguration trBase trForward trConfig

    traceWith (nodeVersionTracer tracers) getNodeVersion

    pure tracers

-- | The one declaration of every trace the node constructs: each trace is
-- built once here, through the registry, and assembled into the records the
-- libraries expect. Nothing is configured or traced yet; the registry's entries
-- are configured, documented and checked by the callers.
buildNodeTracers
  :: forall blk .
  ( Consensus.RunNode blk
  , TraceConstraints blk
  , LogFormatting (LedgerEvent blk)
  , LogFormatting
    (TraceLabelPeer
      (ConnectionId RemoteAddress) (TraceChainSyncClientEvent blk))
  , LogFormatting (TraceGsmEvent (Tip blk))
  , MetaTrace (TraceGsmEvent (Tip blk))
  , ToJSON (HeaderHash blk)
  )
  => Registry
  -> IO (Tracers RemoteAddress LocalAddress blk IO)
buildNodeTracers registry = do

    !nodeInfoDP <- newDataPoint registry

    !nodeStartupInfoDP <- newDataPoint registry

    -- Fed by the node state projections of the other tracers; its inner names
    -- (OpeningDbs, NodeKernelOnline, ...) have no prefix.
    !nodeStateDP <- newDataPointWith runtimeOnly registry

    !stateTr <- newTrace registry ["NodeState"]

    !resourcesTr <- newTrace registry []

    !ledgerMetricsTr <- newTrace registry []

    !startupTr <- newTrace registry ["Startup"]

    !shutdownTr <- newTrace registry ["Shutdown"]

    !chainDBTr <- newTrace registry ["ChainDB"]
    !chainDBTr' <- withAddedToCurrentChainEmptyLimited chainDBTr

    !nodeVersionTr <- newTrace registry ["Version"]

    -- Filter out replayed blocks for this tracer
    let chainDBTr'' = filterTrace
                      (\case (_, ChainDB.TraceLedgerDBEvent
                                            (LedgerDB.LedgerReplayEvent (LedgerDB.TraceReplayProgressEvent
                                                                        (LedgerDB.ReplayedBlock {})))) -> False
                             (_, _) -> True)
                      chainDBTr'


    !replayBlockTr <- newTrace registry ["ChainDB", "ReplayBlock"]

    -- This tracer handles replayed blocks specially
    !replayBlockTr' <- withReplayedBlock replayBlockTr


    !consensusTr <- mkConsensusTracers registry

    !nodeToClientTr <- mkNodeToClientTracers registry

    !nodeToNodeTr <- mkNodeToNodeTracers registry

    !(diffusionTr :: Cardano.Diffusion.CardanoTracers IO) <- mkDiffusionTracers registry

    -- TraceChurnMode's MetaTrace instance (cardano-diffusion, Churn.hs) has an
    -- empty inner namespace and no severity for the documentation, so the
    -- documentation and the consistency check would warn about it.
    !churnModeTr <- newTraceWith runtimeOnly registry ["Net", "PeerSelection", "ChurnMode"]

    !rpcTr <- newTrace registry ["RPC"]

    -- hermod's own messages: emitted by hermod, documented here.
    docOnly @HermodTracingMessage registry ["Reflection"]

    -- The node state projections are callbacks over several constructors;
    -- everything else is handed over as it is.
    pure Tracers
      {
        chainDBTracer = chainDBTr''
                      <> replayBlockTr'
                      <> mkTracer (SR.traceNodeStateChainDB nodeStateDP)
      , consensusTracers = consensusTr
      , churnModeTracer = churnModeTr
      , nodeToClientTracers = nodeToClientTr
      , nodeToNodeTracers = nodeToNodeTr
      , diffusionTracers = diffusionTr
      , startupTracer   = startupTr
                         <> mkTracer (SR.traceNodeStateStartup nodeStateDP)
      , shutdownTracer  = shutdownTr
                         <> contramap SR.NodeShutdown nodeStateDP
      , nodeInfoTracer  = nodeInfoDP
      , nodeStartupInfoTracer = nodeStartupInfoDP
      , nodeStateTracer = stateTr <> nodeStateDP
      , nodeVersionTracer = nodeVersionTr
      , resourcesTracer = resourcesTr
      , ledgerMetricsTracer = ledgerMetricsTr
      , rpcTracer = rpcTr
    }

mkConsensusTracers :: forall blk.
  ( Consensus.RunNode blk
  , TraceConstraints blk
  , LogFormatting (TraceLabelPeer
                    (ConnectionId RemoteAddress) (TraceChainSyncClientEvent blk))
  , LogFormatting (TraceGsmEvent (Tip blk))
  , MetaTrace (TraceGsmEvent (Tip blk))
  , ToJSON (HeaderHash blk)
  )
  => Registry
  -> IO (Consensus.Tracers IO (ConnectionId RemoteAddress) (ConnectionId LocalAddress) blk)
mkConsensusTracers registry = do
    let mbTrEKG = bkEKG (regBackends registry)

    !chainSyncClientTr  <- newTrace registry ["ChainSync", "Client"]
    !chainSyncServerHeaderTr <- newTrace registry ["ChainSync", "ServerHeader"]

    -- Special chainSync server metrics
    -- any server header event advances the counter
    let chainSyncServerHeaderMetricsTr =
           contramap
              (const
                (FormattedMetrics
                  [CounterM "ChainSync.HeadersServed" CounterIncrement]))
              (mkMetricsTracer mbTrEKG)

    !chainSyncServerBlockTr <- newTrace registry ["ChainSync", "ServerBlock"]

    !consensusSanityCheckTr <- newTrace registry ["Consensus", "SanityCheck"]

    !blockFetchDecisionTr  <- newTrace registry ["BlockFetch", "Decision"]

    !blockFetchClientTr  <- newTrace registry ["BlockFetch", "Client"]

    -- Special blockFetch client metrics, send directly to EKG
    !blockFetchClientMetricsTr <- do
        tr1 <- foldTraceM (\cm lc -> pure . calculateBlockFetchClientMetrics cm lc) initialClientMetrics
                    (metricsFormatter
                      (mkMetricsTracer mbTrEKG))
        pure $ filterTrace (\ (_, TraceLabelPeer _ m) -> case m of
                                              BlockFetch.CompletedBlockFetch {} -> True
                                              _ -> False)
                 tr1
    -- The metrics above bypass configuration; this documents them.
    docOnly @ClientMetrics registry ["BlockFetch", "Client"]

    !blockFetchServerTr  <- newTrace registry ["BlockFetch", "Server"]

    !servedBlockLatestTr <- servedBlockLatest mbTrEKG

    !forgeKESInfoTr  <- newTrace registry ["Forge", "StateInfo"]

    !txInboundTr  <- newTrace registry ["TxSubmission", "TxInbound"]

    !txOutboundTr  <- newTrace registry ["TxSubmission", "TxOutbound"]

    !localTxSubmissionServerTr <- newTrace registry ["TxSubmission", "LocalServer"]

    !mempoolTr   <- newTrace registry ["Mempool"]

    !forgeTr    <- newTrace registry ["Forge", "Loop"]

    !forgeStatsTr <- newTrace registry ["Forge", "Stats"]
    !forgeStatsTr' <- calcForgeStats forgeStatsTr

    !blockchainTimeTr   <- newTrace registry ["BlockchainTime"]

    !keepAliveClientTr  <- newTrace registry ["Net"]

    !consensusStartupErrorTr <- newTrace registry ["Consensus", "Startup"]

    !consensusGddTr <- newTrace registry ["Consensus", "GDD"]

    !consensusGsmTr <- newTrace registry ["Consensus", "GSM"]

    !consensusCsjTr <- newTrace registry ["Consensus", "CSJ"]

    !consensusKesAgentTr <- newTrace registry ["Consensus", "KESAgent"]

    !consensusDbfTr <- newTrace registry ["Consensus", "DevotedBlockFetch"]

    !txLogicTracer  <-  newTrace registry ["txLogic", "Remote"]

    !txCountersTracer  <-  newTrace registry ["txCounters", "Remote"]

    !txPerasCertIn  <-  newTrace registry ["Peras", "Cert", "Inbound"]

    !txPerasCertOut  <-  newTrace registry ["Peras", "Cert", "Outbound"]

    !txPerasVoteIn  <-  newTrace registry ["Peras", "Vote", "Inbound"]

    !txPerasVoteOut  <-  newTrace registry ["Peras", "Vote", "Outbound"]

    pure $ Consensus.Tracers
      { Consensus.chainSyncClientTracer = chainSyncClientTr
      , Consensus.chainSyncServerHeaderTracer =
          chainSyncServerHeaderTr <> chainSyncServerHeaderMetricsTr
      , Consensus.chainSyncServerBlockTracer = chainSyncServerBlockTr
      , Consensus.consensusSanityCheckTracer = consensusSanityCheckTr
      , Consensus.blockFetchDecisionTracer = blockFetchDecisionTr
      , Consensus.blockFetchClientTracer =
          blockFetchClientTr <> blockFetchClientMetricsTr
      , Consensus.blockFetchServerTracer =
          blockFetchServerTr <> servedBlockLatestTr
      , Consensus.forgeStateInfoTracer = traceAsKESInfo (Proxy @blk) forgeKESInfoTr
      , Consensus.gddTracer = consensusGddTr
      , Consensus.txInboundTracer = txInboundTr
      , Consensus.txOutboundTracer = txOutboundTr
      , Consensus.localTxSubmissionServerTracer = localTxSubmissionServerTr
      , Consensus.mempoolTracer = mempoolTr
      , Consensus.forgeTracer =
          contramap (\(Consensus.TraceLabelCreds _ x) -> x) (forgeTr <> forgeStatsTr')
      , Consensus.blockchainTimeTracer = blockchainTimeTr
      , Consensus.keepAliveClientTracer = keepAliveClientTr
      , Consensus.consensusErrorTracer =
          contramap ConsensusStartupException consensusStartupErrorTr
      , Consensus.gsmTracer = consensusGsmTr
      , Consensus.csjTracer = consensusCsjTr
      , Consensus.dbfTracer = consensusDbfTr
      , Consensus.kesAgentTracer = consensusKesAgentTr
      , Consensus.txLogicTracer = txLogicTracer
      , Consensus.txCountersTracer = txCountersTracer
      , Consensus.perasCertDiffusionInboundTracer = txPerasCertIn
      , Consensus.perasCertDiffusionOutboundTracer = txPerasCertOut
      , Consensus.perasVoteDiffusionInboundTracer = txPerasVoteIn
      , Consensus.perasVoteDiffusionOutboundTracer = txPerasVoteOut
      }

mkNodeToClientTracers :: forall blk.
     Consensus.RunNode blk
  => Registry
  -> IO (NodeToClient.Tracers IO (ConnectionId LocalAddress) blk DeserialiseFailure)
mkNodeToClientTracers registry = do
    !chainSyncTr <- newTrace registry ["ChainSync", "Local"]

    !txMonitorTr <- newTrace registry ["TxSubmission", "MonitorClient"]

    !txSubmissionTr <- newTrace registry ["TxSubmission", "Local"]

    !stateQueryTr <- newTrace registry ["StateQueryServer"]

    pure $ NtC.Tracers
      { NtC.tChainSyncTracer = chainSyncTr
      , NtC.tTxMonitorTracer = txMonitorTr
      , NtC.tTxSubmissionTracer = txSubmissionTr
      , NtC.tStateQueryTracer = stateQueryTr
      }

mkNodeToNodeTracers :: forall blk.
  ( Consensus.RunNode blk
  , TraceConstraints blk)
  => Registry
  -> IO (NodeToNode.Tracers IO RemoteAddress blk DeserialiseFailure)
mkNodeToNodeTracers registry = do

    !chainSyncTracer <-  newTrace registry ["ChainSync", "Remote"]

    !chainSyncSerialisedTr <-  newTrace registry ["ChainSync", "Remote", "Serialised"]

    !blockFetchTr  <-  newTrace registry ["BlockFetch", "Remote"]

    !blockFetchSerialisedTr <-  newTrace registry ["BlockFetch", "Remote", "Serialised"]

    !txSubmission2Tracer  <-  newTrace registry ["TxSubmission", "Remote"]

    !keepAliveTracer  <-  newTrace registry ["KeepAlive", "Remote"]

    !peerSharingTracer  <-  newTrace registry ["PeerSharing", "Remote"]

    !txPerasCertDiffusion  <-  newTrace registry ["Peras", "Cert", "Remote"]

    !txPerasVoteDiffusion  <-  newTrace registry ["Peras", "Vote", "Remote"]

    pure $ NtN.Tracers
      { NtN.tChainSyncTracer = chainSyncTracer
      , NtN.tChainSyncSerialisedTracer = chainSyncSerialisedTr
      , NtN.tBlockFetchTracer = blockFetchTr
      , NtN.tBlockFetchSerialisedTracer = blockFetchSerialisedTr
      , NtN.tTxSubmission2Tracer = txSubmission2Tracer
      , NtN.tKeepAliveTracer = keepAliveTracer
      , NtN.tPeerSharingTracer = peerSharingTracer
      , NtN.tPerasCertDiffusionTracer = txPerasCertDiffusion
      , NtN.tPerasVoteDiffusionTracer = txPerasVoteDiffusion
      }

mkDiffusionTracers ::
    ( LogFormatting
        ( Mux.WithBearer
            (ConnectionId RemoteAddress)
            Mux.Trace
        )
    ) =>
    Registry ->
    IO (Cardano.Diffusion.CardanoTracers IO)
mkDiffusionTracers registry = do

    !dtMuxTr   <-  newTrace registry ["Net", "Mux", "Remote"]

    !dtChannelTracer <- newTrace registry ["Net", "Mux", "Remote", "Channel"]

    -- Mux.BearerTrace's TraceEmitDeltaQ has no severity (network-mux,
    -- Network/Mux/Tracing.hs), so the documentation would warn.
    !dtBearerTracer <- newTraceWith undocumented registry ["Net", "Mux", "Remote", "Bearer"]

    !dtHandshakeTracer <- newTrace registry ["Net", "Handshake", "Remote"]

    !dtLocalMuxTr   <-  newTrace registry ["Net", "Mux", "Local"]

    !dtLocalChannelTracer <- newTrace registry ["Net", "Mux", "Local", "Channel"]

    !dtLocalBearerTracer <- newTraceWith undocumented registry ["Net", "Mux", "Local", "Bearer"]

    !dtLocalHandshakeTracer <- newTrace registry ["Net", "Handshake", "Local"]

    !dtDiffusionInitializationTr   <-  newTrace registry ["Startup", "DiffusionInit"]

    !localRootPeersTr  <-  newTrace registry ["Net", "Peers", "LocalRoot"]

    !publicRootPeersTr  <-  newTrace registry ["Net", "Peers", "PublicRoot"]

    !peerSelectionTr  <-  newTrace registry ["Net", "PeerSelection", "Selection"]

    !debugPeerSelectionTr  <-  newTrace registry ["Net", "PeerSelection", "Initiator"]

    !peerSelectionCountersTr  <-  newTrace registry ["Net", "PeerSelection"]

    !peerSelectionActionsTr  <-  newTrace registry ["Net", "PeerSelection", "Actions"]

    !connectionManagerTr  <-  newTrace registry ["Net", "ConnectionManager", "Remote"]

    !connectionManagerTransitionsTr  <-  newTrace registry ["Net", "ConnectionManager", "Transition"]

    !serverTr  <-  newTrace registry ["Net", "Server", "Remote"]

    !inboundGovernorTr  <-  newTrace registry ["Net", "InboundGovernor", "Remote"]

    !localInboundGovernorTr  <-  newTrace registry ["Net", "InboundGovernor", "Local"]

    !inboundGovernorTransitionsTr  <-  newTrace registry ["Net", "InboundGovernor", "Transition"]

    -- never conflate metrics of the same name with those originating from `connectionManagerTr`
    !localConnectionManagerTr  <-  newTraceWith withoutMetrics registry ["Net", "ConnectionManager", "Local"]

    !localServerTr  <-  newTrace registry ["Net", "Server", "Local"]

    !dtLedgerPeersTr   <- newTrace registry ["Net", "Peers", "Ledger"]

    -- DNSTrace's MetaTrace instance (ouroboros-network, DNSActions.hs) has no
    -- severity for the documentation, so the documentation would warn.
    !dtDnsTr  <- newTraceWith undocumented registry ["Net", "DNS"]

    pure $ Diffusion.Tracers
       { Diffusion.dtMuxTracer = dtMuxTr
       , Diffusion.dtChannelTracer = dtChannelTracer
       , Diffusion.dtBearerTracer = dtBearerTracer
       , Diffusion.dtHandshakeTracer = dtHandshakeTracer
       , Diffusion.dtLocalMuxTracer = dtLocalMuxTr
       , Diffusion.dtLocalChannelTracer = dtLocalChannelTracer
       , Diffusion.dtLocalBearerTracer = dtLocalBearerTracer
       , Diffusion.dtLocalHandshakeTracer = dtLocalHandshakeTracer
       , Diffusion.dtDiffusionTracer = dtDiffusionInitializationTr
       , Diffusion.dtTraceLocalRootPeersTracer = localRootPeersTr
       , Diffusion.dtTracePublicRootPeersTracer = publicRootPeersTr
       , Diffusion.dtTracePeerSelectionTracer = peerSelectionTr
       , Diffusion.dtDebugPeerSelectionTracer = debugPeerSelectionTr
       , Diffusion.dtTracePeerSelectionCounters = peerSelectionCountersTr
       , Diffusion.dtPeerSelectionActionsTracer = peerSelectionActionsTr
       , Diffusion.dtConnectionManagerTracer = connectionManagerTr
       , Diffusion.dtConnectionManagerTransitionTracer = connectionManagerTransitionsTr
       , Diffusion.dtServerTracer = serverTr
       , Diffusion.dtInboundGovernorTracer = inboundGovernorTr
       , Diffusion.dtLocalInboundGovernorTracer = localInboundGovernorTr
       , Diffusion.dtInboundGovernorTransitionTracer = inboundGovernorTransitionsTr
       , Diffusion.dtLocalConnectionManagerTracer = localConnectionManagerTr
       , Diffusion.dtLocalServerTracer = localServerTr
       , Diffusion.dtTraceLedgerPeersTracer = dtLedgerPeersTr
       , Diffusion.dtDnsTracer = dtDnsTr
       }
