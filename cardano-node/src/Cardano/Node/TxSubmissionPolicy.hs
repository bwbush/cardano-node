{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

-- | Directional policy at the existing diffusion boundary. In TxSubmission2
-- the responder requests peer transactions and admits them to our mempool;
-- the initiator serves our mempool to the peer.
module Cardano.Node.TxSubmissionPolicy
  ( runWithTxSubmissionPolicy
  , suppressInboundTxSubmission
  , suppressInboundMiniProtocol
  ) where

import           Cardano.Network.NodeToClient (LocalAddress)
import           Cardano.Network.NodeToNode (RemoteAddress, txSubmissionMiniProtocolNum)
import           Control.Monad (forever)
import           Control.Monad.Class.MonadTimer.SI (MonadDelay, threadDelay)
import qualified Network.Mux as Mux
import qualified Ouroboros.Consensus.Node as Node
import           Ouroboros.Network.Diffusion.Types (Applications (..))
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolCb (..),
                   RunMiniProtocol (..))
import           Ouroboros.Network.PeerSelection.PeerSharing.Codec
                   (decodeRemoteAddress, encodeRemoteAddress)

-- | Keep the stock entry point when disabled. When enabled, use precisely its
-- standard arguments, changing only the applications passed to diffusion.
runWithTxSubmissionPolicy
  :: Node.RunNode blk
  => Bool
  -> Node.RunNodeArgs IO RemoteAddress LocalAddress blk
  -> Node.StdRunNodeArgs IO blk
  -> IO ()
runWithTxSubmissionPolicy False = Node.run
runWithTxSubmissionPolicy True = \args stdArgs -> do
  low <- Node.stdLowLevelRunNodeArgsIO args stdArgs
  Node.runWith args encodeRemoteAddress decodeRemoteAddress
    low { Node.llrnRunDataDiffusion = \kernel consensusArgs apps ->
            Node.llrnRunDataDiffusion low kernel consensusArgs
              (suppressInboundTxSubmission True apps)
        }

-- | Leave initiator-only and local-socket applications, protocol versions,
-- policies, and peer selection intact. Map all negotiated duplex versions and
-- protocol temperatures, replacing exactly the TxSubmission2 responder.
suppressInboundTxSubmission
  :: MonadDelay m
  => Bool
  -> Applications na nv nd la lv ld flags m a
  -> Applications na nv nd la lv ld flags m a
suppressInboundTxSubmission False apps = apps
suppressInboundTxSubmission True apps =
  apps { daApplicationInitiatorResponderMode =
           fmap (fmap (map suppressInboundMiniProtocol))
             (daApplicationInitiatorResponderMode apps)
       }

suppressInboundMiniProtocol
  :: MonadDelay m
  => MiniProtocol Mux.InitiatorResponderMode ic rc bytes m a b
  -> MiniProtocol Mux.InitiatorResponderMode ic rc bytes m a b
suppressInboundMiniProtocol protocol
  | miniProtocolNum protocol == txSubmissionMiniProtocolNum
  , InitiatorAndResponderProtocol outbound _ <- miniProtocolRun protocol =
      protocol { miniProtocolRun = InitiatorAndResponderProtocol outbound parked }
  | otherwise = protocol
 where
  -- Do not run the inbound handler, request identifiers/bodies, or touch its
  -- mempool writer. Keep the mini-protocol registered and cancellable by mux:
  -- returning immediately would terminate the connection and affect sync.
  -- TxSubmission2 is pull-based, so a compliant peer waits for our requests.
  -- A timer keeps the wait alive even through major garbage collection. An
  -- unrooted @atomically retry@ can raise BlockedIndefinitelyOnSTM instead.
  parked = MiniProtocolCb $ \_ _ -> forever (threadDelay 60)
