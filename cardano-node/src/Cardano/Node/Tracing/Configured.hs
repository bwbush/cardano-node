{-# LANGUAGE ScopedTypeVariables #-}

-- | A node-local formatting adapter, leaving the dispatcher's public class and
-- forwarding protocol unchanged. Filtering and metrics retain the original
-- event's metadata; only representation is replaced.
module Cardano.Node.Tracing.Configured
  ( ConfiguredTrace (..)
  ) where

import           Cardano.Logging hiding (detail)
import           Data.Aeson (Object)

data ConfiguredTrace a = ConfiguredTrace
  { useMachineForHuman :: Bool
  , machineFormat :: DetailLevel -> a -> Object
  , originalEvent :: a
  }

instance LogFormatting a => LogFormatting (ConfiguredTrace a) where
  forMachine detail (ConfiguredTrace _ format event) = format detail event
  -- Empty text requests the dispatcher's machine-object fallback. In particular
  -- do not call an old human renderer that may still print transaction bodies.
  forHuman (ConfiguredTrace compact _ event)
    | compact = mempty
    | otherwise = forHuman event
  asMetrics = asMetrics . originalEvent

instance MetaTrace a => MetaTrace (ConfiguredTrace a) where
  namespaceFor = nsCast . namespaceFor . originalEvent
  severityFor ns event = severityFor (nsCast ns :: Namespace a) (originalEvent <$> event)
  privacyFor ns event = privacyFor (nsCast ns :: Namespace a) (originalEvent <$> event)
  detailsFor ns event = detailsFor (nsCast ns :: Namespace a) (originalEvent <$> event)
  documentFor ns = documentFor (nsCast ns :: Namespace a)
  metricsDocFor ns = metricsDocFor (nsCast ns :: Namespace a)
  allNamespaces = map nsCast (allNamespaces :: [Namespace a])
