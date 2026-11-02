{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Cardano.Node.Configuration.CardanoConfigResolve
import           Cardano.Node.Configuration.POM
import           Cardano.Node.Parsers (nodeCLIParser)
import           Cardano.Node.TxSubmissionPolicy
import           Control.Exception (SomeException, try)
import           Control.Monad (filterM, forM_, unless, void)
import           Data.Aeson (Value (..), eitherDecodeFileStrict', encode)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (toList)
import           Data.IORef
import           Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import           Ouroboros.Network.Diffusion.Types (Applications (..))
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Protocol.Handshake.Version
import qualified Options.Applicative as Opt
import           System.Directory (copyFile, doesFileExist, listDirectory)
import           System.Environment (withArgs)
import           System.FilePath ((</>))
import           System.IO.Temp (withSystemTempDirectory)
import           System.Timeout (timeout)

check :: String -> Bool -> IO ()
check name result = do
  unless result $ fail name
  putStrLn $ "PASS: " <> name

parseCli :: [String] -> IO PartialNodeConfiguration
parseCli flags = case Opt.execParserPure Opt.defaultPrefs (Opt.info nodeCLIParser mempty) ("run" : flags) of
  Opt.Success pnc -> pure pnc
  Opt.Failure e -> fail (fst (Opt.renderFailure e "cardano-node"))
  Opt.CompletionInvoked _ -> fail "unexpected completion"

main :: IO ()
main = do
  dirs <- filterM (doesFileExist . (</> "config.json"))
    ["test/cardano-config-compare/config", "cardano-node/test/cardano-config-compare/config"]
  fixtures <- case dirs of
    d : _ -> pure d
    [] -> fail "missing existing node configuration fixtures"
  -- Repeat in one process, switching ON -> OFF, to catch retained policy state.
  forM_ [1 :: Int, 2] $ \roundNumber -> do
    putStrLn $ "Round " <> show roundNumber
    withSystemTempDirectory "tx-submission-policy" $ \dir -> do
      names <- listDirectory fixtures
      forM_ names $ \name -> copyFile (fixtures </> name) (dir </> name)
      forM_ ["config.json", "config-envelope.json"] $ \name -> do
        Right (Object original) <- eitherDecodeFileStrict' (fixtures </> name)
        let path = dir </> name
            resolve field flags = do
              let o = maybe original (\v -> KeyMap.insert "SuppressInboundTxSubmission" v original) field
              LBS.writeFile path (encode (Object o))
              let args = ["--config", path] <> flags
              cli <- parseCli args
              withArgs ("run" : args) $ buildNodeConfiguration SkipCrossCheck cli
        forM_ [(Nothing, [], False), (Just (Bool False), [], False),
               (Just (Bool True), [], True),
               (Just (Bool False), ["--suppress-inbound-tx-submission"], True),
               (Just (Bool True), ["--no-suppress-inbound-tx-submission"], False)] $
          \(field, flags, expected) -> do
            resolved <- resolve field flags
            check (name <> " file/CLI " <> show (field, flags)) $
              ncSuppressInboundTxSubmission (rncConfiguration resolved) == expected
            checkCallbacks (ncSuppressInboundTxSubmission (rncConfiguration resolved))
        forM_ [Null, String "true", Number 1] $ \bad -> do
          result <- try (resolve (Just bad) []) :: IO (Either SomeException ResolvedNodeConfiguration)
          check (name <> " rejects invalid policy " <> show bad <> " for that field") $
            case result of
              Left e -> "SuppressInboundTxSubmission" `isInfixOf` show e
              Right _ -> False

checkCallbacks :: Bool -> IO ()
checkCallbacks enabled = do
  called <- newIORef ([] :: [Int])
  let cb :: Int -> MiniProtocolCb ctx LBS.ByteString IO ()
      cb n = MiniProtocolCb $ \_ _ -> modifyIORef' called (<> [n]) >> pure ((), Nothing)
      proto :: Int -> RunMiniProtocol mode ic rc LBS.ByteString IO a b
            -> MiniProtocol mode ic rc LBS.ByteString IO a b
      proto n action = MiniProtocol (MiniProtocolNum (fromIntegral n)) StartOnDemand (MiniProtocolLimits 4096) action
      duplex n = proto n (InitiatorAndResponderProtocol (cb n) (cb (-n)))
      -- Literal wire numbers, including TxSubmission2=4 and ChainSync=2 /
      -- BlockFetch=3, keep the test independent of the implementation's constant.
      bundle = TemperatureBundle (WithHot [duplex 2, duplex 3, duplex 4, duplex 14, duplex 15])
                                 (WithWarm [duplex 8]) (WithEstablished [duplex 10])
      versions :: a -> Versions Int () a
      versions app = combineVersions [simpleSingletonVersions n () (const app) | n <- [1 :: Int, 2]]
      original :: Applications () Int () () Int () () IO ()
      original = Applications
        { daApplicationInitiatorMode = versions $
            TemperatureBundle (WithHot [proto 4 (InitiatorProtocolOnly (cb 104))])
                              (WithWarm []) (WithEstablished [])
        , daApplicationInitiatorResponderMode = versions bundle
        , daLocalResponderApplication = versions $
            OuroborosApplication [proto 6 (ResponderProtocolOnly (cb 106))]
        , daRethrowPolicy = error "policy unexpectedly evaluated"
        , daReturnPolicy = error "return policy unexpectedly evaluated"
        , daRepromoteErrorDelay = error "repromote delay unexpectedly evaluated"
        , daLocalRethrowPolicy = error "local policy unexpectedly evaluated"
        , daPeerSelectionPolicy = error "peer selection unexpectedly evaluated"
        , daPeerSharingRegistry = error "peer sharing unexpectedly evaluated"
        }
      apps = suppressInboundTxSubmission enabled original
      run :: MiniProtocolCb ctx LBS.ByteString IO a -> IO (a, Maybe LBS.ByteString)
      run cb' = runMiniProtocolCb cb' (error "context forced") (error "channel touched")
      applications :: Versions Int () a -> [a]
      applications vs = [versionApplication v () | v <- Map.elems (getVersions vs)]
  forM_ (applications (daApplicationInitiatorResponderMode apps)) $ \b ->
    forM_ (concat (toList b)) $ \p -> do
      let MiniProtocolNum n = miniProtocolNum p
      check "protocol retains start strategy" (miniProtocolStart p == StartOnDemand)
      case miniProtocolRun p of
        InitiatorAndResponderProtocol outbound inbound -> do
          before <- readIORef called
          void $ run outbound
          after <- readIORef called
          check ("outbound callback preserved " <> show n) (after == before <> [fromIntegral n])
          outcome <- timeout 100000 (run inbound)
          now <- readIORef called
          if enabled && n == 4
            then check "ON: inbound handler/channel never touched; cancellable wait" (outcome == Nothing && now == after)
            else check ("stock responder preserved " <> show n) (outcome == Just ((), Nothing) && now == after <> [negate (fromIntegral n)])
  forM_ (applications (daApplicationInitiatorMode apps)) $ \b ->
    forM_ (concat (toList b)) $ \p -> case miniProtocolRun p of
      InitiatorProtocolOnly outbound -> void $ run outbound
  forM_ (applications (daLocalResponderApplication apps)) $ \a ->
    forM_ (getOuroborosApplication a) $ \p -> case miniProtocolRun p of
      ResponderProtocolOnly local -> void $ run local
  final <- readIORef called
  check "initiator-only serving and local submission preserved across versions" (drop (length final - 4) final == [104,104,106,106])
