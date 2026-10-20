{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Cardano.Node.Tracing.TransactionLogging

import           Control.Monad (forM_, unless)
import           Data.Aeson (Value (..), eitherDecode, encode, object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Either (isLeft)
import qualified Data.Text as Text

main :: IO ()
main = do
  let parseConfig bytes = eitherDecode bytes >>= parseEither parseTransactionLogOptions
      legacy = defaultTransactionLogOptions
      compact = TransactionLogOptions True True
      hashA = "deadbeef" <> Text.replicate 56 "0"
      hashB = "deadbeef" <> Text.replicate 56 "1"
      original = KeyMap.fromList [("txid", String "deadbeef"), ("tx", String "body")]
      check name condition = do
        unless condition $ fail name
        putStrLn $ "PASS: " <> name

  check "absent policy preserves defaults" $ parseConfig "{}" == Right legacy
  let parseIdentity bytes = eitherDecode bytes >>= parseEither parseTransactionIdentityLogOptions
      identities = TransactionIdentityLogOptions True
  check "absent identity policy preserves defaults" $
    parseIdentity "{}" == Right defaultTransactionIdentityLogOptions
  check "empty identity policy preserves defaults" $
    parseIdentity "{\"TraceOptionTransactionIdentity\":{}}" == Right defaultTransactionIdentityLogOptions
  forM_ [False, True] $ \enabled -> check ("identity policy roundtrip " <> show enabled) $
    parseIdentity (encode $ object ["TraceOptionTransactionIdentity" .= TransactionIdentityLogOptions enabled]) ==
      Right (TransactionIdentityLogOptions enabled)
  forM_ [ "{\"TraceOptionTransactionIdentity\":null}"
        , "{\"TraceOptionTransactionIdentity\":{\"includeMempoolIdentityPairs\":null}}"
        , "{\"TraceOptionTransactionIdentity\":{\"includeMempoolIdentityPairs\":\"true\"}}"
        , "{\"TraceOptionTransactionIdentity\":{\"includeIdentityPairs\":true}}"
        ] $ \bad -> check ("reject invalid identity policy " <> BSL.unpack bad) $
          isLeft (parseIdentity bad)
  check "disabled identities do not evaluate ID, serialization or hash" $
    transactionIdentityField defaultTransactionIdentityLogOptions
      (error "ID forced") (error "serialization forced") == mempty
  check "enabled identities preserve both hash domains and size" $
    KeyMap.lookup "txIdentity" (transactionIdentityField identities hashA (Just (hashB,202))) ==
      Just (object ["schema" .= (2 :: Int), "encoding" .= ("leios-block-transaction" :: Text.Text), "ledgerTxId" .= hashA,
        "serializedDomain" .= ("blake2b-256-serialized-transaction" :: Text.Text),
        "derivation" .= ("node-leios-serialization" :: Text.Text),
        "status" .= ("mapped" :: Text.Text), "serializedTxHash" .= hashB, "bytes" .= (202 :: Int)])
  check "unsupported era is explicit" $
    KeyMap.lookup "txIdentity" (transactionIdentityField identities hashA Nothing) ==
      Just (object ["schema" .= (2 :: Int), "encoding" .= ("leios-block-transaction" :: Text.Text), "ledgerTxId" .= hashA,
        "serializedDomain" .= ("blake2b-256-serialized-transaction" :: Text.Text),
        "derivation" .= ("node-leios-serialization" :: Text.Text),
        "status" .= ("unsupported_era" :: Text.Text)])
  let parseLeios bytes = eitherDecode bytes >>= parseEither parseLeiosReferenceLogOptions
  check "absent Leios policy preserves defaults" $ parseLeios "{}" == Right defaultLeiosReferenceLogOptions
  check "Leios references opt in independently" $
    parseLeios "{\"TraceOptionLeios\":{\"includeTxReferences\":true}}" == Right (LeiosReferenceLogOptions True)
  check "empty Leios policy preserves defaults" $
    parseLeios "{\"TraceOptionLeios\":{}}" == Right defaultLeiosReferenceLogOptions
  forM_ [False, True] $ \refs -> forM_ [False, True] $ \ids -> forM_ [False, True] $ \bodies -> do
    let config = encode $ object ["TraceOptionTransactions" .= TransactionLogOptions ids bodies,
                                  "TraceOptionLeios" .= LeiosReferenceLogOptions refs,
                                  "NetworkMagic" .= (164 :: Int)]
    check ("independent startup policies " <> show (refs, ids, bodies)) $
      parseLeios config == Right (LeiosReferenceLogOptions refs) &&
      parseConfig config == Right (TransactionLogOptions ids bodies)
  forM_ [ "{\"TraceOptionLeios\":null}"
        , "{\"TraceOptionLeios\":{\"includeTxReferences\":null}}"
        , "{\"TraceOptionLeios\":{\"includeTxReference\":true}}"
        , "{\"TraceOptionLeios\":{\"includeTxReferences\":\"true\"}}"
        ] $ \bad -> check ("reject invalid Leios policy " <> BSL.unpack bad) $
          isLeft (parseLeios bad)
  check "empty policy preserves defaults" $
    parseConfig "{\"TraceOptionTransactions\":{}}" == Right legacy
  check "unrelated node settings are accepted" $
    parseConfig "{\"NetworkMagic\":164}" == Right legacy
  forM_ [False, True] $ \ids -> forM_ [False, True] $ \bodies -> do
    let opts = TransactionLogOptions ids bodies
    check ("round trip " <> show opts) $
      parseConfig (encode $ object ["TraceOptionTransactions" .= opts]) == Right opts
  forM_ [ "{\"TraceOptionTransactions\":null}"
        , "{\"TraceOptionTransactions\":true}"
        , "{\"TraceOptionTransactions\":{\"fullTxIds\":null}}"
        , "{\"TraceOptionTransactions\":{\"fullTxIds\":\"true\"}}"
        , "{\"TraceOptionTransactions\":{\"fullTxId\":true}}"
        ] $ \bad -> check ("reject invalid policy " <> BSL.unpack bad) $
          isLeft (parseConfig bad)
  check "legacy does not evaluate the extra ID" $
    transactionObject legacy (error "unexpected hash calculation") original == original
  check "full ID addition retains legacy fields" $
    transactionObject (TransactionLogOptions True False) hashA original ==
      original <> KeyMap.singleton "txIdFull" (String hashA)
  check "suppression does not evaluate legacy rendering" $
    BSL.length (encode $ transactionObject compact hashA (error "body rendered")) > 0
  check "suppression is independent of full ID selection" $
    transactionObject (TransactionLogOptions False True) hashA original ==
      KeyMap.fromList [("txid", String "deadbeef"), ("txBodyOmitted", Bool True)]
  check "colliding prefixes retain different full identities" $
    transactionObject compact hashA original /= transactionObject compact hashB original
  check "batch order and duplicates survive" $
    fullTxIdsField compact "txIdsFull" [hashB, hashA, hashB] ==
      KeyMap.singleton "txIdsFull" (toJSON [hashB, hashA, hashB])
  check "disabled lists do not evaluate IDs" $
    fullTxIdsField legacy "txIdsFull" (error "IDs evaluated") == mempty
  check "peer payload suppressed before Show" $
    transactionPayloadField compact "txs" (error "Show evaluated") ==
      KeyMap.singleton "txBodyOmitted" (Bool True)
  check "legacy peer payload unchanged" $
    transactionPayloadField legacy "txs" "body" == KeyMap.singleton "txs" (String "body")
