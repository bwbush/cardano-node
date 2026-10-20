# Configurable transaction logging

The trace dispatcher can retain complete ledger transaction identifiers without repeating transaction dumps in supported mempool and submission events. This is an opt-in representation change, not a change to transaction processing, trace selection, or network protocols.

This branch now targets the w42 prototype. Opt-in identity pairs and Leios references use **schema 2**, identifying `leios-block-transaction` encoding. Historical w40a output used schema 1. Do not feed schema-2 captures to a schema-1 decoder: both the transaction encoding and the fetch-reply context changed upstream.

## Configuration

Add this top-level object to the node configuration:

```json
{
  "TraceOptionTransactions": {
    "fullTxIds": true,
    "suppressTxBodies": true
  }
}
```

Both settings default to `false`. Omitting the object, or supplying an empty object, preserves legacy behavior. The settings are independent: operators may add full identifiers while retaining transaction dumps, or suppress dumps without adding full identifiers. Unknown fields, explicit nulls, and non-boolean values inside this object are startup errors.

The node reads this policy when constructing its dispatcher tracers. Restart to change it. There is no per-event configuration file access and no new dispatcher detail level. Existing namespace selection, severity filters, detail levels, frequency limits, and backends still apply; this policy does not enable events that those settings disable. This baseline no longer supports the old legacy logging system.

Do not assume an older node recognizes this configuration. Archive the node revision and effective configuration with each dataset. The existing trace-configuration reflection describes dispatcher settings, not this node-local policy.

## Fields and coverage

### Opt-in mempool identity pairs

The additional, independent top-level policy below links ledger IDs to the hash of the node's Leios serialization, without logging transaction bodies:

```json
{
  "TraceOptionTransactionIdentity": {
    "includeMempoolIdentityPairs": true
  }
}
```

The default is false. Omitted/empty policies preserve existing output and do not force extra serialization or hashing. Explicit nulls, unknown fields, and non-boolean values within this object are startup errors. Existing constructors and the older transaction/reference option types are unchanged. Install a supporting binary before enabling this option: older binaries do not provide the new field, and successful startup alone is not evidence that an unfamiliar top-level policy was read. Namespace selection still controls which events are emitted. Restart to change the setting; it is not a hot-reload policy.

This increment applies only to decoded transaction objects in `Mempool.AttemptAdd`, `AddedTx`, `RejectedTx`, `RemoveTxs`, and the invalidated transactions in `ManuallyRemovedTxs`. ID-only manual-removal requests cannot provide pairs. Other submission, block-fetch, and Leios-only paths are unchanged. The nested transaction gains:

```json
{
  "txIdentity": {
    "schema": 2,
    "encoding": "leios-block-transaction",
    "status": "mapped",
    "ledgerTxId": "<64 lowercase hexadecimal characters>",
    "serializedTxHash": "<64 lowercase hexadecimal characters>",
    "bytes": 202,
    "serializedDomain": "blake2b-256-serialized-transaction",
    "derivation": "node-leios-serialization"
  }
}
```

The byte count above is illustrative. The implementation uses consensus `leiosTxBytesOfGenTx`, hashes exactly its returned bytes, and retains the ledger ID separately. At this baseline the serializer is supported for Dijkstra; unsupported eras emit `status: "unsupported_era"` with the schema, ledger ID, domain, and derivation, but no serialized hash or byte count. This avoids pretending an unavailable pair was established.

This performs serialization and hashing, but no additional transaction decoding, signature validation, or script execution. Each logged occurrence incurs that cost; there is no node-side identity cache or deduplication. No bodies are added by the identity field; the independent body-suppression policy still governs older fields. One ledger ID can have multiple witnessed serialized variants. The emitted hash describes the node's serialization, not an assertion that every wire representation has identical bytes. Join only matching serialized hashes and sizes, retain the mapping's provenance, and reject conflicts. A later or other-observer mapping can resolve earlier references without establishing that the earlier observer decoded, admitted, validated, or retained the transaction.

### Transaction payload fields

| Event path | Opt-in representation |
|---|---|
| `Mempool.AttemptAdd`, `AddedTx`, `RejectedTx`, `RemoveTxs` | Transaction objects gain `txIdFull`. With suppression, retain the legacy eight-character `txid`, add `txBodyOmitted: true`, and omit the textual `tx` dump. |
| `Mempool.ManuallyRemovedTxs` | Add `txsRemovedFull`; apply transaction-object policy to each invalidated transaction. Existing `txsRemoved` remains unchanged. |
| `TxSubmission.LocalServer.ReceivedTx` and local wire `SubmitTx` | Add `txIdFull`. Replies without a transaction identity remain unchanged. |
| Peer wire `ReplyTxIds`, `RequestTxs`, `ReplyTxs` | Add ordered `txIdsFull` arrays. Configured transaction replies also report `numTxs`; suppression replaces the textual `txs` dump with `txBodyOmitted: true`. |
| `TxSubmission.TxOutbound` requests/replies | Add full ID arrays; suppress reply dumps before constructing their textual representation. Configured replies also report `numTxs`. |
| `TxSubmission.TxInbound` collected/requested/admitted/rejected lists | Add full ID arrays, retaining existing counts, durations, and legacy fields. |

Full identifiers are lowercase hexadecimal representations of the ledger transaction ID, not a search through rendered transaction text. Batch order and repeated identifiers are preserved. A transaction ID identifies a body, not a submission occurrence; retries, re-admissions, and rollbacks still require occurrence-aware analysis.

The Leios transaction-reference hash identifies serialized transaction bytes, including witnesses, and is a different identifier domain. The transaction options do not compute that mapping. The separate Leios option below exposes serialized hashes but does not equate them to ledger IDs. Do not join these hashes to ledger IDs simply because both are full-length hexadecimal strings.

## Opt-in Leios fetch references

```json
{"TraceOptionLeios": {"includeTxReferences": true}}
```

The default is false. Unknown fields, explicit nulls, and non-boolean values fail startup. This setting is independent of `TraceOptionTransactions` and detail level, and leaves both existing tracer-construction APIs intact. No collector change is required.

For `LeiosFetch.Remote.Send/Receive.Block`, the nested message gains `txReferenceSchema: 2`, `txReferenceDomain: "blake2b-256-serialized-transaction"`, `txEncoding: "leios-block-transaction"`, and `txReferences`: an ordered array of objects with zero-based `index`, full `serializedTxHash`, and `bytes` (serialized transaction length). Existing `ebHash` identifies the body. Order and multiplicity are preserved; consumers must validate them, not sort or deduplicate them.

For `LeiosFetch.Remote.Send/Receive.BlockTxs`, the same metadata accompanies `txReferences` containing full `serializedTxHash` and `bytes` for each actual transaction, in reply order. No transaction body is emitted. In w42 a reply no longer carries an endorser-block hash, slot, or bitmap; the formatter does not invent these fields. Requests still identify their block and bitmap. Hashing replies adds CPU/allocation work, including for repeated replies: this is an instrumentation cost, not a free cache lookup. Membership hashes are already present in the endorser block. Measure overhead under the intended workload before using this configuration for performance conclusions.

In the pinned consensus implementation, bitmap `(chunk, word)` addresses EB positions `64*chunk + i`, where bit `63-i` selects position `i`. Chunks and selected positions are ascending. Validate the bitmap population, vector bounds, ordered reply hashes and byte sizes before expanding references. Retain the original message identity so expanding one batch into many transaction rows does not multiply message byte totals.

These fields describe observed messages, not cache admission, validation success, mempool removal, or chain adoption. An EB member reference is not a received transaction body. A send is not evidence of delivery at another node. Preserve unresolved mappings and missing prerequisites; a missing ledger-ID match is not evidence that the node never received the transaction. A ledger-ID bridge requires independently identified signed transaction bytes and must allow multiple serialized variants per ledger body. This formatter does not decode arbitrary Leios transaction bytes to obtain ledger IDs, and does not add producer-only or per-transaction cache-transition instrumentation.

### Schema and upgrade checks

EB means endorser block. Schema 2 adds fields only to the nested `MsgLeiosBlock` and `MsgLeiosBlockTxs` objects, in both directions and at every detail level. Membership entries have `index`, `serializedTxHash`, and `bytes`; reply entries have only `serializedTxHash` and `bytes`. The reply array describes actual supplied bytes, not merely a copy of requested membership. Neither message adds a ledger transaction ID. An empty array means an observed empty vector; an absent field means references were not supplied by that formatter, not an empty vector. Consumers should dispatch on `txReferenceSchema`, `txReferenceDomain`, and `txEncoding`, rejecting or quarantining unsupported interpretations while retaining the raw message.

The reply has no derived `index`, block association, or echoed bitmap. Its actual serialized hashes and sizes still support body-receipt evidence after identity resolution. Associating it with a particular request requires verified connection/protocol ordering or additional consensus instrumentation; block membership alone is not a unique request association. Preserve ambiguity across reconnects, missing logs, and transactions shared by multiple endorser blocks.

When upgrading consensus, ledger, node, or CLI dependencies, repeat the bitmap-boundary, reply-order, default-output, and adapter tests. Recheck the source encoding for EB identity and serialized transaction hashing; retain the exact signed transaction files and verify both their ledger IDs with the actual CLI and their serialized hashes/sizes against newly captured replies. Record executable versions/checksums, effective options, dependency pins, and raw evidence. Replaying an old capture qualifies the new analyzer against that capture; it does not qualify a newly deployed formatter. A quiet network with no relevant replies is insufficient evidence, not a successful canary. No upgrade is implied by this checklist.

## Compatibility and suppression boundaries

Existing configurations retain existing field names, values, types, and detail-level decisions in the covered formatters. The original `LogFormatting` instances and `mkDispatchTracers` entry point remain available; the configured path uses an adapter without changing the dispatcher class or forwarder protocol. Namespace metadata and metrics delegate to the original events. Existing `cardano-tracer` collectors can continue transporting and storing machine-formatted records.

New fields are additive and opt-in. Consumers with strict schemas may still need updates. A consumer that parses transaction dumps cannot continue unchanged after an operator requests their suppression. Do not substitute an empty transaction string or array: omitted content is not an empty transaction or batch.

Suppression prevents construction of the supported transaction dumps, including their witnesses and script data. Human output for adapted events falls back to the configured machine object, avoiding a second legacy renderer that might expose the dump. Rejection diagnostics retain their existing detail-level behavior. **This is not a privacy-redaction guarantee:** errors and other diagnostics can contain transaction-related values.

This implementation does not transform every diagnostic that could mention a transaction. Shared transaction-state snapshots, transaction-logic debugging, arbitrary error text, block-fetch formatting, local transaction-monitor traces, and Leios traces other than the opt-in fetch references are not changed. Operators requiring a broader suppression guarantee must review those paths and their namespace configuration separately. The policy does not remove data from already archived logs.

Distinguish the decoded and serialized block-fetch instances. The decoded `BlockFetch.Remote` message has no body-rendering `forHuman` override; its send/receive and peer envelopes add small human text. The separate `BlockFetch.Remote.Serialised` instance renders serialized bytes, including in its machine object. Enabling that namespace changes the cost and content materially. A collector configured for machine output does not by itself prevent the node's forwarder from evaluating and transporting human text. Reference-enabled LeiosFetch and body-suppressed transaction adapters explicitly return empty human text, avoiding their legacy human renderer. These are source-path distinctions, not a sustained-load performance guarantee.

## Tests and remaining validation

From the repository root, in its development environment:

```sh
cabal test cardano-node:transaction-logging-test
cabal test cardano-node:transaction-logging-integration
nix build .#cardano-node
```

The lightweight suite tests configuration parsing, all combinations of the independent transaction/reference settings, legacy passthrough, deliberately colliding prefixes, batch ordering, and non-evaluation of suppressed payload expressions. Integration tests exercise the actual peer formatters and adapter metadata/metrics; literal bitmap positions across 64-bit chunk boundaries; reply order and duplicates; encoded legacy-object equality at each detail level; send/receive reference-option wiring; and non-evaluation of decoded block bodies in human output. These are formatter/adapter tests, not a replacement for startup-through-collector acceptance, a cross-version JSON canonicalization promise, a live-node performance result, or proof of complete capture.

Before deployment or upstream submission, extend coverage with real mempool transactions and rejection fixtures; exercise startup configuration through a node and unchanged collector; measure CPU, allocation, output volume, and forwarding drops under representative bursts; and complete the diagnostic-path audit. Preserve the same workload, trace selection, and collector settings when comparing legacy and configured output. Do not turn on new high-volume event families during that comparison.
