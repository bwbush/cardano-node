# Configurable transaction logging

The trace dispatcher can retain complete ledger transaction identifiers without repeating transaction dumps in supported mempool and submission events. This is an opt-in representation change, not a change to transaction processing, trace selection, or network protocols.

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

The node reads this policy when constructing its dispatcher tracers. Restart to change it. There is no per-event configuration file access and no new dispatcher detail level. Existing namespace selection, severity filters, detail levels, frequency limits, and backends still apply; this policy does not enable events that those settings disable. It does not apply to the legacy logging system.

Do not assume an older node recognizes this configuration. Archive the node revision and effective configuration with each dataset. The existing trace-configuration reflection describes dispatcher settings, not this node-local policy.

## Fields and coverage

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

For `LeiosFetch.Remote.Send/Receive.Block`, the nested message gains `txReferenceSchema: 1`, `txReferenceDomain: "blake2b-256-serialized-transaction"`, and `txReferences`: an ordered array of objects with zero-based `index`, full `serializedTxHash`, and `bytes` (serialized transaction length). Existing `ebHash` identifies the body. Order and multiplicity are preserved; consumers must validate them, not sort or deduplicate them.

For `LeiosFetch.Remote.Send/Receive.BlockTxs`, the same metadata accompanies `txReferences` containing full `serializedTxHash` and `bytes` for each actual transaction, in reply order. No transaction body is emitted. Existing request/reply bitmaps and summary fields remain unchanged. Hashing replies adds CPU/allocation work, including for repeated replies: this is an instrumentation cost, not a free cache lookup. Membership hashes are already present in the EB. Measure overhead under the intended workload before using this configuration for performance conclusions.

In the pinned consensus implementation, bitmap `(chunk, word)` addresses EB positions `64*chunk + i`, where bit `63-i` selects position `i`. Chunks and selected positions are ascending. Validate the bitmap population, vector bounds, ordered reply hashes and byte sizes before expanding references. Retain the original message identity so expanding one batch into many transaction rows does not multiply message byte totals.

These fields describe observed messages, not cache admission, validation success, mempool removal, or chain adoption. An EB member reference is not a received transaction body. A send is not evidence of delivery at another node. Preserve unresolved mappings and missing prerequisites; a missing ledger-ID match is not evidence that the node never received the transaction. A ledger-ID bridge requires independently identified signed transaction bytes and must allow multiple serialized variants per ledger body. This formatter does not decode arbitrary Leios transaction bytes to obtain ledger IDs, and does not add producer-only or per-transaction cache-transition instrumentation.

## Compatibility and suppression boundaries

Existing configurations retain existing field names, values, types, and detail-level decisions in the covered formatters. The original `LogFormatting` instances and `mkDispatchTracers` entry point remain available; the configured path uses an adapter without changing the dispatcher class or forwarder protocol. Namespace metadata and metrics delegate to the original events. Existing `cardano-tracer` collectors can continue transporting and storing machine-formatted records.

New fields are additive and opt-in. Consumers with strict schemas may still need updates. A consumer that parses transaction dumps cannot continue unchanged after an operator requests their suppression. Do not substitute an empty transaction string or array: omitted content is not an empty transaction or batch.

Suppression prevents construction of the supported transaction dumps, including their witnesses and script data. Human output for adapted events falls back to the configured machine object, avoiding a second legacy renderer that might expose the dump. Rejection diagnostics retain their existing detail-level behavior. **This is not a privacy-redaction guarantee:** errors and other diagnostics can contain transaction-related values.

This implementation does not transform every diagnostic that could mention a transaction. Shared transaction-state snapshots, transaction-logic debugging, arbitrary error text, block-fetch formatting, local transaction-monitor traces, and Leios traces other than the opt-in fetch references are not changed. Operators requiring a broader suppression guarantee must review those paths and their namespace configuration separately. The policy does not remove data from already archived logs.

## Tests and remaining validation

From the repository root, in its development environment:

```sh
cabal test cardano-node:transaction-logging-test
cabal test cardano-node:transaction-logging-integration
nix build .#cardano-node
```

The lightweight suite tests configuration parsing, independent settings, legacy passthrough, deliberately colliding prefixes, batch ordering, and non-evaluation of suppressed payload expressions. Integration tests exercise the actual peer formatters and adapter metadata/metrics. These tests are not a live-node performance result or proof of complete capture.

Before deployment or upstream submission, extend coverage with real mempool transactions and rejection fixtures; exercise startup configuration through a node and unchanged collector; measure CPU, allocation, output volume, and forwarding drops under representative bursts; and complete the diagnostic-path audit. Preserve the same workload, trace selection, and collector settings when comparing legacy and configured output. Do not turn on new high-volume event families during that comparison.
