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

The Leios transaction-reference hash identifies serialized transaction bytes, including witnesses, and is a different identifier domain. This change does not compute or log that mapping. Do not join these hashes to ledger IDs simply because both are full-length hexadecimal strings.

## Compatibility and suppression boundaries

Existing configurations retain existing field names, values, types, and detail-level decisions in the covered formatters. The original `LogFormatting` instances and `mkDispatchTracers` entry point remain available; the configured path uses an adapter without changing the dispatcher class or forwarder protocol. Namespace metadata and metrics delegate to the original events. Existing `cardano-tracer` collectors can continue transporting and storing machine-formatted records.

New fields are additive and opt-in. Consumers with strict schemas may still need updates. A consumer that parses transaction dumps cannot continue unchanged after an operator requests their suppression. Do not substitute an empty transaction string or array: omitted content is not an empty transaction or batch.

Suppression prevents construction of the supported transaction dumps, including their witnesses and script data. Human output for adapted events falls back to the configured machine object, avoiding a second legacy renderer that might expose the dump. Rejection diagnostics retain their existing detail-level behavior. **This is not a privacy-redaction guarantee:** errors and other diagnostics can contain transaction-related values.

This initial implementation does not transform every diagnostic that could mention a transaction. Shared transaction-state snapshots, transaction-logic debugging, arbitrary error text, block-fetch formatting, local transaction-monitor traces, and Leios-specific traces are not changed. Operators requiring a broader suppression guarantee must review those paths and their namespace configuration separately. The policy does not remove data from already archived logs.

## Tests and remaining validation

From the repository root, in its development environment:

```sh
cabal test cardano-node:transaction-logging-test
cabal test cardano-node:transaction-logging-integration
nix build .#cardano-node
```

The lightweight suite tests configuration parsing, independent settings, legacy passthrough, deliberately colliding prefixes, batch ordering, and non-evaluation of suppressed payload expressions. Integration tests exercise the actual peer formatters and adapter metadata/metrics. These tests are not a live-node performance result or proof of complete capture.

Before deployment or upstream submission, extend coverage with real mempool transactions and rejection fixtures; exercise startup configuration through a node and unchanged collector; measure CPU, allocation, output volume, and forwarding drops under representative bursts; and complete the diagnostic-path audit. Preserve the same workload, trace selection, and collector settings when comparing legacy and configured output. Do not turn on new high-volume event families during that comparison.
