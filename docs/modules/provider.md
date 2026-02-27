# Provider

::: {.module}
`Cardano.Node.Client.Provider`
:::

Protocol-agnostic interface for querying the Cardano blockchain.

```haskell
data Provider m = Provider
    { queryUTxOs         :: Addr -> m [(TxIn, TxOut ConwayEra)]
    , queryProtocolParams :: m (PParams ConwayEra)
    , evaluateTx         :: Tx ConwayEra -> m (EvaluateTxResult ConwayEra)
    }
```

## Fields

| Field | Description |
|-------|-------------|
| `queryUTxOs` | Look up UTxOs at an address |
| `queryProtocolParams` | Fetch current protocol parameters |
| `evaluateTx` | Evaluate script execution units for a transaction |

### `evaluateTx`

Evaluates Plutus script execution units for a fully-built transaction.
The implementation resolves all transaction inputs (spending, collateral,
and reference) from the node, fetches protocol parameters, system start,
and the hard-fork interpreter, then calls the ledger's `evalTxExUnits`
locally.

Returns a `Map` from each script purpose to either a
`TransactionScriptFailure` or the computed `ExUnits`.

## Constructors

| Function | Module | Transport |
|----------|--------|-----------|
| `mkN2CProvider` | `Cardano.Node.Client.N2C.Provider` | Unix socket (N2C) |

## Usage

```haskell
import Cardano.Node.Client.N2C.Provider (mkN2CProvider)

let provider = mkN2CProvider lsqChannel

utxos  <- queryUTxOs provider myAddress
pp     <- queryProtocolParams provider
exUnits <- evaluateTx provider mySignedTx
```
