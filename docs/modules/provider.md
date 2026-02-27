# Provider

::: {.module}
`Cardano.Node.Client.Provider`
:::

Protocol-agnostic interface for querying the Cardano blockchain.

```haskell
data Provider m = Provider
    { queryUTxOs         :: Addr -> m [(TxIn, TxOut ConwayEra)]
    , queryProtocolParams :: m (PParams ConwayEra)
    }
```

## Constructors

| Function | Module | Transport |
|----------|--------|-----------|
| `mkN2CProvider` | `Cardano.Node.Client.N2C.Provider` | Unix socket (N2C) |

## Usage

```haskell
import Cardano.Node.Client.N2C.Provider (mkN2CProvider)

let provider = mkN2CProvider lsqChannel

utxos <- queryUTxOs provider myAddress
pp    <- queryProtocolParams provider
```
