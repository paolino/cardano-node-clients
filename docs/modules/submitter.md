# Submitter

::: {.module}
`Cardano.Node.Client.Submitter`
:::

Protocol-agnostic interface for submitting signed transactions.

```haskell
data SubmitResult
    = Submitted TxId
    | Rejected ByteString

newtype Submitter m = Submitter
    { submitTx :: Tx ConwayEra -> m SubmitResult
    }
```

## Constructors

| Function | Module | Transport |
|----------|--------|-----------|
| `mkN2CSubmitter` | `Cardano.Node.Client.N2C.Submitter` | Unix socket (N2C) |

## Usage

```haskell
import Cardano.Node.Client.N2C.Submitter (mkN2CSubmitter)

let submitter = mkN2CSubmitter ltxsChannel

result <- submitTx submitter signedTx
case result of
    Submitted txId -> putStrLn $ "OK: " <> show txId
    Rejected reason -> putStrLn $ "FAIL: " <> show reason
```
