# N2C (Node-to-Client)

::: {.module}
`Cardano.Node.Client.N2C.*`
:::

Node-to-Client protocol implementation over Unix sockets.

## Connection

```haskell
runNodeClient
    :: NetworkMagic -> FilePath
    -> LSQChannel -> LTxSChannel
    -> IO (Either SomeException ())
```

Opens a multiplexed connection with two mini-protocols:

- **MiniProtocol 6** -- LocalTxSubmission
- **MiniProtocol 7** -- LocalStateQuery

Uses `NodeToClientV_20` and `CardanoNodeToClientVersion16`.

## Channels

Create channels before starting the connection:

```haskell
lsqCh  <- newLSQChannel 16   -- 16-slot query queue
ltxsCh <- newLTxSChannel 16  -- 16-slot submit queue
```

## LocalStateQuery

The LSQ client batches queries in a single acquired session.
Use `queryLSQ` for direct access:

```haskell
queryLSQ :: LSQChannel -> Query Block result -> IO result
```

## LocalTxSubmission

Use `submitTxN2C` for low-level submission (takes `GenTx Block`),
or prefer the high-level `mkN2CSubmitter` which wraps
`Tx ConwayEra` automatically.

```haskell
submitTxN2C :: LTxSChannel -> GenTx Block -> IO (Either (ApplyTxErr Block) ())
```
