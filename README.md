# cardano-node-clients

Channel-driven Haskell clients for Cardano node Ouroboros mini-protocols (N2C + N2N).

**[Documentation](https://paolino.github.io/cardano-node-clients/)**

## Features

- **Provider** -- query UTxOs and protocol parameters
- **Submitter** -- submit signed transactions
- **Balance** -- iterative fee estimation and transaction balancing
- **N2C** -- LocalStateQuery + LocalTxSubmission over Unix socket

## Build

```bash
nix develop -c just build
nix develop -c just ci       # format + lint + build
```

## License

[Apache-2.0](LICENSE)
