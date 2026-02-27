# Architecture

## Design

The library separates **protocol-agnostic interfaces** from
**protocol-specific implementations**.

```
Cardano.Node.Client
├── Types          -- Block, BlockPoint aliases
├── Provider       -- query interface (record-of-functions)
├── Submitter      -- submit interface (record-of-functions)
├── Balance        -- transaction balancing
└── N2C
    ├── Types              -- LSQChannel, LTxSChannel
    ├── Codecs             -- N2C codec config
    ├── Connection         -- multiplexed Unix socket
    ├── LocalStateQuery    -- LSQ protocol client
    ├── LocalTxSubmission  -- LTxS protocol client
    ├── Provider           -- N2C-backed Provider
    └── Submitter          -- N2C-backed Submitter
```

## Channel-driven protocol clients

Each mini-protocol client is driven by an STM channel (`TBQueue`).
Callers enqueue a request and block on a `TMVar` for the result.
This decouples request submission from the Ouroboros protocol loop.

```
                  ┌───────────────┐
  caller ──req──► │   TBQueue     │ ──► protocol client ──► node
  caller ◄─res──  │   TMVar       │ ◄── protocol client ◄── node
                  └───────────────┘
```

The **LocalStateQuery** client batches queries: it waits for the
first query, acquires the volatile tip, drains the queue in a
single session, then releases and loops.

## N2C connection

`runNodeClient` opens a Unix socket to the Cardano node and
multiplexes two mini-protocols:

- **MiniProtocol 6** -- LocalTxSubmission
- **MiniProtocol 7** -- LocalStateQuery

The connection blocks until closed. Run it in a background thread
with `async`.

## Transaction balancing

`balanceTx` iteratively estimates fees using `estimateMinFeeTx`,
adding fee-paying inputs and a change output. It converges in at
most 10 rounds. Only ADA-only inputs are supported; multi-asset
coin selection is out of scope.
