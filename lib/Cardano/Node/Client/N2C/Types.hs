{-# LANGUAGE GADTs #-}

{- |
Module      : Cardano.Node.Client.N2C.Types
Description : N2C channel and request types
License     : Apache-2.0

Channel types and request wrappers for communicating
with the Cardano node via the node-to-client
Ouroboros mini-protocols: LocalStateQuery (UTxO and
protocol-parameter queries) and LocalTxSubmission
(signed transaction submission).

Communication is channel-based: callers enqueue
requests into a 'TBQueue' and block on a 'TMVar'
for the result.
-}
module Cardano.Node.Client.N2C.Types (
    -- * Channel types
    LSQChannel (..),
    LTxSChannel (..),

    -- * Request wrappers
    SomeLSQQuery (..),
    TxSubmitRequest (..),
) where

import Control.Concurrent.STM (TBQueue, TMVar)

import Cardano.Node.Client.Types (Block)
import Ouroboros.Consensus.Ledger.Query (Query)
import Ouroboros.Consensus.Ledger.SupportsMempool (
    ApplyTxErr,
    GenTx,
 )

{- | Existential wrapper for a query with its result
slot, so the protocol loop can serve arbitrary
queries without knowing the result type.
-}
data SomeLSQQuery where
    SomeLSQQuery ::
        Query Block result ->
        TMVar result ->
        SomeLSQQuery

{- | Channel for communicating with the
LocalStateQuery mini-protocol client.

Callers enqueue a 'SomeLSQQuery' and then block
on the embedded 'TMVar' to receive the result.
-}
newtype LSQChannel = LSQChannel
    { lsqRequests :: TBQueue SomeLSQQuery
    }

{- | A transaction submission request bundled with
its response slot.
-}
data TxSubmitRequest = TxSubmitRequest
    { tsrTx :: !(GenTx Block)
    -- ^ The transaction to submit
    , tsrResult ::
        !(TMVar (Either (ApplyTxErr Block) ()))
    -- ^ Where to put the submission result
    }

{- | Channel for communicating with the
LocalTxSubmission mini-protocol client.
-}
newtype LTxSChannel = LTxSChannel
    { ltxsRequests :: TBQueue TxSubmitRequest
    }
