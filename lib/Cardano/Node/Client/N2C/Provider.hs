{- |
Module      : Cardano.Node.Client.N2C.Provider
Description : N2C-backed Provider via LocalStateQuery
License     : Apache-2.0

Production implementation of the 'Provider'
interface. Queries a local Cardano node via the
N2C LocalStateQuery mini-protocol using an
'LSQChannel' (see "Cardano.Node.Client.N2C.Connection").

Protocol parameters and UTxOs are retrieved with
Conway-era queries. An era mismatch (node not yet
in Conway) results in a runtime error.
-}
module Cardano.Node.Client.N2C.Provider (
    -- * Construction
    mkN2CProvider,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Cardano.Ledger.State (UTxO (..))

import Ouroboros.Consensus.Cardano.Block (
    pattern QueryIfCurrentConway,
 )
import Ouroboros.Consensus.Ledger.Query (
    Query (BlockQuery),
 )
import Ouroboros.Consensus.Shelley.Ledger.Query (
    pattern GetCurrentPParams,
    pattern GetUTxOByAddress,
 )

import Cardano.Node.Client.N2C.LocalStateQuery (
    queryLSQ,
 )
import Cardano.Node.Client.N2C.Types (LSQChannel)
import Cardano.Node.Client.Provider (Provider (..))

{- | Create a 'Provider IO' backed by the N2C
LocalStateQuery protocol.
-}
mkN2CProvider ::
    -- | LocalStateQuery channel to the Cardano node
    LSQChannel ->
    Provider IO
mkN2CProvider ch =
    Provider
        { queryProtocolParams = do
            result <-
                queryLSQ ch $
                    BlockQuery $
                        QueryIfCurrentConway
                            GetCurrentPParams
            case result of
                Right pp -> pure pp
                Left _mismatch ->
                    error
                        "queryProtocolParams: era \
                        \mismatch — node not in Conway"
        , queryUTxOs = \addr -> do
            result <-
                queryLSQ ch $
                    BlockQuery $
                        QueryIfCurrentConway $
                            GetUTxOByAddress
                                (Set.singleton addr)
            case result of
                Right utxo ->
                    pure $
                        Map.toList $
                            unUTxO utxo
                Left _mismatch ->
                    error
                        "queryUTxOs: era mismatch \
                        \— node not in Conway"
        }
