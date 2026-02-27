{- |
Module      : Cardano.Node.Client.Provider
Description : Protocol-agnostic query interface
License     : Apache-2.0

Record-of-functions interface for querying the Cardano
blockchain. Protocol-specific implementations provide
constructors (e.g. 'Cardano.Node.Client.N2C.Provider.mkN2CProvider').
-}
module Cardano.Node.Client.Provider (
    -- * Provider interface
    Provider (..),
) where

import Cardano.Ledger.Address (Addr)
import Cardano.Ledger.Api.Tx.Out (TxOut)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (PParams)
import Cardano.Ledger.TxIn (TxIn)

{- | Interface for querying the blockchain.
All era-specific types are fixed to 'ConwayEra'.
-}
data Provider m = Provider
    { queryUTxOs ::
        Addr ->
        m [(TxIn, TxOut ConwayEra)]
    -- ^ Look up UTxOs at an address
    , queryProtocolParams ::
        m (PParams ConwayEra)
    -- ^ Fetch current protocol parameters
    }
