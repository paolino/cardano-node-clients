{- |
Module      : Cardano.Node.Client.Types
Description : Block type aliases
License     : Apache-2.0

Shared block type aliases for Cardano node clients.
-}
module Cardano.Node.Client.Types (
    -- * Block types
    Block,
    BlockPoint,
) where

import Ouroboros.Consensus.Cardano.Block qualified as Consensus
import Ouroboros.Network.Block qualified as Network

-- | Cardano block type.
type Block =
    Consensus.CardanoBlock
        Consensus.StandardCrypto

-- | Point for the Cardano block.
type BlockPoint = Network.Point Block
