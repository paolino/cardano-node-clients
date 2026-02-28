module Main (main) where

import Test.Hspec (hspec)

import Cardano.Node.Client.E2E.ProviderSpec qualified as ProviderSpec

main :: IO ()
main = hspec ProviderSpec.spec
