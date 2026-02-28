default:
    just --list

format:
    find . -type f -name '*.hs' -not -path '*/dist-newstyle/*' -exec fourmolu -i {} +
    cabal-fmt -i cardano-node-clients.cabal

hlint:
    find . -type f -name '*.hs' -not -path '*/dist-newstyle/*' -exec hlint {} +

build:
    cabal build all -O0

e2e:
    cabal test e2e-tests -O0 --test-show-details=direct

ci:
    just build
    just e2e
    cabal-fmt -c cardano-node-clients.cabal
    find . -type f -name '*.hs' -not -path '*/dist-newstyle/*' -exec fourmolu -m check {} +
    find . -type f -name '*.hs' -not -path '*/dist-newstyle/*' -exec hlint {} +

serve-docs:
    mkdocs serve
